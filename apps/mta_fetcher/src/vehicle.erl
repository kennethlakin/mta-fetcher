-module(vehicle).
-include ("vehicle-response.hrl").

%Fetches:
% * Vehicle position data for a given line.
%   NOTE: Vehicle position data requires a last update time. Historical
%   position data doesn't go further back than five minutes.
%   At the moment, we don't know what our last update time was, so we set the
%   last update time to 0, then use the update time that's sent back to use in
%   subsequent queries.

%External API
-export([addRecord/1, startRunner/2, linked_runner/2]).
%Housekeeping API
-export([createTable/0, repeatMoveData/0]).
%Debugging API
-export([reportVehicFetchHang/2]).

-compile([{parse_transform, lager_transform}]).

-record(vehicleQueryRecord, {queryTime :: erlang:timestamp(),
                       body :: body()}).

-define(JITTER_TIME, random:uniform(200)).
-define(SLEEPTIME, timer:seconds(10)+?JITTER_TIME).
-define(TIMEOUT, ?SLEEPTIME+2000).
-define(SOCKET_CLOSE_RETRY_WAIT, 2000+?JITTER_TIME).
-define(MNESIA_TABLE, vehicle).

reportVehicFetchHang(Route, Pid) ->
  lager:error("Vehicle fetch hang for route=~p. Backtrace=~p", [Route, process_info(Pid, backtrace)]),
  exit(Pid, vehicle_fetch_hung).

startRunner(Route, _) ->
  Pid=spawn_link(?MODULE, linked_runner, [Route, 0]),
  true=register(process:getChildName(?MODULE, Route), Pid),
  {ok, Pid}.

linked_runner(Route, LastUpdateTime) ->
  {ok, FetchHangTimer}=timer:apply_after(?SLEEPTIME, ?MODULE, reportVehicFetchHang, [Route, self()]),
  {Locations, UpdateTime}=
    try 
      Ret=runQuery(Route, LastUpdateTime),
      {ok, cancel}=timer:cancel(FetchHangTimer),
      Ret
    catch E={error, {http_error, Code}} ->
      {ok, cancel}=timer:cancel(FetchHangTimer),
          case Code of
            500 -> ok;
            503 -> ok
          end,
      sleepAndExitAfterHTTPServerError(E)
    end,
  spawn(vehicle, addRecord, [Locations]),
  timer:sleep(?SLEEPTIME),
  checkMessages(),
  ?MODULE:linked_runner(Route, UpdateTime).

checkMessages() ->
  receive
    _Msg ->
      lager:warning("vehicle:linked_runner. Got unexpected msg ~p", [_Msg]),
      checkMessages()
  after 0 ->
      ok
  end.

sleepAndExitAfterHTTPServerError(ErrorCode) ->
  timer:sleep(?SLEEPTIME),
  exit(ErrorCode).

runQuery(Route, UpdateTime) ->
  vehicleLocations(Route, UpdateTime, fetch:getProfiles()).

vehicleLocations(Route, UpdateTime, Profiles) ->
  Params=lists:concat(["&r=", Route, "&t=", UpdateTime]),
  doQuery(Params, UpdateTime, Profiles).

doQuery(Params, UT, Profiles) ->
  doQuery(Params, UT, "", Profiles).

doQuery(_Params, UT, PrevOut, []) ->
  lager:info("Vehicle fetch error. Ran out of alternatives."),
  {PrevOut, UT};

doQuery(Params, UT, PrevOut, Profiles) ->
  case fetch:request("vehicleLocations", "sf-muni", Profiles, Params) of
    {error, no_more_alternatives} ->
      lager:info("Vehicle fetch error. Ran out of alternatives."),
      {PrevOut, UT};
    {ok, {Status, Body, Profile, RestOfProfiles}} ->
      {ok, Model} = model_srv:getModel(?MODULE),
      {ok, Out, _} = erlsom:scan(Body, Model),
      case Out#body.'Error' /= undefined of
        true ->
          Err=Out#body.'Error',
          ShouldRetry=Err#'vehicle-error'.shouldRetry,
          Text=Err#'vehicle-error'.'#text',
          Reason=process:determineErrorReason(Text),
          case Reason of
            unknown_reason ->
              Report=#{status=>Status, profile=>element(1, Profile), shouldRetry=>ShouldRetry, reason=>Reason, text=>Text},
              lager:warning("Vehicle Fetch Found error: ~p", [Report]);
            too_much_data_requested ->
              Report=#{status=>Status, profile=>element(1, Profile), shouldRetry=>ShouldRetry, reason=>Reason},
              lager:debug("Vehicle Fetch Found error: ~p", [Report]);
            _ ->
              Report=#{status=>Status, profile=>element(1, Profile), shouldRetry=>ShouldRetry, reason=>Reason},
              lager:warning("Vehicle Fetch Found error: ~p", [Report])
          end,
          doQuery(Params, UT, Out, RestOfProfiles);
        false ->
          UpdateTime=Out#body.lastTime#'vehicle-time'.time,
          {Out, UpdateTime}
      end
  end.

%Database stuff from here down:
createTable() ->
  mnesia:create_table(?MNESIA_TABLE, [{attributes, record_info(fields, vehicleQueryRecord)},
                                  {type, bag},
                                  {disc_only_copies, [node()]}]).

addRecord(Rec) ->
  Txn= fun () ->
           mnesia:write({?MNESIA_TABLE, os:timestamp(), Rec})
       end,
  mnesia:transaction(Txn).

%At some point in the past, we were unable to get all records on our 
%first go around from our mnesia table. This doesn't appear to have
%happened in *ages*, though.
repeatMoveData() ->
  case moveData() of
    done ->
      ok;
    _ ->
      lager:info("Vehicle repeatMoveData: Going again."),
      repeatMoveData()
  end.

moveData() ->
  Key=database:getFirst(?MNESIA_TABLE),
  case Key == '$end_of_table' of
    true ->
    	done;
    false ->
         {_, _}=database:getRecord(?MNESIA_TABLE, Key),
         {ok, C} = database:connect(),
         {ok, VehiclePosExpr} = database:prepare(C, "VehicleLocInsert", "select vehiclelocationinsert ($1, $2, $3, $4, 
                                                 $5, $6, $7, $8, $9, $10, $11, $12)"),
         moveData(C, VehiclePosExpr, Key)
  end.

moveData(C, VehiclePosExpr, K) ->
  {Key, Val}=database:getRecord(?MNESIA_TABLE, K),
  QueryList=[],
  dataInsert(C, VehiclePosExpr, QueryList, Val),
  database:recordDelete(?MNESIA_TABLE, K),
  case Key == '$end_of_table' of
    true ->
      database:disconnect(C),
      done;
    false ->
      moveData(C, VehiclePosExpr, Key)
  end.

dataInsert(C, _VehiclePosExpr, QueryList, []) ->
  database:doInsertData(C, QueryList);

dataInsert(C, VehiclePosExpr, QueryList, [V|T]) ->
  {_, DateTime, Val} = V,
  case Val == [] of
    %FIXME: It's not yet clear why these records are empty.
    %       It seems like it's correlated with network issues.
    true ->
      lager:warning("WARN: Vehicle Record empty!"),
      dataInsert(C, VehiclePosExpr, QueryList, T);
    false ->
      IsErrorRecord=Val#body.'Error' /= undefined,
      case IsErrorRecord of
        true -> 
          dataInsert(C, VehiclePosExpr, QueryList, T);
        false ->
          VehicPosData=Val#body.vehicle,
          case VehicPosData==undefined of
            true ->
              %No position info, so don't worry about it.
              dataInsert(C, VehiclePosExpr, QueryList, T);
            false ->
              PosDataQueries=lists:foldl(fun (PosData, Acc) ->
                                             RawLastUpdateTime=Val#body.lastTime#'vehicle-time'.time,
                                             RawLat=PosData#'vehicle-vehicle'.lat,
                                             RawLon=PosData#'vehicle-vehicle'.lon,
                                             RawHeading=PosData#'vehicle-vehicle'.heading,
                                             RawSpeed=PosData#'vehicle-vehicle'.speedKmHr,
                                             LastUpdateTime=convertToNumeric(RawLastUpdateTime),
                                             Lat=convertToNumeric(RawLat),
                                             Lon=convertToNumeric(RawLon),
                                             Heading=convertToNumeric(RawHeading),
                                             Speed=convertToNumeric(RawSpeed),

                                             lists:append(Acc, [{VehiclePosExpr,
                                                                 [DateTime,
                                                                  database:nullConv(PosData#'vehicle-vehicle'.id),
                                                                  database:nullConv(PosData#'vehicle-vehicle'.routeTag),
                                                                  database:nullConv(PosData#'vehicle-vehicle'.dirTag),
                                                                  Lat,
                                                                  Lon,
                                                                  database:nullConv(PosData#'vehicle-vehicle'.secsSinceReport),
                                                                  database:nullConv(PosData#'vehicle-vehicle'.predictable),
                                                                  Heading,
                                                                  Speed,
                                                                  database:nullConv(PosData#'vehicle-vehicle'.leadingVehicleId),
                                                                  database:nullConv(LastUpdateTime)]}])
                                         end, QueryList, VehicPosData),
              dataInsert(C, VehiclePosExpr, PosDataQueries, T)
          end
      end
  end.

% "25" doesn't convert to a float for some reason, so we have this stupid
% function.
convertToNumeric(Val) ->
  case Val == undefined of
    true ->
      database:nullConv(Val);
    false ->
      TryOne=string:to_float(Val),
      case TryOne of
        {error, _} ->
          {NumTwo, _}=string:to_integer(Val),
          NumTwo;
        {NumOne, _} ->
          NumOne
      end
  end.
