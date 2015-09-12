-module(prediction).
-include ("prediction-response.hrl").

%Fetches
% * Bus arrival predictions for a given line and stop or stops.

%External API
-export([addRecord/1, startRunner/2, linked_runner/2]).
%Housekeeping API
-export([createTable/0, repeatMoveData/0]).
%Debugging API
-export([reportPredFetchHang/2]).

-record(predictionQueryRecord, {queryTime :: erlang:timestamp(),
                       body :: body()}).

-compile([{parse_transform, lager_transform}]).

-define(MAX_ROUTE_AND_STOPS, 100).
-define(JITTER_TIME, random:uniform(200)).
-define(SLEEPTIME, timer:seconds(20)+?JITTER_TIME).
-define(MNESIA_TABLE, prediction).

startRunner(Route, Stops) ->
  Pid=spawn_link(?MODULE, linked_runner, [Route, Stops]),
  true=register(process:getChildName(?MODULE, Route), Pid),
  {ok, Pid}.

reportPredFetchHang(Route, Pid) ->
  lager:error("Prediction fetch hang for route=~p. Backtrace=~p", [Route, process_info(Pid, backtrace)]),
  exit(Pid, prediction_fetch_hung).

linked_runner(Route, Stops) ->
  {ok, FetchHangTimer}=timer:apply_after(?SLEEPTIME, ?MODULE, reportPredFetchHang, [Route, self()]),
  Prediction=
    try 
      Pred=runQuery(Route, Stops),
      {ok, cancel}=timer:cancel(FetchHangTimer),
      Pred
    catch E={error, {http_error, Code}} ->
      {ok, cancel}=timer:cancel(FetchHangTimer),
          case Code of
            500 -> ok;
            503 -> ok
          end,
      sleepAndExitAfterHTTPServerError(E)
    end,
  spawn(prediction, addRecord, [Prediction]),
  timer:sleep(?SLEEPTIME),
  checkMessages(),
  ?MODULE:linked_runner(Route, Stops).

checkMessages() ->
  receive
    _Msg ->
      lager:warning("prediction:linked_runner. Got unexpected msg ~p", [_Msg]),
      checkMessages()
  after 0 -> ok
  end.

sleepAndExitAfterHTTPServerError(ErrorCode) ->
  timer:sleep(?SLEEPTIME),
  exit(ErrorCode).

runQuery(Route, Stops) ->
  runQuery(Route, Stops, fetch:getProfiles()).

runQuery(Route, Stops, Profiles) ->
  predictForRouteAndStops(Route, Stops, Profiles).

predictForRouteAndStops(R, Stops, Profiles) ->
  RouteAndStops=lists:map(fun (Stop) -> lists:concat([R, "|", Stop]) end, Stops),
  {ok, Model} = model_srv:getModel(?MODULE),
  Out=doPredictForRouteAndStops(RouteAndStops, [], Model, Profiles),
  Out.

doPredictForRouteAndStops([], Acc, _, _Profiles) ->
  Acc;

doPredictForRouteAndStops(RS, A, Model, Profiles) ->
  %predictionsForMultiStops only permits 100 stops per query.
  case length(RS) > ?MAX_ROUTE_AND_STOPS of
    true ->
      {RouteAndStops, Rest} = lists:split(?MAX_ROUTE_AND_STOPS, RS);
    false ->
      RouteAndStops = RS,
      Rest = []
  end,
  Request=lists:flatten(lists:map(fun(F) -> lists:concat(["&stops=", F]) end, RouteAndStops)),
  Acc=doFetch(Request, A, Model, Profiles),
  doPredictForRouteAndStops(Rest, Acc, Model, Profiles).

doFetch(_Request, A, _Model, []) ->
  lager:warning("Predictions Fetch error. Ran out of alternatives"),
  A;

doFetch(Request, A, Model, Profiles) ->
  case fetch:request("predictionsForMultiStops", "sf-muni", Profiles, Request) of 
    {error, no_more_alternatives} ->
      lager:warning("Predictions Fetch error. Ran out of alternatives"),
      A;
    {ok, {Status, Body, Profile, RestOfProfiles}} ->
      case erlsom:scan(Body, Model) of
        {error, Message} ->
          lager:error("XML Parse Error: ~p ~p", [Message, Body]),
          {error, Message};
        {ok, Out, _} ->
          case Out#body.'Error' /= undefined of
            true ->
              Err=Out#body.'Error',
              ShouldRetry=Err#'prediction-error'.shouldRetry,
              Text=Err#'prediction-error'.'#text',
              Reason=process:determineErrorReason(Text),
              case Reason of
                unknown_reason ->
                  Report=#{status=>Status, profile=>element(1, Profile), shouldRetry=>ShouldRetry, reason=>Reason, text=>Text},
                  lager:warning("Prediction Found error: ~p", [Report]);
                too_much_data_requested ->
                  Report=#{status=>Status, profile=>element(1, Profile), shouldRetry=>ShouldRetry, reason=>Reason},
                  lager:debug("Prediction Found error: ~p", [Report]);
                _ ->
                  Report=#{status=>Status, profile=>element(1, Profile), shouldRetry=>ShouldRetry, reason=>Reason},
                  lager:warning("Prediction Found error: ~p", [Report])
              end,
              doFetch(Request, A, Model, RestOfProfiles);
            false ->
              lists:append([[Out], A])
          end
      end
  end.

%Database stuff from here down:
createTable() ->
  mnesia:create_table(?MNESIA_TABLE, [{attributes, record_info(fields, predictionQueryRecord)},
                                  {type, bag},
                                  {disc_only_copies, [node()]}]).

addRecord(Rec) ->
  lists:foreach(fun(F) ->
                    mnesia:transaction(fun () ->
                                           mnesia:write({?MNESIA_TABLE, os:timestamp(), F})
                                       end)
                end, Rec).

%At some point in the past, we were unable to get all records on our 
%first go around from our mnesia table. This doesn't appear to have
%happened in *ages*, though.
repeatMoveData() ->
  case moveData() of
   done -> ok;
    _ ->
      lager:info("Prediction repeatMoveData: Going again."),
      repeatMoveData()
  end.

moveData() ->
  Key=database:getFirst(?MNESIA_TABLE),
  case Key == '$end_of_table' of
  true -> done;
  false ->
	  {_, _}=database:getRecord(?MNESIA_TABLE, Key),
	  {ok, C} = database:connect(),
	  {ok, MessagesExpr} = database:prepare(C, "MessagesInsert", "select messagesinsert($1, $2, $3, $4, $5)"),
	  {ok, NoPredictionExpr} = database:prepare(C, "NoPredictionInsert", "select nopredictioninsert($1, $2, $3, $4, $5, $6, $7)"),
    {ok, PredictionExpr} = database:prepare(C, "PredictionInsert", "select predictioninsert($1, $2, $3, $4, $5, $6, 
               $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19)"),
	  moveData(C, MessagesExpr, NoPredictionExpr, PredictionExpr, Key)
   end.

moveData(C, MessagesExpr, NoPredictionExpr, PredictionExpr, K) ->
  {Key, Val}=database:getRecord(?MNESIA_TABLE, K),
  QueryList=[],
  dataInsert(C, MessagesExpr, NoPredictionExpr, PredictionExpr, QueryList, Val),
  database:recordDelete(?MNESIA_TABLE, K),
  case Key == '$end_of_table' of
    true ->
      database:disconnect(C),
      done;
    false ->
      moveData(C, MessagesExpr, NoPredictionExpr, PredictionExpr, Key)
  end.

dataInsert(C, _MessagesExpr, _NoPredictionExpr, _PredictionExpr, QueryList, []) ->
  database:doInsertData(C, QueryList);

dataInsert(C, MessagesExpr, NoPredictionExpr, PredictionExpr, QueryList, [V|T]) ->
  {_, DateTime, Val} = V,
  IsErrorRecord=Val#body.'Error' /= undefined,
  case IsErrorRecord of
    true -> 
      dataInsert(C, MessagesExpr, NoPredictionExpr, PredictionExpr, QueryList, T);
    false ->
      Predictions=Val#body.predictions,
      case Predictions of
        undefined ->
          lager:warning("undefined Predictions array."),
          dataInsert(C, MessagesExpr, NoPredictionExpr, PredictionExpr, QueryList, T);
        _ ->
          QL=handlePredictions(C, Predictions, MessagesExpr, NoPredictionExpr, PredictionExpr, QueryList, DateTime),
          dataInsert(C, MessagesExpr, NoPredictionExpr, PredictionExpr, QL, T)
      end
  end.

handlePredictions(C, Predictions, MessagesExpr, NoPredictionExpr, PredictionExpr, QueryList, DateTime) ->
  lists:foldl(fun (Prediction, Acc) ->
                    %Copy out all of the common data...
                    BaseArgs=[DateTime,
                    database:nullConv(Prediction#'prediction-predictions'.agencyTitle),
                    database:nullConv(Prediction#'prediction-predictions'.routeTag),
                    database:nullConv(Prediction#'prediction-predictions'.routeTitle),
                    database:nullConv(Prediction#'prediction-predictions'.stopTitle),
                    database:nullConv(Prediction#'prediction-predictions'.stopTag)],

                    %Check to see what more work there is to do:
                    HasPredictions=
                      Prediction#'prediction-predictions'.dirTitleBecauseNoPredictions==undefined
                      andalso
                        Prediction#'prediction-predictions'.direction /= undefined,
                    AccAfterPredictions= case HasPredictions of
                      %We have predictions. Great!
                      true ->
                        DirectionArr=Prediction#'prediction-predictions'.direction,
                        lists:foldl(fun (Direction, DAcc) ->
                                        Title=Direction#'prediction-direction'.title,
                                        DirectionPredictions=Direction#'prediction-direction'.prediction,
                                        lists:foldl(fun (DirectionPrediction, DPAcc) ->
                                                        {EpochTime, _} = string:to_integer(DirectionPrediction#'prediction-direction/prediction'.epochTime),
                                                        lists:append(DPAcc, [{PredictionExpr, lists:append(BaseArgs, 
                                                          [Title,
                                                          database:nullConv(DirectionPrediction#'prediction-direction/prediction'.seconds),
                                                          database:nullConv(DirectionPrediction#'prediction-direction/prediction'.minutes),
                                                          EpochTime,
                                                          database:nullConv(DirectionPrediction#'prediction-direction/prediction'.isDeparture),
                                                          database:nullConv(DirectionPrediction#'prediction-direction/prediction'.affectedByLayover),
                                                          database:nullConv(DirectionPrediction#'prediction-direction/prediction'.dirTag),
                                                          database:nullConv(DirectionPrediction#'prediction-direction/prediction'.slowness),
                                                          database:nullConv(DirectionPrediction#'prediction-direction/prediction'.vehicle),
                                                          database:nullConv(DirectionPrediction#'prediction-direction/prediction'.block),
                                                          database:nullConv(DirectionPrediction#'prediction-direction/prediction'.tripTag),
                                                          database:nullConv(DirectionPrediction#'prediction-direction/prediction'.delayed),
                                                          database:nullConv(DirectionPrediction#'prediction-direction/prediction'.vehiclesInConsist)])}])
                                                    end,
                                                    DAcc, DirectionPredictions)
                                    end,
                                    Acc, DirectionArr);
                      %We don't have predictions. That's also cool!
                      false ->
                        NoPredictionDirTitle=Prediction#'prediction-predictions'.dirTitleBecauseNoPredictions,
                        lists:append(Acc, [{NoPredictionExpr, lists:append(BaseArgs, [NoPredictionDirTitle])}])
                       end,

                    %Handle Messages, if any.
                    Messages=Prediction#'prediction-predictions'.message,
                    AccAfterMessages= case Messages==undefined of
                      true ->
                        lager:warning("undefined message: ~p", [Prediction]),
                        AccAfterPredictions;
                      false ->
                        lists:foldl(fun(Message, MAcc) ->
                                        lists:append(MAcc, [{MessagesExpr, 
                                                             [DateTime, 
                                        database:nullConv(Prediction#'prediction-predictions'.routeTag),
                                        database:nullConv(Prediction#'prediction-predictions'.stopTag),
                                        database:nullConv(Message#'prediction-message'.text),
                                        database:nullConv(Message#'prediction-message'.priority)]}])
                                    end,
                                    AccAfterPredictions, Messages)
                    end,
                    AccAfterMessages
                end, QueryList, Predictions).
