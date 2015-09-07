-module(routeConfig).
-include ("routeconfig-response.hrl").

%Fetches:
% * The list of stops for a given line
% * Additional information about a given line.

-behaviour(supervisor).
%Supervisor exports
-export([init/1]).
%External API
-export([getSupervisorName/0, doUpdate/1, restartFetchers/1, stopFetchers/1, startFetchers/1, purgeFetchers/1]).
%Housekeeping API
-export([createTable/0]).

-define(SUPERVISOR_NAME, mta_fetcher_routeConfig_sup).
%FIXME: Maybe put this in the config file?
-define(FETCH_PERIOD, timer:minutes(15)).

-compile([{parse_transform, lager_transform}]).
-record(routeConfig, {route :: erlang:string()
                      %data is the transformed erlsom routeConfig record.
                       ,data :: erlang:map()
                       %lastFetch is the last time we fetched info.
                       ,lastFetch :: erlang:integer()
                       %lastChange is the last time we got info
                       %that differed from what was stored.
                       ,lastChange :: erlang:integer()
                     }).

-define(JITTER_TIME, random:uniform(200)).

getSupervisorName() ->
  ?SUPERVISOR_NAME.

findStoredFirst() ->
  mnesia:sync_dirty(fun() -> 
                        Key=mnesia:first(routeConfig),
                        case Key of
                          '$end_of_table' ->
                            {error, empty_table};
                          _ ->
                            {ok, mnesia:read(routeConfig, Key)}
                        end
                    end).

%Triggers an update of RC data for all 
%available Routes if we have no stored records.
readStoredFirst() ->
  case findStoredFirst() of
    {error, empty_table} ->
      lager:debug("Empty RouteConfig table. Fetching."),
      doUpdate(routeList:getRoutes()),
      readStoredFirst();
    {ok, R} when length(R) == 1 ->
      Result=lists:nth(1, R),
      {ok, #{data => Result#routeConfig.data
             ,lastFetch => Result#routeConfig.lastFetch
             ,lastChange => Result#routeConfig.lastChange}}
  end.

findStoredConfig(Route) ->
  R=mnesia:sync_dirty(fun() -> mnesia:read(routeConfig, Route) end),
  case R of
    [] -> {error, not_found};
    _ when length(R) == 1 ->
      Result=lists:nth(1, R),
      {ok, #{data => Result#routeConfig.data
             ,lastFetch => Result#routeConfig.lastFetch
             ,lastChange => Result#routeConfig.lastChange}}
  end.

readStoredConfig(Route) ->
  readStoredFirst(),
  findStoredConfig(Route).

processRecord(Type, Tag, Config, Acc, LastFetch, LastChange) when is_atom(Type) ->
  addRecord(#routeConfig{route=Tag, data=Config, lastFetch=LastFetch, lastChange=LastChange}),
  PrevList=maps:get(Type, Acc),
  maps:update(Type, lists:append(PrevList, [maps:get(tag, Config)]), Acc).

doUpdate(ChangedRoutes) when is_map(ChangedRoutes) ->
  Added=routeList:getRoutes(maps:get(added, ChangedRoutes)),
  Deleted=routeList:getRoutes(maps:get(deleted, ChangedRoutes)),
  Unchanged=routeList:getRoutes(maps:get(unchanged, ChangedRoutes)),
  %Kill our fetchers for deleted routes immediately
  %FIXME: Mark in our Postgres DB that this route is no longer valid.
  routeConfig:purgeFetchers(Deleted),
  UpdateRet=routeConfig:doUpdate(lists:umerge(lists:sort(Added), lists:sort(Unchanged))),
  %FIXME: Mark in our Postgres DB that these have been added or updated.
  startFetchers(maps:get(added, UpdateRet)),
  restartFetchers(maps:get(modified, UpdateRet));

doUpdate(Routes) when is_list(Routes) ->
  RouteConfigs=lists:foldl(fun(Route, A) ->
                               RC=update(Route),
                               lists:append(A, [createMap(RC#body.route)])
                           end, [], Routes),
  Now=erlang:system_time(milli_seconds),
  lists:foldl(fun(Config, Acc) ->
                  Tag=maps:get(tag, Config),
                  MaybeOldConfig=findStoredConfig(Tag),
                  case MaybeOldConfig of
                    {error, not_found} ->
                      processRecord(added, Tag, Config, Acc, Now, Now);
                    {ok, OldConfig} ->
                      Data=maps:get(data, OldConfig),
                      case Data /= Config of
                        true ->
                          processRecord(modified, Tag, Config, Acc, Now, Now);
                        false ->
                          LastChange=maps:get(lastChange, OldConfig),
                          processRecord(unchanged, Tag, Config, Acc, Now, LastChange)
                      end
                  end
              end,
              #{added=>[], modified=>[], unchanged=>[]},
              RouteConfigs).

init([]) ->
  routeConfig:startFetchers(routeList:getRoutes()),
  {ok, 
   {#{strategy=>one_for_one
         ,intensity=>1
         ,frequency=>1},
   []}}.

update(R) ->
  update(R, fetch:getProfiles()).

update(_R, []) ->
  %FIXME: If we get here, but we haven't loaded our Route Configs
  %       we need to recover somehow.
  lager:warning("RouteConfig: Ran out of alternatives."),
  error;

update(R, Profiles) ->
  Route=lists:concat(["&r=", R, "&terse"]),
  case fetch:request("routeConfig", "sf-muni", Profiles, Route) of
    {error, no_more_alternatives} ->
      lager:warning("RouteConfig: Ran out of alternatives."),
      error;
    {ok, {Status, Body, Profile, RestOfProfiles}} ->
      {ok, Model} = model_srv:getModel(?MODULE),
      {ok, Out, _} = erlsom:scan(Body, Model),
      case Out#body.'Error' /= undefined of
        true ->
          Err=Out#body.'Error',
          ShouldRetry=Err#'routeconfig-error'.shouldRetry,
          Text=Err#'routeconfig-error'.'#text',
          Reason=process:determineErrorReason(Text),
          case Reason of
            unknown_reason ->
              Report=#{status=>Status, profile=>element(1, Profile), shouldRetry=>ShouldRetry, reason=>Reason, text=>Text},
              lager:error("RC Found error: ~p", [Report]);
            too_much_data_requested ->
              Report=#{status=>Status, profile=>element(1, Profile), shouldRetry=>ShouldRetry, reason=>Reason},
              lager:debug("RC Found error: ~p", [Report]);
            _ ->
              Report=#{status=>Status, profile=>element(1, Profile), shouldRetry=>ShouldRetry, reason=>Reason},
              lager:error("RC Found error: ~p", [Report])
          end,
          update(R, RestOfProfiles);
        false ->
          Out
      end
  end.

restartFetchers(Routes) ->
  stopFetchers(Routes),
  startFetchers(Routes).

%FIXME: It's strange for us to be fiddling with processes attached to
%       the process supervisor, but we can't start_child on a super that
%       isn't yet started, and we want to call startFetchers from init/1.
stopFetchers(Routes) ->
  ServerName=process:getSupervisorName(),
  lists:foreach(fun(Route) ->
                    lists:foreach(fun(Mod) ->
                                      ChildName=process:getChildName(Mod, Route),
                                      ok=supervisor:terminate_child(ServerName, ChildName),
                                      ok=supervisor:delete_child(ServerName, ChildName)
                                  end,
                                  process:getFetcherModuleNames())
                end,
                Routes).

startFetchers(Routes) ->
  lists:foreach(fun(Module) ->
                    ChildSpecs=lists:foldl(fun(Route, A) ->
                                               %Don't get stops if we don't need them.
                                               case Module of
                                                 prediction -> {ok, Stops} = getStopsForRoute(Route);
                                                 vehicle -> Stops = []
                                               end,
                                               lists:append(A, [#{id=>process:getChildName(Module, Route)
                                                                  ,start=>{Module, startRunner, [Route, Stops]}
                                                                  ,restart=>permanent}])
                                           end, [], Routes),
                    lists:foreach(fun(ChildSpec) ->
                                      {ok, _} = supervisor:start_child(process:getSupervisorName(), ChildSpec),
                                      timer:sleep(random:uniform(20))
                                  end, ChildSpecs)
                end,
                process:getFetcherModuleNames()).

purgeFetchers(Routes) ->
  stopFetchers(Routes),
  {atomic, _}=mnesia:transaction(fun() ->
                                     lists:foreach(fun(Route) ->
                                                       mnesia:delete(routeConfig, Route, write)
                                                   end,
                                                   Routes)
                                 end).


routeConfig(R) ->
  case readStoredConfig(R) of
    {ok, StoredConfig} ->
      {ok, maps:get(data, StoredConfig)};
    {error, not_found} ->
      {error, not_found}
  end.

getStopsForRoute(R) ->
  case routeConfig(R) of
    {ok, Data} ->
      {ok, lists:map(fun(L) -> maps:get(tag, L) end, maps:get(stops, Data))};
    {error, not_found} ->
      Ret=doUpdate([R]),
      %Ensure that something was actually added.
      case length(maps:get(added, Ret)) of
        %If nothing was added, then we can't find stops for this route.
        0 -> {error, data_not_found};
        %We expect that exactly one record was added.
        1 -> getStopsForRoute(R)
      end
  end.

createTable() ->
  mnesia:create_table(routeConfig, [{attributes, record_info(fields, routeConfig)},
                                  {type, set},
                                  {disc_copies, [node()]}]).

addRecord(Rec) ->
  Txn= fun () ->
       mnesia:write(Rec)
       end,
  {atomic, _}=mnesia:transaction(Txn).

extractRouteStops(undefined) -> [];
extractRouteStops(Stops) ->
  lists:foldl(fun(Stop, A) ->
                  Digest=[#{tag => Stop#'routeconfig-stop'.tag
                            ,title => Stop#'routeconfig-stop'.title
                            ,shortTitle => Stop#'routeconfig-stop'.shortTitle
                            ,lat => Stop#'routeconfig-stop'.lat
                            ,lon => Stop#'routeconfig-stop'.lon
                            ,stopId => Stop#'routeconfig-stop'.stopId
                           }],
                  lists:append(A, Digest)
              end,
              [], Stops).

extractRouteDirections(undefined) -> [];
extractRouteDirections(Directions) ->
  lists:foldl(fun(Dir, A) ->
                  Digest=[#{tag => Dir#'routeconfig-direction'.tag
                            ,title => Dir#'routeconfig-direction'.title
                            ,useForUI => Dir#'routeconfig-direction'.useForUI
                            ,stop => extractDirectionStops(Dir#'routeconfig-direction'.stop)
                         }],
                  lists:append(A, Digest)
              end,
              [], Directions).

extractDirectionStops(undefined) -> [];
extractDirectionStops(Stops) ->
  lists:foldl(fun(Stop, A) ->
                  Digest=[#{tag => Stop#'routeconfig-direction/stop'.tag}],
                  lists:append(A, Digest)
              end,
              [], Stops).

extractRoutePaths(undefined) -> [];
extractRoutePaths(Paths) ->
  lists:foldl(fun(Path, A) ->
                  Digest=[#{tag => Path#'routeconfig-path'.tag
                            ,title => Path#'routeconfig-path'.title
                            ,color => Path#'routeconfig-path'.color
                            ,oppositeColor => Path#'routeconfig-path'.oppositeColor
                            ,latMin => Path#'routeconfig-path'.latMin
                            ,latMax => Path#'routeconfig-path'.latMax
                            ,lonMin => Path#'routeconfig-path'.lonMin
                            ,lonMax => Path#'routeconfig-path'.lonMax
                            ,point=> extractPathPoints(Path#'routeconfig-path'.point)
                           }],
                  lists:append(A, Digest)
              end,
              [], Paths).

extractPathPoints(undefined) -> [];
extractPathPoints(Points) ->
  lists:foldl(fun(Point, A) ->
                  Digest=[#{lat => Point#'routeconfig-point'.lat
                            ,lon => Point#'routeconfig-point'.lon
                  }],
                  lists:append(A, Digest)
              end,
              [], Points).

%Takes a record from erlsom and converts it into a map.
createMap(RouteConfig) ->
  #{type => routeConfig
    , tag => RouteConfig#'routeconfig-route'.tag
    , title => RouteConfig#'routeconfig-route'.title
    , color => RouteConfig#'routeconfig-route'.color
    , oppositeColor => RouteConfig#'routeconfig-route'.oppositeColor
    , latMin => RouteConfig#'routeconfig-route'.latMin
    , latMax => RouteConfig#'routeconfig-route'.latMax
    , lonMin => RouteConfig#'routeconfig-route'.lonMin
    , lonMax => RouteConfig#'routeconfig-route'.lonMax
    , stops => extractRouteStops(RouteConfig#'routeconfig-route'.stop)
    , direction => extractRouteDirections(RouteConfig#'routeconfig-route'.direction)
    , path => extractRoutePaths(RouteConfig#'routeconfig-route'.path)
   }.
