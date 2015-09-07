-module(process).
-define(MNESIA_TABLE, workqueue).
-define(MODEL_SERVER_NAME, "Mta-Fetcher-Model-Server").
-define(PROCESS_SUPERVISOR_NAME, process_sup).
-define(FETCHER_MODULE_NAMES, [prediction, vehicle]).
-compile([{parse_transform, lager_transform}]).
-behaviour(supervisor).

%Supervisor exports
-export([init/1]).
%External API exports
-export([diffLists/2, findDeleted/2, findAdded/2, findUnchanged/2, packageNewRouteList/1
        ,getSupervisorName/0, getFetcherModuleNames/0, getExpectedNumberOfRoutes/0
        ,determineErrorReason/1, getChildName/2, waitForTables/0]).
%Fetcher start call:
-export([startFetchers/0]).
%Internal housekeeping API
-export([sitAndSpin/0]).
%Debugging API
-export([justStartXmlMonitor/0, createAllTables/0]).

findDeleted(OldList, NewList) ->
  Old=sets:from_list(OldList),
  New=sets:from_list(NewList),
  sets:to_list(findDeletedSet(Old, New)).

findAdded(OldList, NewList) ->
  Old=sets:from_list(OldList),
  New=sets:from_list(NewList),
  sets:to_list(findAddedSet(Old, New)).

findUnchanged(OldList, NewList) ->
  Old=sets:from_list(OldList),
  New=sets:from_list(NewList),
  sets:to_list(findUnchangedSet(Old, New)).

%This is used as an optimization when we're starting up and need a list of
%route for which we want to start fetchers.
packageNewRouteList(List) when is_list(List)->
  #{added => List
    ,deleted => []
    ,unchanged => []}.

diffLists(OldList, NewList) ->
  Old=sets:from_list(OldList),
  New=sets:from_list(NewList),
  Deleted=findDeletedSet(Old, New),
  Added=findAddedSet(Old, New),
  Unchanged=findUnchangedSet(Old, New),
  #{added => sets:to_list(Added)
    ,deleted => sets:to_list(Deleted)
    ,unchanged => sets:to_list(Unchanged)}.

findDeletedSet(OldSet, NewSet) ->
  sets:subtract(OldSet, NewSet).

findAddedSet(OldSet, NewSet) ->
  sets:subtract(NewSet, OldSet).

findUnchangedSet(OldSet, NewSet) ->
  sets:intersection(OldSet, NewSet).

justStartXmlMonitor() ->
  application:ensure_all_started(lager),
  lager:error("Don't use this in production. This is for debugging."),
  inets:start(),
  {ok, _}=supervisor:start_link({local, getSupervisorName()}, ?MODULE, []),
  %Load our XML models
  superviseModelServer(),
  %Start up our http client:
  ok=fetch:start(),
  %Start up our DB
  mnesia:start().

%Used to determine why we got an error from upstream's servers:
determineErrorReason(ErrStr) ->
  {ok, Reasons} = model_srv:getModel(errorReasons),
  %Once we find a single matching reason, there's no need to keep looking.
  Ret=lists:foldl(fun
                    ({Test, ReasonCode}, []) ->
                      case re:run(ErrStr, Test) of
                        match -> ReasonCode;
                        {match, _} -> ReasonCode;
                        _ -> []
                      end;
                    (_, Acc) -> Acc
                  end,
                  [], Reasons),
  case Ret of
    [] -> unknown_reason;
    _ -> Ret
  end.

%Create child process name given the module and route (if any).
getChildName(Module, Route) when is_atom(Module) ->
  list_to_atom(atom_to_list(Module) ++ "-fetch-for-" ++ Route).

getSupervisorName() ->
  ?PROCESS_SUPERVISOR_NAME.

getFetcherModuleNames() ->
 ?FETCHER_MODULE_NAMES.

%This is the expected number of routes we'll have to deal with.
%We use this in restart intensity calculations.
getExpectedNumberOfRoutes() ->
  84.

%Ensure that our Mnesia tables have been loaded.
waitForTables() ->
  case mnesia:wait_for_tables([prediction, vehicle], timer:hours(2)) of
    ok -> ok
  end.

%Supervisor's init.
init([]) ->
  %We have 2 fetchers for each route. If they all fail 5x in one second
  %something terrible has gone wrong.
  NumDeaths=getExpectedNumberOfRoutes()*2*5,
  InNumSecs=1,
  {ok, 
   { #{strategy=>one_for_one, intensity=>NumDeaths, period=>InNumSecs}, []}
  }.

%Start the position and prediction fetchers.
startFetchers() ->
  true=register(process_loop, self()),
  %Start the fetcher supervisor:
  {ok, _}=supervisor:start_link({local, getSupervisorName()}, ?MODULE, []),
  %Load our XML models
  superviseModelServer(),
  %Start up our http client:
  ok=fetch:start(),
  %Start up our DB
  ok=mnesia:start(),
  %Ensure tables are loaded.
  ok=waitForTables(),
  %Start our RouteList supervisor and fetcher:
  {ok, _}=supervisor:start_link({local, routeList:getSupervisorName()}, routeList, []),
  sitAndSpin().

superviseModelServer() ->
  {ok, _}=supervisor:start_child(getSupervisorName(),
                             #{id=>?MODEL_SERVER_NAME
                               ,start=>{model_srv, start_link, []}
                               ,restart=>permanent}).

sitAndSpin() ->
  receive
    _Unexpected ->
      lager:warning("process:sitAndSpin: Unexpected message ~p", [_Unexpected]),
      process:sitAndSpin()
  after timer:seconds(1) ->
      process:sitAndSpin()
  end.

createAllTables() ->
  routeList:createTable(),
  prediction:createTable(),
  routeConfig:createTable(),
  vehicle:createTable().
