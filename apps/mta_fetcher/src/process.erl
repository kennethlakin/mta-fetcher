-module(process).
-define(MNESIA_TABLE, workqueue).
-define(MODEL_SERVER_NAME, "Mta-Fetcher-Model-Server").
-define(PROCESS_SUPERVISOR_NAME, process_sup).
-define(FETCHER_MODULE_NAMES, [prediction, vehicle]).
-define(TABLE_COPY_MGR_NAME, table_copy_mgr).
-define(TABLE_COPY_TRANSITION_SIZE, 200).
-compile([{parse_transform, lager_transform}]).
-behaviour(supervisor).

%Supervisor exports
-export([init/1]).
%External API exports
-export([diffLists/2, findDeleted/2, findAdded/2, findUnchanged/2, packageNewRouteList/1
        ,getSupervisorName/0, getFetcherModuleNames/0, getExpectedNumberOfRoutes/0
        ,determineErrorReason/1, getChildName/2, waitForTables/0]).
%Internal child start API
-export([startFetchers/0, startTableCopyMgr/0]).
%Internal housekeeping API
-export([sitAndSpin/0, tableCopyMgrLoop/2]).
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

tableIsTooLarge({P, V}) ->
  P >= ?TABLE_COPY_TRANSITION_SIZE
  orelse V >= ?TABLE_COPY_TRANSITION_SIZE.

tableTransformOperation(LastFive, PVTuple) ->
  case length(LastFive) >= 4 of
    %We don't have enough info, so do nothing.
    false -> {lists:append([PVTuple], LastFive), none};
    true ->
      {NewFive, _}=lists:split(5, lists:append([PVTuple], LastFive)),
      {_, SmallEnough}=lists:partition(fun tableIsTooLarge/1, NewFive),
      case length(SmallEnough) == 5 of
        %If the last five size samples are small enough, convert to disc_copies.
        true -> Operation=disc_copies;
        %Else, if the last *two* sizes have been too large,
        %then convert to disc_only_copies.
        false ->
          {LastTwo, _}=lists:split(2, NewFive),
          TooLarge=lists:foldl(fun(PV, true) -> tableIsTooLarge(PV);
                                  (_, false) -> false
                               end, true, LastTwo),
          case TooLarge of
            true -> Operation=disc_only_copies;
            false -> Operation=disc_copies
          end
      end,
      {NewFive, Operation}
  end.

maybeTransformTable(CopyType) ->
  case CopyType of
    none -> noconvert;
    _ ->
      PNodes=mnesia:table_info(prediction, CopyType),
      VNodes=mnesia:table_info(vehicle, CopyType),
      Node=node(),
      case lists:member(Node, PNodes) andalso
           lists:member(Node, VNodes) of
        true ->
          lager:debug("TableCopyMgr: Already converted to type ~p", [CopyType]),
          noconvert;
        false ->
          lager:debug("TableCopyMgr: Converting to type ~p", [CopyType]),
          {atomic, ok}=mnesia:change_table_copy_type(prediction, node(), CopyType),
          {atomic, ok}=mnesia:change_table_copy_type(vehicle, node(), CopyType),
          converted
      end
  end.

tableCopyMgrLoop(LastFive, UpdateTime) ->
  timer:sleep(timer:seconds(1)),
  PredSize=mnesia:table_info(prediction, size),
  VehicSize=mnesia:table_info(vehicle, size),
  {NewFive, Operation}=tableTransformOperation(LastFive, {PredSize, VehicSize}),
  case erlang:system_time(milli_seconds) >= UpdateTime of
    false -> NewUpdateTime=UpdateTime;
    true ->
      case maybeTransformTable(Operation) of
        noconvert -> Delay=0;
        converted ->
          case Operation of
            disc_only_copies -> Delay=timer:minutes(5);
            disc_copies -> Delay=timer:seconds(20)
          end
      end,
      NewUpdateTime=erlang:system_time(milli_seconds)+Delay
  end,
  tableCopyMgrCheckMsgs(),
  ?MODULE:tableCopyMgrLoop(NewFive, NewUpdateTime).

tableCopyMgrCheckMsgs() ->
  receive Unexpected ->
            lager:warning("TableCopyMgr: Unexpected message ~p", [Unexpected]),
            tableCopyMgrCheckMsgs()
  after 0 -> ok
  end.

startTableCopyMgr() ->
  Pid=spawn_link(?MODULE, tableCopyMgrLoop, [[], 0]),
  true=register(?TABLE_COPY_MGR_NAME, Pid),
  {ok, Pid}.

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
  %Start our table copy manager
  superviseTableCopyManager(),
  %Start our RouteList supervisor and fetcher:
  {ok, _}=supervisor:start_link({local, routeList:getSupervisorName()}, routeList, []),
  sitAndSpin().

superviseModelServer() ->
  {ok, _}=supervisor:start_child(getSupervisorName(),
                             #{id=>?MODEL_SERVER_NAME
                               ,start=>{model_srv, start_link, []}
                               ,restart=>permanent}).

superviseTableCopyManager() ->
  {ok, _}=supervisor:start_child(getSupervisorName(),
                             #{id=>?TABLE_COPY_MGR_NAME
                               ,start=>{process, startTableCopyMgr, []}
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
