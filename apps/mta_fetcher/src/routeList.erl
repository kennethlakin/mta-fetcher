-module(routeList).
-include ("routelist-response.hrl").

-define(SUPERVISOR_NAME, mta_fetcher_routeList_sup).
-define(UPDATER_NAME, mta_fetcher_routeList_updater).
-define(KEY, 0).
-define(JITTER_TIME, random:uniform(200)).
%FIXME: Maybe put this in the config file?
-define(FETCH_PERIOD, timer:minutes(15)).

%Fetches:
% * All routes served by our agency.

-compile([{parse_transform, lager_transform}]).

-behaviour(supervisor).
%supervisor exports.
-export([init/1]).

%FIXME: For debugging. Un-export later.
-export([readStoredRouteList/0, doUpdate/0]).

%External API
-export([getSupervisorName/0, getRoutes/0, getRoutes/1, routeList/0]).
%Gross parts of the External API.
-export([lastFetch/0]).
%Internal API
-export([startScheduledUpdate/0, scheduledUpdate/2]).

%Database init stuff.
-export([createTable/0]).

-record(routeList, {key :: erlang:integer()
                               %Data is the transformed erlsom routeList record.
                               ,data :: erlang:map()
                               %lastFetch is the last time we fetched info.
                               ,lastFetch :: erlang:integer()
                               %lastChange is the last time we got info
                               %that differed from what was stored.
                               ,lastChange :: erlang:integer()
                              }).

getSupervisorName() ->
  ?SUPERVISOR_NAME.

extractRoutes(undefined) ->
  [];
extractRoutes(Routes) ->
  lists:foldl(fun(Route, A) ->
                  Digest=[#{tag => Route#'routelist-route'.tag
                           ,title => Route#'routelist-route'.title
                          }],
                  lists:append(A, Digest)
              end,
              [], Routes).

getTimeLeftToUpdate() ->
  LastFetch=lastFetch(),
  timeLeftToUpdate(LastFetch).

timeLeftToUpdate(LastFetch) ->
  Now=erlang:system_time(milli_seconds),
  (LastFetch+?FETCH_PERIOD)-Now.

isTimeToUpdate(Map) when is_map(Map) ->
  TimeLeft=timeLeftToUpdate(maps:get(lastFetch, Map)),
  isTimeToUpdate(TimeLeft);
isTimeToUpdate(TimeLeft) ->
  {TimeLeft =< 0, max(TimeLeft, 0)}.

init([]) ->
  {ok, _}=doUpdate(true),
  {ok, _}=supervisor:start_link({local, routeConfig:getSupervisorName()}, routeConfig, []),
  {ok, 
   {#{strategy=>one_for_one
         ,intensity=>1
         ,frequency=>1},
   [#{id=>?UPDATER_NAME
     ,start=>{routeList, startScheduledUpdate, []}
     ,type=>worker}]
   }}.

startScheduledUpdate() ->
  TimeLeft=getTimeLeftToUpdate(),
  Now=erlang:system_time(milli_seconds),
  Pid=spawn_link(?MODULE, scheduledUpdate, [Now, TimeLeft]),
  true=register(?UPDATER_NAME, Pid),
  {ok, Pid}.

scheduledUpdate(StartTime, TimeToWaitInMs) ->
  lager:debug("scheduledUpdate: started at ~p, waiting for ~p", [StartTime, TimeToWaitInMs]),
  receive
    Unexpected ->
      lager:warning("~p:scheduledUpdate: Recieved unexpected msg: ~p", [?MODULE, Unexpected]),
      Now=erlang:system_time(milli_seconds),
      Elapsed=Now-StartTime,
      ?MODULE:scheduledUpdate(Now, max(TimeToWaitInMs-Elapsed, 0))
  after TimeToWaitInMs ->
          lager:debug("Time for scheduled RouteList update!"),
          {ok, {changed_routes, ChangedRoutes}}=doUpdate(),
          Added=maps:get(added, ChangedRoutes),
          Deleted=maps:get(deleted, ChangedRoutes),
          Unchanged=maps:get(unchanged, ChangedRoutes),
          lager:debug("Added: ~p", [Added]),
          lager:debug("Deleted: ~p", [Deleted]),
          lager:debug("Unchanged: ~p", [Unchanged]),
          %Hand the route changes off to routeConfig.
          routeConfig:doUpdate(ChangedRoutes),
          ?MODULE:scheduledUpdate(erlang:system_time(milli_seconds), ?FETCH_PERIOD)
  end.

%doUpdate/1 is called by things that aren't init/1
doUpdate() ->
  doUpdate(true).

doUpdate(ForceUpdate) ->
  {ok, Map} = readStoredRouteList(),
  Data=maps:get(data, Map),
  LastChange=maps:get(lastChange, Map),
  IsTimeToUpdate=isTimeToUpdate(Map),
  case ForceUpdate of
    true ->
      %Someone really wants an update.
      case IsTimeToUpdate of
        {true, _} -> ok;
        {false, _} -> lager:warning("~p:doUpdate called early.", [?MODULE])
      end,
      {ok, {changed_routes, update(Data, LastChange)}};
    false ->
      %If we're here, we're starting up and have been called by init/1.
      %Don't do the update if we don't need to.
      case IsTimeToUpdate of
        {true, _} -> {ok, {changed_routes, update(Data, LastChange)}};
        {false, TimeLeft} -> {ok, {time_till_update, TimeLeft}}
      end
  end.

doFetch() ->
  doFetch(fetch:getProfiles()).

doFetch([]) ->
  error;
doFetch(Profiles) ->
  case fetch:request("routeList", "sf-muni", Profiles) of
    {error, no_more_alternatives} ->
      error;
    {ok, {Status, Body, Profile, RestOfProfiles}} ->
      {ok, Model} = model_srv:getModel(?MODULE),
      {ok, Out, _} = erlsom:scan(Body, Model),
      case Out#body.'Error' /= undefined of
        true ->
          Err=Out#body.'Error',
          ShouldRetry=Err#'routelist-error'.shouldRetry,
          Text=Err#'routelist-error'.'#text',
          Reason=process:determineErrorReason(Text),
          case Reason of
            unknown_reason ->
              Report=#{status=>Status, profile=>element(1, Profile), shouldRetry=>ShouldRetry, reason=>Reason, text=>Text},
              lager:error("RL Found error: ~p", [Report]);
            too_much_data_requested ->
              Report=#{status=>Status, profile=>element(1, Profile), shouldRetry=>ShouldRetry, reason=>Reason},
              lager:debug("RL Found error: ~p", [Report]);
            _ ->
              Report=#{status=>Status, profile=>element(1, Profile), shouldRetry=>ShouldRetry, reason=>Reason},
              lager:error("RL Found error: ~p", [Report])
          end,
          doFetch(RestOfProfiles);
        false ->
          extractRoutes(Out#body.route)
      end
  end.

%update/1 is only called when there is no data to compare against, so our
%LastChange doesn't matter.
update(PrevData=[]) ->
  update(PrevData, 0).
update(PrevData, LastChange) ->
  Out=doFetch(),
  case Out of
    %FIXME: This is crude and needs more thought.
    error -> 
      throw({routeList_error, error});
    _ ->
      %Save data
      Now=erlang:system_time(milli_seconds),
      case PrevData /= Out of
        %Update last changed time if data changed.
        true ->
          lager:debug("Updating with new data."),
          addRecord(Out, Now, Now),
          process:diffLists(PrevData, Out);
        false ->
          lager:debug("Retaining old data."),
          addRecord(Out, Now, LastChange),
          %Because our data has not changed, no added or deleted
          %routes exist.
          #{added=>[], deleted=>[], unchanged=>Out}
      end
  end.

readStoredRouteList() ->
  R=mnesia:sync_dirty(fun() -> mnesia:read(routeList, ?KEY) end),
  case R of
    [] ->
      lager:debug("Empty RouteList record. Fetching."),
      update([]),
      readStoredRouteList();
    _ when length(R) == 1 ->
      Result=lists:nth(1, R),
      {ok, #{data => Result#routeList.data
             ,lastFetch => Result#routeList.lastFetch
             ,lastChange => Result#routeList.lastChange}}
  end.

lastFetch() ->
  {ok, Data}=readStoredRouteList(),
  maps:get(lastFetch, Data).

routeList() ->
  {ok, Record}=readStoredRouteList(),
  maps:get(data, Record).

getRoutes() ->
  lists:map(fun(L) -> maps:get(tag, L) end, routeList()).

%Used to get the route names from a particular set of Route records.
getRoutes(Routes) ->
  lists:map(fun(L) -> maps:get(tag, L) end, Routes).

createTable() ->
  mnesia:create_table(routeList, [{attributes, record_info(fields, routeList)},
                                  {type, set},
                                  {disc_copies, [node()]}]).

addRecord(Map, FetchTime, ChangeTime) ->
  mnesia:transaction(
    fun () ->
        mnesia:write(#routeList{key=?KEY, data=Map, lastFetch=FetchTime, lastChange=ChangeTime})
    end).
