-module(mta_fetcher).
-behaviour(application).
-behaviour(supervisor).

%To make lager work:
-compile([{parse_transform, lager_transform}]).
% Application callbacks
-export([start/2, stop/1]).
% So that we can be our own supervisor:
-export([init/1]).
%To start and link the children:
-export([startFetcherMonitor/0]).

%Convenience functions:
-export([start/0, stop/0]).

start() ->
  {ok, _}=application:ensure_all_started(mta_fetcher).

stop() ->
  application:stop(mta_fetcher).

start(StartType, _StartArgs) ->
  case StartType of 
    normal -> ok;
    _ -> lager:warning("Recieved start type of ~p", [StartType])
  end,
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
  ok.

startFetcherMonitor() ->
  ChildPid = spawn_link(process, startFetchers, []),
  {ok, ChildPid}.
  
init([]) ->
  NumDeaths=5,
  InNumSecs=10,
  {ok, 
   { {one_for_one, NumDeaths, InNumSecs}, [
                                           #{id=>mta_fetcher_fetcher_monitor, 
                                             start=>{mta_fetcher, startFetcherMonitor, []},
                                             restart=>permanent}
                                          ]}
  }.

