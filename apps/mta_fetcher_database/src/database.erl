-module(database).

-compile([{parse_transform, lager_transform}]).

-behaviour(application).
-behaviour(supervisor).

%Public API
-export([connect/0, disconnect/1]).
-export([prepare/3, startTransaction/1, commitTransaction/1, rollbackTransaction/1, nullConv/1
        ,doInsertData/2]).
-export([getFirst/1, getRecord/2, recordDelete/2]).
%Internal API
-export([predictionPusherStart/0, vehiclePusherStart/0, pusher/1, shootSelfInHead/2]).

% Application callbacks
-export([start/2, stop/1]).
% So that we can be our own supervisor:
-export([init/1]).
%Convenience methods.
-export([start/0, stop/0]).

-define(POSTGRES_CYCLE_TIME, 500). %Time in msec

start() ->
  {ok, _}=application:ensure_all_started(mta_fetcher_database).
stop() ->
  application:stop(mta_fetcher_database).

start(StartType, _StartArgs) ->
  case StartType of 
    normal ->
      ok;
    _ ->
      lager:info("Database: recieved non-normal start type of ~p", [StartType])
  end,
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_) ->
  lager:info("mta_fetcher database pusher stopped."),
  ok.

init([]) ->
  %Ensure tables are loaded.
  ok=process:waitForTables(),
  NumDeaths=5,
  InNumSecs=10,
  {ok, 
   { {one_for_one, NumDeaths, InNumSecs}, [
                                           #{id=>prediction_pusher, 
                                             start=>{database, predictionPusherStart, []},
                                             restart=>permanent}
                                           ,#{id=>vehicle_pusher, 
                                             start=>{database, vehiclePusherStart, []},
                                             restart=>permanent}
                                          ]}
  }.

predictionPusherStart() ->
  {ok, spawn_link(database, pusher, [prediction])}.

vehiclePusherStart() ->
  {ok, spawn_link(database, pusher, [vehicle])}.

pusher(Module) ->
  T1=calendar:local_time(),
  Module:repeatMoveData(),
  T2=calendar:local_time(),
  case Module of
    prediction ->
      moveTimePrint("P", calendar:time_difference(T1, T2), 15);
    vehicle ->
      moveTimePrint("V", calendar:time_difference(T1, T2), 8)
  end,
  receive 
  after ?POSTGRES_CYCLE_TIME ->
          checkMessages(),
          database:pusher(Module)
  end.

checkMessages() ->
  receive 
    Msg ->
      lager:warning("database:pusher. Got unexpected msg ~p", [Msg]),
      checkMessages()
  after 0 ->
      ok
  end.

%We expect that our thresholds will always be less than one minute.
moveTimePrint(_Prefix, {A, {B, C, D}}, Thresh) when A == 0 andalso B == 0 andalso C == 0 andalso D < Thresh+1 -> 
  ok;

moveTimePrint(Prefix, Time, _) ->
  lager:warning("~p move=~p ", [Prefix, Time]).

connect() ->
  {ok, Host}=application:get_env(mta_fetcher_database, host),
  {ok, User}=application:get_env(mta_fetcher_database, user),
  {ok, Pass}=application:get_env(mta_fetcher_database, pass),
  {ok, Db}=application:get_env(mta_fetcher_database, database),
  pgsql:connect(Host, User, Pass, [{database, Db}]).

disconnect(Conn) ->
  pgsql:close(Conn).

prepare(Conn, Name, Statement) ->
  prepare(Conn, Name, Statement, []).

%We have to use statement names, or else pgsql replaces the previous unnamed
%parsed statement with the next one. 
%ALSO, we have to pass the empty array for for the types, because 
%-contrary to what the docs say- the three-arg version of parse assumes that 
%you're passing Connection, Statement, TypeArray!!
prepare(Conn, Name, Statement, ValueTypes) ->
  pgsql:parse(Conn, Name, Statement, ValueTypes).


%Because pgsql only accepts the atom "null" for NULL values, we must change
%"undefined" into "null".
nullConv(undefined) ->
  null;
nullConv(A) ->
  A.

startTransaction(C) ->
  {ok, _, _} = pgsql:squery(C, "begin").

commitTransaction(C) ->
  {ok, _, _} = pgsql:squery(C, "commit").

rollbackTransaction(C) ->
  {ok, _, _} = pgsql:squery(C, "rollback").

%%%FIXME: This is a temporary disconnect and suicide switch that kills ourself if
%%%we've hung for too long. The root cause needs to be figured out.
shootSelfInHead(C, Pid) ->
  lager:info("Shooting self in head."),
  {ok, FailsafeTimer} = timer:exit_after(timer:seconds(10), Pid, suicide_failsafe_triggered),
  database:commitTransaction(C),
  database:disconnect(C),
  timer:cancel(FailsafeTimer),
  exit(Pid, db_wedge_triggered_suicide).
%%%End temporary disconnect and suicide switches. Make sure to remove
%%%them once we figure out what is causing our failure.
%%%Or, yanno, just after we rewrite most of the code. :/

doInsertData (C, QueryList) ->
  %When prediction insert hangs, it hangs in a call to execute_batch.
  %%%FIXME: This is a temporary disconnect and suicide switch that kills ourself if
  %%%we've hung for too long. The root cause needs to be figured out.
  {ok, SuicideSwitch}=timer:apply_after(timer:seconds(30), database, shootSelfInHead, [C, self()]),
  V=pgsql:execute_batch(C,  QueryList),
  %%%FIXME: Remove this cancel, too, once we figure out what is going on.
  {ok, cancel}=timer:cancel(SuicideSwitch),
  verifyOkay(V).

verifyOkay([]) ->
  ok;
verifyOkay([H|T]) ->
  case H of
    {ok, _ } ->
      verifyOkay(T);
    {Err, Thing} -> %FIXME: We should actually *deal with* these problems.
      lager:warning("verifyOkay: row failed with {~p, ~p}", [Err, Thing]),
      verifyOkay(T)
  end;
verifyOkay(SingleItem) ->
  lager:warning("Strange. VerifyOkay only has a single item: ~p", [SingleItem]),
  case SingleItem of
    {ok, _ } ->
      ok;
    {Err, Thing} -> %FIXME: We should actually *deal with* these problems.
      lager:warning("verifyOkay: row failed with {~p, ~p}", [Err, Thing]),
      ok
  end.


getFirst(Table) when is_atom(Table) ->
  {atomic, Key} = mnesia:transaction( fun() -> mnesia:first(Table) end),
  Key.

getRecord(Table, K) when is_atom(Table) ->
  {atomic, Val}=mnesia:transaction( fun() -> mnesia:read(Table, K) end),
  {atomic, Key}=mnesia:transaction( fun() -> mnesia:next(Table, K) end),
  {Key, Val}.

recordDelete(Table, Key) when is_atom(Table) ->
  mnesia:transaction(fun() -> mnesia:delete(Table, Key, write) end).
