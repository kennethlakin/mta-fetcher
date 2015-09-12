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
                                           #{id=>prediction_db_pusher,
                                             start=>{database, predictionPusherStart, []},
                                             restart=>permanent}
                                           ,#{id=>vehicle_db_pusher,
                                             start=>{database, vehiclePusherStart, []},
                                             restart=>permanent}
                                          ]}
  }.

predictionPusherStart() ->
  PID=spawn_link(database, pusher, [prediction]),
  true=register(prediction_db_pusher, PID),
  {ok, PID}.

vehiclePusherStart() ->
  PID=spawn_link(database, pusher, [vehicle]),
  true=register(vehicle_db_pusher, PID),
  {ok, PID}.

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
  doInsertData(C, QueryList, 0).

%1000 is an arbitrary limit.
doInsertData (C, _, 1000) ->
  lager:error("doInsertData: Recursion limit hit!"),
  database:rollbackTransaction(C),
  throw({error, doInsertData_recursion_limit_hit});

doInsertData (C, QueryList, NumRecursiveCalls) ->
  database:startTransaction(C),
  %When prediction insert hangs, it hangs in a call to execute_batch.
  %%%FIXME: This is a temporary disconnect and suicide switch that kills ourself if
  %%%we've hung for too long. The root cause needs to be figured out.
  {ok, SuicideSwitch}=timer:apply_after(timer:seconds(30), database, shootSelfInHead, [C, self()]),
  V=pgsql:execute_batch(C,  QueryList),
  %%%FIXME: Remove this cancel, too, once we figure out what is going on.
  {ok, cancel}=timer:cancel(SuicideSwitch),
  
  VLen=length(V),
  QLen=length(QueryList),
  {ResPlusQueryList, LeftoverQueries} =
  %VLen should probably never be larger than QLen, so we leave it unhandled.
  fun
    () when VLen == QLen -> {lists:zip(V, QueryList), []};
    %FIXME: Why can this happen?
    %       And is it guaranteed that the queries in the query list
    %       had been executed in order?!?
    () when VLen < QLen ->
      lager:warning("doInsertData: length(V) < length(QueryList)"),
      {QTrimmed, QRest}=lists:split(VLen, QueryList),
      {lists:zip(V, QTrimmed), QRest}
  end (),
  case verifyOkay(ResPlusQueryList) of
    failed_transaction ->
      lager:warning("doInsertData: Transaction failed. Rolling back and trying each row individually."),
      database:rollbackTransaction(C),
      lists:foreach(fun(Query) -> doInsertData(C, [Query]) end, QueryList, NumRecursiveCalls+1);
    ResultList ->
      database:commitTransaction(C),
      RL=lists:foldl(fun
                      (ok, A) -> A;
                      %Retry txns with unexpected errors, because we don't know
                      %what to do with them.
                      ({unexpected_error, Txn}, A) ->
                         lists:append(A, [Txn]);
                      ({deadlock_detected, Txn}, A) -> 
                         lager:warning("doInsertData: List contains detected deadlock that did not cause txn abort!"),
                         lists:append(A, [Txn]);
                      ({duplicate_key_value, Txn}, A) -> 
                         lager:warning("doInsertData: List contains duplicate key value error"),
                         lists:append(A, [Txn])
                  end, [], ResultList),
      RetryList=case LeftoverQueries of
                  [] -> RL;
                  _ -> 
                    lager:warning("doInsertData: Appending leftover queries to the retry list"),
                    lists:append(RL, LeftoverQueries)
                end,
      case RetryList of
        [] -> ok;
        _ -> 
          lager:warning("doInsertData: Doing insert data again because of non-empty retry list."),
          doInsertData(C, RetryList, NumRecursiveCalls+1)
      end
  end.


verifyOkay(ResultsAndStatements) ->
  lists:foldl(fun
                (_, failed_transaction) -> failed_transaction;
                ({{ok, _}, _Txn}, Acc) -> lists:append(Acc, [ok]);
                %Deadlocks *should* abort the txn, but we add this to the list 
                %(and whine later) just in case one ever *doesn't* abort the txn.
                ({{error, {error, error, <<"40P01">>, _Msg, _}}, Txn}, Acc) ->  lists:append(Acc, [{deadlock_detected, Txn}]);
                ({{error, {error, error, <<"23505">>, _Msg, _}}, Txn}, Acc) ->  lists:append(Acc, [{duplicate_key_value, Txn}]);
                ({{error, {error, error, <<"25P02">>, _Msg, _}},   _},   _) ->  failed_transaction;
                %Better to warn the operator about an overlooked error code than to die.
                ({{error, {error, error, ErrCode, Msg, _}}, Txn}, Acc) ->
                  lager:warning("verifyOkay: Unexpected error! ~p: ~p", [ErrCode, Msg]),
                  lists:append(Acc, [{unexpected_error, Txn}])
              end, [], ResultsAndStatements).

getFirst(Table) when is_atom(Table) ->
  {atomic, Key} = mnesia:transaction( fun() -> mnesia:first(Table) end),
  Key.

getRecord(Table, K) when is_atom(Table) ->
  {atomic, Val}=mnesia:transaction( fun() -> mnesia:read(Table, K) end),
  {atomic, Key}=mnesia:transaction( fun() -> mnesia:next(Table, K) end),
  {Key, Val}.

recordDelete(Table, Key) when is_atom(Table) ->
  mnesia:transaction(fun() -> mnesia:delete(Table, Key, write) end).
