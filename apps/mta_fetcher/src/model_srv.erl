-module(model_srv).
-compile([{parse_transform, lager_transform}]).

%Public exports
-export([getServerName/0, getModel/1, reloadModels/0]).

%gen_server exports.
-export([start_link/0, init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_server).
-define(SERVER_NAME, mta_fetcher_model_srv).

getServerName() ->
  ?SERVER_NAME.

getModel(RequestedData) ->
  gen_server:call(?MODULE:getServerName(), RequestedData).

reloadModels() ->
  gen_server:cast(?MODULE:getServerName(), load_models).

loadModels() ->
  lager:debug("module_srv (re)loading models", []),
  {ok, Prediction} = erlsom:compile_xsd_file("xsd/prediction-response.xsd", [{type_prefix, "prediction-"}]),
  {ok, Vehicle} = erlsom:compile_xsd_file("xsd/vehicle-response.xsd", [{type_prefix,"vehicle-"}]),
  {ok, RouteList} = erlsom:compile_xsd_file("xsd/routelist-response.xsd", [{type_prefix, "routelist-"}]),
  {ok, RouteConfig} = erlsom:compile_xsd_file("xsd/routeconfig-response.xsd", [{type_prefix,"routeconfig-"}]),
  %These regexps are kind of out of place in this server, but it's a convenient
  %place to put them.
  Regexps = lists:foldl(fun({Exp, Reason}, A) ->
                            {ok, RE} = re:compile(Exp),
                            lists:append(A, [{RE, Reason}])
                        end,
                        [],
                        [{".*You have requested too much data in the last .* seconds\. Limit is .* bytes\..*",
                          too_much_data_requested}
                         ,{".*For agency=.* stop s=.* is on none of the directions for r=.* so cannot determine which stop to provide data for\..*",
                           invalid_stop_for_route}
                         ,{".*For agency=.* route r=.* is not currently available\. It might be initializing still\..*",
                           maybe_invalid_route}
                        ]),
  #{ prediction => Prediction
     , vehicle => Vehicle
     , routeList => RouteList
     , routeConfig => RouteConfig
     , errorReasons => Regexps
   }.

start_link() ->
  gen_server:start_link({local, ?MODULE:getServerName()}, ?MODULE, [], []).

init([]) ->
  {ok, loadModels()}.

handle_cast(load_models, _OldState) ->
  {noreply, loadModels()}.

handle_call(load_models, _From, _OldState) ->
  {reply, ok, loadModels()};

handle_call(Request, _From, Models) when
        Request == prediction orelse Request == vehicle orelse
        Request == routeList orelse Request == routeConfig orelse
        Request == errorReasons ->
  {reply, {ok, maps:get(Request, Models)}, Models}.

handle_info(timeout, State) ->
  lager:warning("~p:handle_info got timeout", [?MODULE]),
  {noreply, State};

handle_info(Info, State) ->
  lager:warning("~p:handle_info got unexpected message, ~p", [?MODULE, Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
