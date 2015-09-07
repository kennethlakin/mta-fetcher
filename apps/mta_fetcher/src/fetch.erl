-module(fetch).

%FIXME: Actually pull these from the env.
-define(FETCH_TIMEOUT, timer:seconds(3)).

-define(JITTER_TIME, random:uniform(200)).
-define(SOCKET_CLOSE_RETRY_WAIT, 2000+?JITTER_TIME).

%Public API
-export([getProfiles/0, request/3, request/4]).
%Internal housekeeping
-export([start/0]).

start() ->
  lists:foreach(fun({P, IP}) ->
                    {ok, Addr} = inet:parse_address(IP),
                    case inets:start(httpc, [{profile, P}]) of
                      {ok, _Pid} -> ok;
                      {error, {already_started, _Pid}} -> ok
                    end,
                    ok = httpc:set_options([{ip, Addr}], P)
                end, fetch:getProfiles()).

getProfiles() ->
  application:get_env(mta_fetcher, fetch_profiles, [{local, "0.0.0.0"}]).

request(Command, Agency, Profiles) ->
  request(Command, Agency, Profiles, []).

request(_Command, _Agency, [], _Args) ->
  {error, no_more_alternatives};
request(Command, Agency, [Profile|Rest], Args) ->
  case doRequest(Command, Agency, Profile, Args) of
    {ok, {{_, Status, _}, _, Body}} ->
      case Status of
        Status when Status == 500 orelse Status == 503 ->
          throw({error, {http_error, Status}});
        200 ->
          ok;
        _ ->
          lager:warning("fetch:request: Unexpected HTTP status ~p", [Status])
      end,
      {ok, {Status, Body, Profile, Rest}};
    {error, socket_closed_remotely} ->
      timer:sleep(?SOCKET_CLOSE_RETRY_WAIT),
      request(Command, Agency, [Profile|Rest], Args);
    {error, timeout} ->
      request(Command, Agency, Rest, Args);
    {error, {failed_connect, _}} ->
      request(Command, Agency, Rest, Args)
  end.

doRequest(C, A, {Profile, _IP}, Args) ->
  REQ_URL=lists:concat(
            ["http://webservices.nextbus.com/service/publicXMLFeed",
            "?command=", C, "&a=", A, Args]),
  get_gzipped_url(REQ_URL, Profile). %ok, status, headers, body

get_gzipped_url(Url, Profile) ->
  get_url(Url, Profile, [{"Accept-Encoding", "gzip"}]).

get_url(Url, Profile, ExtraHeaders) ->
  case Resp=doFetch(Url, Profile, ExtraHeaders)
  of
    {ok, {_StatusLine, Headers, Body}} ->
      {ok, {_StatusLine, Headers, get_body(Headers, Body)}};
    _ ->
      Resp
  end.

doFetch(Url, Profile, ExtraHeaders) when is_list(ExtraHeaders) ->
  {_Http, Host, _Port, _File} = parse(Url),
  Headers=lists:append([{"Host", Host},
                        {"Accept", "text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8"}],
                        ExtraHeaders),
  httpc:request(get, {Url, Headers},
                [{timeout, ?FETCH_TIMEOUT}],
                [{body_format, binary}],
                Profile).

get_body(Headers, Body) ->
  case lists:keysearch("content-encoding", 1, Headers) of
    {value, {_Key, Value}} when Value =:= "gzip" -> zlib:gunzip(Body);
    _ -> Body
  end.

%%----------------------------------------------------------------------
%% parse(URL) -> {http, Site, Port, File} |
%%               {file, File}             | {error,Why}
%% (primitive)

parse([$h,$t,$t,$p,$:,$/,$/|T]) ->  parse_http(T);
parse([$f,$t,$p,$:,$/,$/|_T])    ->  {error, no_ftp};
parse([$f,$i,$l,$e,$:,$/,$/|F]) ->  {file, F};
parse(_X)                        ->  {error, unknown_url_type}.

parse_http(X) ->
  case string:chr(X, $/) of
    0 ->
      %% not terminated by "/" (sigh)
      %% try again
      parse_http(X ++ "/");
    N ->
      %% The Host is up to the first "/"
      %% The file is everything else
      Host = string:substr(X, 1, N-1),
      File = string:substr(X, N, length(X)),
      %% Now check to see if the host name contains a
      %colon
      %% i.e. there is an explicit port address in the
      %hostname
      case string:chr(Host, $:) of
        0 ->
          %% no colon
          Port = 80,
          {http, Host, Port, File};
        M ->
          Site = string:substr(Host,1,M-1),
          case (catch list_to_integer(
                        string:substr(Host, M+1, length(Host)))) of
            {'EXIT', _} ->
              {http, Site, 80, File};
            Port ->
              {http, Site, Port, File}
          end
      end
  end.
