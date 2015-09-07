%% HRL file generated by ERLSOM
%%
%% It is possible (and in some cases necessary) to change the name of
%% the record fields.
%%
%% It is possible to add default values, but be aware that these will
%% only be used when *writing* an xml document.


-type anyAttrib()  :: {{string(),    %% name of the attribute
                        string()},   %% namespace
                       string()}.    %% value

-type anyAttribs() :: [anyAttrib()] | undefined.

%% xsd:QName values are translated to #qname{} records.
-record(qname, {uri :: string(),
                localPart :: string(),
                prefix :: string(),
                mappedPrefix :: string()}).



-record(body, {anyAttribs :: anyAttribs(),
	copyright :: string() | undefined,
	'Error' :: 'routeconfig-error'() | undefined,
	route :: 'routeconfig-route'() | undefined}).

-type body() :: #body{}.


-record('routeconfig-route', {anyAttribs :: anyAttribs(),
	tag :: string() | undefined,
	title :: string() | undefined,
	color :: string() | undefined,
	oppositeColor :: string() | undefined,
	latMin :: string() | undefined,
	latMax :: string() | undefined,
	lonMin :: string() | undefined,
	lonMax :: string() | undefined,
	stop :: ['routeconfig-stop'()] | undefined,
	direction :: ['routeconfig-direction'()] | undefined,
	path :: ['routeconfig-path'()] | undefined}).

-type 'routeconfig-route'() :: #'routeconfig-route'{}.


-record('routeconfig-path', {anyAttribs :: anyAttribs(),
	tag :: string() | undefined,
	title :: string() | undefined,
	color :: string() | undefined,
	oppositeColor :: string() | undefined,
	latMin :: string() | undefined,
	latMax :: string() | undefined,
	lonMin :: string() | undefined,
	lonMax :: string() | undefined,
	point :: ['routeconfig-point'()]}).

-type 'routeconfig-path'() :: #'routeconfig-path'{}.


-record('routeconfig-point', {anyAttribs :: anyAttribs(),
	lat :: string() | undefined,
	lon :: string() | undefined}).

-type 'routeconfig-point'() :: #'routeconfig-point'{}.


-record('routeconfig-direction', {anyAttribs :: anyAttribs(),
	tag :: string() | undefined,
	title :: string() | undefined,
	name :: string() | undefined,
	useForUI :: boolean() | undefined,
	stop :: ['routeconfig-direction/stop'()]}).

-type 'routeconfig-direction'() :: #'routeconfig-direction'{}.


-record('routeconfig-direction/stop', {anyAttribs :: anyAttribs(),
	tag :: string() | undefined}).

-type 'routeconfig-direction/stop'() :: #'routeconfig-direction/stop'{}.


-record('routeconfig-stop', {anyAttribs :: anyAttribs(),
	tag :: string() | undefined,
	title :: string() | undefined,
	shortTitle :: string() | undefined,
	lat :: string() | undefined,
	lon :: string() | undefined,
	stopId :: string() | undefined}).

-type 'routeconfig-stop'() :: #'routeconfig-stop'{}.


-record('routeconfig-error', {anyAttribs :: anyAttribs(),
	shouldRetry :: boolean() | undefined,
	'#text' :: string() | undefined}).

-type 'routeconfig-error'() :: #'routeconfig-error'{}.