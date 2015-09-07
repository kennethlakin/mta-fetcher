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
	'Error' :: 'prediction-error'() | undefined,
	predictions :: ['prediction-predictions'()] | undefined}).

-type body() :: #body{}.


-record('prediction-predictions', {anyAttribs :: anyAttribs(),
	agencyTitle :: string() | undefined,
	routeTitle :: string() | undefined,
	routeTag :: string() | undefined,
	stopTitle :: string() | undefined,
	stopTag :: string() | undefined,
	dirTitleBecauseNoPredictions :: string() | undefined,
	direction :: ['prediction-direction'()] | undefined,
	message :: ['prediction-message'()] | undefined}).

-type 'prediction-predictions'() :: #'prediction-predictions'{}.


-record('prediction-message', {anyAttribs :: anyAttribs(),
	text :: string() | undefined,
	priority :: string() | undefined}).

-type 'prediction-message'() :: #'prediction-message'{}.


-record('prediction-direction', {anyAttribs :: anyAttribs(),
	title :: string() | undefined,
	prediction :: ['prediction-direction/prediction'()]}).

-type 'prediction-direction'() :: #'prediction-direction'{}.


-record('prediction-direction/prediction', {anyAttribs :: anyAttribs(),
	seconds :: integer() | undefined,
	minutes :: integer() | undefined,
	epochTime :: string() | undefined,
	isDeparture :: boolean() | undefined,
	affectedByLayover :: boolean() | undefined,
	dirTag :: string() | undefined,
	slowness :: string() | undefined,
	vehicle :: string() | undefined,
	block :: string() | undefined,
	tripTag :: string() | undefined,
	delayed :: boolean() | undefined,
	vehiclesInConsist :: string() | undefined}).

-type 'prediction-direction/prediction'() :: #'prediction-direction/prediction'{}.


-record('prediction-error', {anyAttribs :: anyAttribs(),
	shouldRetry :: boolean() | undefined,
	'#text' :: string() | undefined}).

-type 'prediction-error'() :: #'prediction-error'{}.