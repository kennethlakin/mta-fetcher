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
	error :: 'agencylist-error'() | undefined,
	agency :: ['agencylist-agency'()] | undefined}).

-type body() :: #body{}.


-record('agencylist-agency', {anyAttribs :: anyAttribs(),
	tag :: string() | undefined,
	title :: string() | undefined,
	shortTitle :: string() | undefined,
	regionTitle :: string() | undefined}).

-type 'agencylist-agency'() :: #'agencylist-agency'{}.


-record('agencylist-error', {anyAttribs :: anyAttribs(),
	shouldRetry :: boolean() | undefined,
	'#text' :: string() | undefined}).

-type 'agencylist-error'() :: #'agencylist-error'{}.