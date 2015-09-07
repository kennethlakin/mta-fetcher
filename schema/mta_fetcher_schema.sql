--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

--
-- Name: getorcreateagencyidindex(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION getorcreateagencyidindex(input text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
  agencyidIdxVar agencyidindexes.agencyidIndex%TYPE;
begin
  if input is null then
    select agencyidIndex into agencyidIdxVar from agencyidindexes where agencyid is null;
    if not found then
      insert into agencyidindexes (agencyid) values (input);
      select agencyidIndex into agencyidIdxVar  from agencyidindexes where agencyid is null;
    end if;
  else
    select agencyidIndex into agencyidIdxVar from agencyidindexes where agencyid = input;
    if not found then
      insert into agencyidindexes (agencyid) values (input);
      select agencyidIndex into agencyidIdxVar  from agencyidindexes where agencyid = input;
    end if;
  end if;
  return agencyidIdxVar;
end
$$;


--
-- Name: getorcreateblocknumberindex(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION getorcreateblocknumberindex(input text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
  blocknumberIdxVar blocknumberindexes.blocknumberIndex%TYPE;
begin
  if input is null then
    select blocknumberIndex into blocknumberIdxVar from blocknumberindexes where blocknumber is null;
    if not found then
      insert into blocknumberindexes (blocknumber) values (input);
      select blocknumberIndex into blocknumberIdxVar  from blocknumberindexes where blocknumber is null;
    end if;
  else
    select blocknumberIndex into blocknumberIdxVar from blocknumberindexes where blocknumber = input;
    if not found then
      insert into blocknumberindexes (blocknumber) values (input);
      select blocknumberIndex into blocknumberIdxVar  from blocknumberindexes where blocknumber = input;
    end if;
  end if;
  return blocknumberIdxVar;
end
$$;


--
-- Name: getorcreatedirectiontitleindex(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION getorcreatedirectiontitleindex(input text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
  directiontitleIdxVar directiontitleindexes.directiontitleIndex%TYPE;
begin
  if input is null then
    select directiontitleIndex into directiontitleIdxVar from directiontitleindexes where directiontitle is null;
    if not found then
      insert into directiontitleindexes (directiontitle) values (input);
      select directiontitleIndex into directiontitleIdxVar  from directiontitleindexes where directiontitle is null;
    end if;
  else
    select directiontitleIndex into directiontitleIdxVar from directiontitleindexes where directiontitle = input;
    if not found then
      insert into directiontitleindexes (directiontitle) values (input);
      select directiontitleIndex into directiontitleIdxVar  from directiontitleindexes where directiontitle = input;
    end if;
  end if;
  return directiontitleIdxVar;
end
$$;


--
-- Name: getorcreatedirtagindex(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION getorcreatedirtagindex(input text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
  dirtagIdxVar dirtagindexes.dirtagIndex%TYPE;
begin
  if input is null then
    select dirtagIndex into dirtagIdxVar from dirtagindexes where dirtag is null;
    if not found then
      insert into dirtagindexes (dirtag) values (input);
      select dirtagIndex into dirtagIdxVar  from dirtagindexes where dirtag is null;
    end if;
  else
    select dirtagIndex into dirtagIdxVar from dirtagindexes where dirtag = input;
    if not found then
      insert into dirtagindexes (dirtag) values (input);
      select dirtagIndex into dirtagIdxVar  from dirtagindexes where dirtag = input;
    end if;
  end if;
  return dirtagIdxVar;
end
$$;


--
-- Name: getorcreatemessageindex(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION getorcreatemessageindex(input text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
  messageIdxVar messageindexes.messageIndex%TYPE;
begin
  if input is null then
    select messageIndex into messageIdxVar from messageindexes where message is null;
    if not found then
      insert into messageindexes (message) values (input);
      select messageIndex into messageIdxVar  from messageindexes where message is null;
    end if;
  else
    select messageIndex into messageIdxVar from messageindexes where message = input;
    if not found then
      insert into messageindexes (message) values (input);
      select messageIndex into messageIdxVar  from messageindexes where message = input;
    end if;
  end if;
  return messageIdxVar;
end
$$;


--
-- Name: getorcreatepriorityindex(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION getorcreatepriorityindex(input text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
  priorityIdxVar priorityindexes.priorityIndex%TYPE;
begin
  if input is null then
    select priorityIndex into priorityIdxVar from priorityindexes where priority is null;
    if not found then
      insert into priorityindexes (priority) values (input);
      select priorityIndex into priorityIdxVar  from priorityindexes where priority is null;
    end if;
  else
    select priorityIndex into priorityIdxVar from priorityindexes where priority = input;
    if not found then
      insert into priorityindexes (priority) values (input);
      select priorityIndex into priorityIdxVar  from priorityindexes where priority = input;
    end if;
  end if;
  return priorityIdxVar;
end
$$;


--
-- Name: getorcreateroutetagindex(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION getorcreateroutetagindex(input text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
  routetagIdxVar routetagindexes.routetagIndex%TYPE;
begin
  if input is null then
    select routetagIndex into routetagIdxVar from routetagindexes where routetag is null;
    if not found then
      insert into routetagindexes (routetag) values (input);
      select routetagIndex into routetagIdxVar  from routetagindexes where routetag is null;
    end if;
  else
    select routetagIndex into routetagIdxVar from routetagindexes where routetag = input;
    if not found then
      insert into routetagindexes (routetag) values (input);
      select routetagIndex into routetagIdxVar  from routetagindexes where routetag = input;
    end if;
  end if;
  return routetagIdxVar;
end
$$;


--
-- Name: getorcreateroutetitleindex(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION getorcreateroutetitleindex(input text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
  routetitleIdxVar routetitleindexes.routetitleIndex%TYPE;
begin
  if input is null then
    select routetitleIndex into routetitleIdxVar from routetitleindexes where routetitle is null;
    if not found then
      insert into routetitleindexes (routetitle) values (input);
      select routetitleIndex into routetitleIdxVar  from routetitleindexes where routetitle is null;
    end if;
  else
    select routetitleIndex into routetitleIdxVar from routetitleindexes where routetitle = input;
    if not found then
      insert into routetitleindexes (routetitle) values (input);
      select routetitleIndex into routetitleIdxVar  from routetitleindexes where routetitle = input;
    end if;
  end if;
  return routetitleIdxVar;
end
$$;


--
-- Name: getorcreatestoptagindex(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION getorcreatestoptagindex(input text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
  stoptagIdxVar stoptagindexes.stoptagIndex%TYPE;
begin
  if input is null then
    select stoptagIndex into stoptagIdxVar from stoptagindexes where stoptag is null;
    if not found then
      insert into stoptagindexes (stoptag) values (input);
      select stoptagIndex into stoptagIdxVar  from stoptagindexes where stoptag is null;
    end if;
  else
    select stoptagIndex into stoptagIdxVar from stoptagindexes where stoptag = input;
    if not found then
      insert into stoptagindexes (stoptag) values (input);
      select stoptagIndex into stoptagIdxVar  from stoptagindexes where stoptag = input;
    end if;
  end if;
  return stoptagIdxVar;
end
$$;


--
-- Name: getorcreatestoptitleindex(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION getorcreatestoptitleindex(input text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
  stoptitleIdxVar stoptitleindexes.stoptitleIndex%TYPE;
begin
  if input is null then
    select stoptitleIndex into stoptitleIdxVar from stoptitleindexes where stoptitle is null;
    if not found then
      insert into stoptitleindexes (stoptitle) values (input);
      select stoptitleIndex into stoptitleIdxVar  from stoptitleindexes where stoptitle is null;
    end if;
  else
    select stoptitleIndex into stoptitleIdxVar from stoptitleindexes where stoptitle = input;
    if not found then
      insert into stoptitleindexes (stoptitle) values (input);
      select stoptitleIndex into stoptitleIdxVar  from stoptitleindexes where stoptitle = input;
    end if;
  end if;
  return stoptitleIdxVar;
end
$$;


--
-- Name: getorcreatetriptagindex(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION getorcreatetriptagindex(input text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
  triptagIdxVar triptagindexes.triptagIndex%TYPE;
begin
  if input is null then
    select triptagIndex into triptagIdxVar from triptagindexes where triptag is null;
    if not found then
      insert into triptagindexes (triptag) values (input);
      select triptagIndex into triptagIdxVar  from triptagindexes where triptag is null;
    end if;
  else
    select triptagIndex into triptagIdxVar from triptagindexes where triptag = input;
    if not found then
      insert into triptagindexes (triptag) values (input);
      select triptagIndex into triptagIdxVar  from triptagindexes where triptag = input;
    end if;
  end if;
  return triptagIdxVar;
end
$$;


--
-- Name: getorcreatevehicleidindex(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION getorcreatevehicleidindex(input text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
  vehicleidIdxVar vehicleidindexes.vehicleidIndex%TYPE;
begin
  if input is null then
    select vehicleidIndex into vehicleidIdxVar from vehicleidindexes where vehicleid is null;
    if not found then
      insert into vehicleidindexes (vehicleid) values (input);
      select vehicleidIndex into vehicleidIdxVar  from vehicleidindexes where vehicleid is null;
    end if;
  else
    select vehicleidIndex into vehicleidIdxVar from vehicleidindexes where vehicleid = input;
    if not found then
      insert into vehicleidindexes (vehicleid) values (input);
      select vehicleidIndex into vehicleidIdxVar  from vehicleidindexes where vehicleid = input;
    end if;
  end if;
  return vehicleidIdxVar;
end
$$;


--
-- Name: messagesinsert(timestamp with time zone, text, text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION messagesinsert(fetchdate timestamp with time zone, routetag text, stoptag text, message text, priority text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into messages (fetchdatetime, routetagindex, stoptagindex, messageindex, priorityindex)
  values (fetchdate, getorcreateroutetagindex(routetag), getorcreatestoptagindex(stoptag),
    getorcreatemessageindex(message), getorcreatepriorityindex(priority));
end
$$;


--
-- Name: nopredictioninsert(timestamp with time zone, text, text, text, text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION nopredictioninsert(fetchdatetime timestamp with time zone, agencyid text, routetag text, routetitle text, stoptitle text, stoptag text, directiontitle text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into predictions (fetchdatetime, agencyidindex, routetagindex, 
    routetitleindex, stoptitleindex, stoptagindex, directiontitleindex) values
  (fetchdatetime, getorcreateagencyidindex(agencyid), getorcreateroutetagindex(routetag),
    getorcreateroutetitleindex(routetitle), getorcreatestoptitleindex(stoptitle),
    getorcreatestoptagindex(stoptag), getorcreatedirectiontitleindex(directiontitle));
end
$$;


--
-- Name: normalize_vehiclelocations(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION normalize_vehiclelocations() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r vehiclelocations%ROWTYPE;
begin
for r in 
  select * from vehiclelocations
loop
  if r.vehicleid is not null or r.routetag is not null or r.dirtag is not null
  or r.leadingvehicleid is not null
  then
  update vehiclelocations set
    vehicleId = null,
    vehicleidindex = getorcreatevehicleidindex(r.vehicleid),
    routeTag = null,
    routetagindex = getorcreateroutetagindex(r.routetag),
    dirTag = null,
    dirtagindex = getorcreatedirtagindex(r.dirtag),
    leadingVehicleId = null,
    leadingvehicleidindex = getorcreatevehicleidindex(r.leadingvehicleid)
  where id = r.id;
  end if;
end loop;
end
$$;


--
-- Name: predictioninsert(timestamp with time zone, text, text, text, text, text, text, integer, integer, bigint, boolean, boolean, text, numeric, text, text, text, boolean, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION predictioninsert(fetchdatetime timestamp with time zone, agencyid text, routetag text, routetitle text, stoptitle text, stoptag text, directiontitle text, seconds integer, minutes integer, epochtime bigint, isdeparture boolean, affectedbylayover boolean, directiontag text, slowness numeric, vehicleid text, blocknumber text, triptag text, delayed boolean, vehiclesinconsist text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into predictions (fetchdatetime, agencyidindex, routetagindex, 
    routetitleindex, stoptitleindex, 
    stoptagindex, directiontitleindex,
    seconds, minutes, epochtime, isdeparture, 
    affectedbylayover, dirtagindex, slowness, 
    vehicleidindex, blocknumberindex, 
    triptagindex, delayed, vehiclesinconsist) values
  (fetchdatetime, getorcreateagencyidindex(agencyid), getorcreateroutetagindex(routetag),
    getorcreateroutetitleindex(routetitle), getorcreatestoptitleindex(stoptitle),
    getorcreatestoptagindex(stoptag), getorcreatedirectiontitleindex(directiontitle),
    seconds, minutes, epochtime, isdeparture,
    affectedbylayover, getorcreatedirtagindex(directiontag), slowness, 
    getorcreatevehicleidindex(vehicleid), getorcreateblocknumberindex(blocknumber),
    getorcreatetriptagindex(triptag), delayed, vehiclesinconsist);
end
$$;


--
-- Name: vehiclelocationinsert(timestamp with time zone, text, text, text, numeric, numeric, integer, boolean, integer, numeric, text, bigint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION vehiclelocationinsert(fetchdatetime timestamp with time zone, vehicleid text, routetag text, dirtag text, lat numeric, lon numeric, secssincereport integer, predictable boolean, heading integer, speedinkm numeric, leadingvehicleid text, lastdataupdatetime bigint) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into vehiclelocations (fetchdatetime, lat, lon, secssincereport, predictable, heading, speedinkm,
    lastdataupdatetime, vehicleidindex, routetagindex, dirtagindex, leadingvehicleidindex) values
  (fetchdatetime, lat, lon, secssincereport, predictable, heading, speedinkm, lastdataupdatetime,
    getorcreatevehicleidindex(vehicleid), getorcreateroutetagindex(routetag), getorcreatedirtagindex(dirtag),
    getorcreatevehicleidindex(leadingvehicleid));
end
$$;


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: agencyidindexes; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE agencyidindexes (
    agencyidindex integer NOT NULL,
    agencyid text
);


--
-- Name: agencyidindexes_agencyidindex_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE agencyidindexes_agencyidindex_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: agencyidindexes_agencyidindex_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE agencyidindexes_agencyidindex_seq OWNED BY agencyidindexes.agencyidindex;


--
-- Name: blocknumberindexes; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE blocknumberindexes (
    blocknumberindex integer NOT NULL,
    blocknumber text
);


--
-- Name: blocknumberindexes_blocknumberindex_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE blocknumberindexes_blocknumberindex_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: blocknumberindexes_blocknumberindex_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE blocknumberindexes_blocknumberindex_seq OWNED BY blocknumberindexes.blocknumberindex;


--
-- Name: directiontitleindexes; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE directiontitleindexes (
    directiontitleindex integer NOT NULL,
    directiontitle text
);


--
-- Name: directiontitleindexes_directiontitleindex_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE directiontitleindexes_directiontitleindex_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: directiontitleindexes_directiontitleindex_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE directiontitleindexes_directiontitleindex_seq OWNED BY directiontitleindexes.directiontitleindex;


--
-- Name: dirtagindexes; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE dirtagindexes (
    dirtagindex integer NOT NULL,
    dirtag text
);


--
-- Name: dirtagindexes_dirtagindex_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE dirtagindexes_dirtagindex_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: dirtagindexes_dirtagindex_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE dirtagindexes_dirtagindex_seq OWNED BY dirtagindexes.dirtagindex;


--
-- Name: messageindexes; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE messageindexes (
    messageindex integer NOT NULL,
    message text
);


--
-- Name: messageindexes_messageindex_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE messageindexes_messageindex_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: messageindexes_messageindex_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE messageindexes_messageindex_seq OWNED BY messageindexes.messageindex;


--
-- Name: messages; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE messages (
    id bigint NOT NULL,
    fetchdatetime timestamp with time zone,
    routetag text,
    stoptag text,
    message text,
    priority text,
    messageindex integer,
    priorityindex integer,
    routetagindex integer,
    stoptagindex integer
);


--
-- Name: messages_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE messages_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: messages_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE messages_id_seq OWNED BY messages.id;


--
-- Name: predictions; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE predictions (
    id bigint NOT NULL,
    fetchdatetime timestamp with time zone,
    agencyid text,
    routetag text,
    routetitle text,
    stoptitle text,
    stoptag text,
    directiontag text,
    directiontitle text,
    seconds integer,
    minutes integer,
    epochtime bigint,
    isdeparture boolean,
    affectedbylayover boolean,
    slowness numeric,
    vehicleid text,
    blocknumber text,
    triptag text,
    delayed boolean,
    vehiclesinconsist text,
    agencyidindex integer,
    routetagindex integer,
    routetitleindex integer,
    stoptitleindex integer,
    stoptagindex integer,
    dirtagindex integer,
    directiontitleindex integer,
    vehicleidindex integer,
    blocknumberindex integer,
    triptagindex integer
);


--
-- Name: predictions_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE predictions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: predictions_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE predictions_id_seq OWNED BY predictions.id;


--
-- Name: priorityindexes; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE priorityindexes (
    priorityindex integer NOT NULL,
    priority text
);


--
-- Name: priorityindexes_priorityindex_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE priorityindexes_priorityindex_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: priorityindexes_priorityindex_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE priorityindexes_priorityindex_seq OWNED BY priorityindexes.priorityindex;


--
-- Name: routelist; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE routelist (
    id bigint NOT NULL,
    fetchdatetime timestamp with time zone,
    routetag text,
    routetitle text,
    routeshorttitle text
);


--
-- Name: routelist_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE routelist_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: routelist_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE routelist_id_seq OWNED BY routelist.id;


--
-- Name: routetagindexes; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE routetagindexes (
    routetagindex integer NOT NULL,
    routetag text
);


--
-- Name: routetagindexes_routetagindex_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE routetagindexes_routetagindex_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: routetagindexes_routetagindex_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE routetagindexes_routetagindex_seq OWNED BY routetagindexes.routetagindex;


--
-- Name: routetitleindexes; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE routetitleindexes (
    routetitleindex integer NOT NULL,
    routetitle text
);


--
-- Name: routetitleindexes_routetitleindex_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE routetitleindexes_routetitleindex_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: routetitleindexes_routetitleindex_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE routetitleindexes_routetitleindex_seq OWNED BY routetitleindexes.routetitleindex;


--
-- Name: stoptagindexes; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE stoptagindexes (
    stoptagindex integer NOT NULL,
    stoptag text
);


--
-- Name: stoptagindexes_stoptagindex_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE stoptagindexes_stoptagindex_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: stoptagindexes_stoptagindex_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE stoptagindexes_stoptagindex_seq OWNED BY stoptagindexes.stoptagindex;


--
-- Name: stoptitleindexes; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE stoptitleindexes (
    stoptitleindex integer NOT NULL,
    stoptitle text
);


--
-- Name: stoptitleindexes_stoptitleindex_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE stoptitleindexes_stoptitleindex_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: stoptitleindexes_stoptitleindex_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE stoptitleindexes_stoptitleindex_seq OWNED BY stoptitleindexes.stoptitleindex;


--
-- Name: triptagindexes; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE triptagindexes (
    triptagindex integer NOT NULL,
    triptag text
);


--
-- Name: triptagindexes_triptagindex_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE triptagindexes_triptagindex_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: triptagindexes_triptagindex_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE triptagindexes_triptagindex_seq OWNED BY triptagindexes.triptagindex;


--
-- Name: vehicleidindexes; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE vehicleidindexes (
    vehicleidindex integer NOT NULL,
    vehicleid text
);


--
-- Name: vehicleidindexes_vehicleidindex_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE vehicleidindexes_vehicleidindex_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: vehicleidindexes_vehicleidindex_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE vehicleidindexes_vehicleidindex_seq OWNED BY vehicleidindexes.vehicleidindex;


--
-- Name: vehiclelocations; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE vehiclelocations (
    id bigint NOT NULL,
    fetchdatetime timestamp with time zone,
    lat numeric,
    lon numeric,
    secssincereport integer,
    predictable boolean,
    heading integer,
    speedinkm numeric,
    lastdataupdatetime bigint,
    vehicleidindex integer,
    routetagindex integer,
    dirtagindex integer,
    leadingvehicleidindex integer
);


--
-- Name: vehiclelocations_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE vehiclelocations_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: vehiclelocations_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE vehiclelocations_id_seq OWNED BY vehiclelocations.id;


--
-- Name: agencyidindex; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY agencyidindexes ALTER COLUMN agencyidindex SET DEFAULT nextval('agencyidindexes_agencyidindex_seq'::regclass);


--
-- Name: blocknumberindex; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY blocknumberindexes ALTER COLUMN blocknumberindex SET DEFAULT nextval('blocknumberindexes_blocknumberindex_seq'::regclass);


--
-- Name: directiontitleindex; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY directiontitleindexes ALTER COLUMN directiontitleindex SET DEFAULT nextval('directiontitleindexes_directiontitleindex_seq'::regclass);


--
-- Name: dirtagindex; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY dirtagindexes ALTER COLUMN dirtagindex SET DEFAULT nextval('dirtagindexes_dirtagindex_seq'::regclass);


--
-- Name: messageindex; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY messageindexes ALTER COLUMN messageindex SET DEFAULT nextval('messageindexes_messageindex_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY messages ALTER COLUMN id SET DEFAULT nextval('messages_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY predictions ALTER COLUMN id SET DEFAULT nextval('predictions_id_seq'::regclass);


--
-- Name: priorityindex; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY priorityindexes ALTER COLUMN priorityindex SET DEFAULT nextval('priorityindexes_priorityindex_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY routelist ALTER COLUMN id SET DEFAULT nextval('routelist_id_seq'::regclass);


--
-- Name: routetagindex; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY routetagindexes ALTER COLUMN routetagindex SET DEFAULT nextval('routetagindexes_routetagindex_seq'::regclass);


--
-- Name: routetitleindex; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY routetitleindexes ALTER COLUMN routetitleindex SET DEFAULT nextval('routetitleindexes_routetitleindex_seq'::regclass);


--
-- Name: stoptagindex; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY stoptagindexes ALTER COLUMN stoptagindex SET DEFAULT nextval('stoptagindexes_stoptagindex_seq'::regclass);


--
-- Name: stoptitleindex; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY stoptitleindexes ALTER COLUMN stoptitleindex SET DEFAULT nextval('stoptitleindexes_stoptitleindex_seq'::regclass);


--
-- Name: triptagindex; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY triptagindexes ALTER COLUMN triptagindex SET DEFAULT nextval('triptagindexes_triptagindex_seq'::regclass);


--
-- Name: vehicleidindex; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY vehicleidindexes ALTER COLUMN vehicleidindex SET DEFAULT nextval('vehicleidindexes_vehicleidindex_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY vehiclelocations ALTER COLUMN id SET DEFAULT nextval('vehiclelocations_id_seq'::regclass);


--
-- Name: agencyidindexes_agencyid_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY agencyidindexes
    ADD CONSTRAINT agencyidindexes_agencyid_key UNIQUE (agencyid);


--
-- Name: agencyidindexes_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY agencyidindexes
    ADD CONSTRAINT agencyidindexes_pkey PRIMARY KEY (agencyidindex);


--
-- Name: blocknumberindexes_blocknumber_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY blocknumberindexes
    ADD CONSTRAINT blocknumberindexes_blocknumber_key UNIQUE (blocknumber);


--
-- Name: blocknumberindexes_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY blocknumberindexes
    ADD CONSTRAINT blocknumberindexes_pkey PRIMARY KEY (blocknumberindex);


--
-- Name: directiontitleindexes_directiontitle_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY directiontitleindexes
    ADD CONSTRAINT directiontitleindexes_directiontitle_key UNIQUE (directiontitle);


--
-- Name: directiontitleindexes_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY directiontitleindexes
    ADD CONSTRAINT directiontitleindexes_pkey PRIMARY KEY (directiontitleindex);


--
-- Name: dirtagindexes_dirtag_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY dirtagindexes
    ADD CONSTRAINT dirtagindexes_dirtag_key UNIQUE (dirtag);


--
-- Name: dirtagindexes_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY dirtagindexes
    ADD CONSTRAINT dirtagindexes_pkey PRIMARY KEY (dirtagindex);


--
-- Name: messageindexes_message_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY messageindexes
    ADD CONSTRAINT messageindexes_message_key UNIQUE (message);


--
-- Name: messageindexes_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY messageindexes
    ADD CONSTRAINT messageindexes_pkey PRIMARY KEY (messageindex);


--
-- Name: messages_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY messages
    ADD CONSTRAINT messages_pkey PRIMARY KEY (id);


--
-- Name: predictions_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY predictions
    ADD CONSTRAINT predictions_pkey PRIMARY KEY (id);


--
-- Name: priorityindexes_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY priorityindexes
    ADD CONSTRAINT priorityindexes_pkey PRIMARY KEY (priorityindex);


--
-- Name: priorityindexes_priority_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY priorityindexes
    ADD CONSTRAINT priorityindexes_priority_key UNIQUE (priority);


--
-- Name: routelist_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY routelist
    ADD CONSTRAINT routelist_pkey PRIMARY KEY (id);


--
-- Name: routetagindexes_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY routetagindexes
    ADD CONSTRAINT routetagindexes_pkey PRIMARY KEY (routetagindex);


--
-- Name: routetagindexes_routetag_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY routetagindexes
    ADD CONSTRAINT routetagindexes_routetag_key UNIQUE (routetag);


--
-- Name: routetitleindexes_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY routetitleindexes
    ADD CONSTRAINT routetitleindexes_pkey PRIMARY KEY (routetitleindex);


--
-- Name: routetitleindexes_routetitle_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY routetitleindexes
    ADD CONSTRAINT routetitleindexes_routetitle_key UNIQUE (routetitle);


--
-- Name: stoptagindexes_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY stoptagindexes
    ADD CONSTRAINT stoptagindexes_pkey PRIMARY KEY (stoptagindex);


--
-- Name: stoptagindexes_stoptag_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY stoptagindexes
    ADD CONSTRAINT stoptagindexes_stoptag_key UNIQUE (stoptag);


--
-- Name: stoptitleindexes_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY stoptitleindexes
    ADD CONSTRAINT stoptitleindexes_pkey PRIMARY KEY (stoptitleindex);


--
-- Name: stoptitleindexes_stoptitle_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY stoptitleindexes
    ADD CONSTRAINT stoptitleindexes_stoptitle_key UNIQUE (stoptitle);


--
-- Name: triptagindexes_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY triptagindexes
    ADD CONSTRAINT triptagindexes_pkey PRIMARY KEY (triptagindex);


--
-- Name: triptagindexes_triptag_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY triptagindexes
    ADD CONSTRAINT triptagindexes_triptag_key UNIQUE (triptag);


--
-- Name: vehicleidindexes_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY vehicleidindexes
    ADD CONSTRAINT vehicleidindexes_pkey PRIMARY KEY (vehicleidindex);


--
-- Name: vehicleidindexes_vehicleid_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY vehicleidindexes
    ADD CONSTRAINT vehicleidindexes_vehicleid_key UNIQUE (vehicleid);


--
-- Name: vehiclelocations_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY vehiclelocations
    ADD CONSTRAINT vehiclelocations_pkey PRIMARY KEY (id);


--
-- Name: agencyidindexes_agencyid_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX agencyidindexes_agencyid_idx ON agencyidindexes USING btree (agencyid);


--
-- Name: blocknumberindexes_blocknumber_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX blocknumberindexes_blocknumber_idx ON blocknumberindexes USING btree (blocknumber);


--
-- Name: directiontitleindexes_directiontitle_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX directiontitleindexes_directiontitle_idx ON directiontitleindexes USING btree (directiontitle);


--
-- Name: dirtagindexes_dirtag_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX dirtagindexes_dirtag_idx ON dirtagindexes USING btree (dirtag);


--
-- Name: messageindexes_message_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX messageindexes_message_idx ON messageindexes USING btree (message);


--
-- Name: messages_fetchdatetime_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX messages_fetchdatetime_idx ON messages USING btree (fetchdatetime);


--
-- Name: predictions_fetchdatetime_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX predictions_fetchdatetime_idx ON predictions USING btree (fetchdatetime);


--
-- Name: priorityindexes_priority_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX priorityindexes_priority_idx ON priorityindexes USING btree (priority);


--
-- Name: routelist_fetchdatetime_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX routelist_fetchdatetime_idx ON routelist USING btree (fetchdatetime);


--
-- Name: routetagindexes_routetag_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX routetagindexes_routetag_idx ON routetagindexes USING btree (routetag);


--
-- Name: routetitleindexes_routetitle_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX routetitleindexes_routetitle_idx ON routetitleindexes USING btree (routetitle);


--
-- Name: stoptagindexes_stoptag_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX stoptagindexes_stoptag_idx ON stoptagindexes USING btree (stoptag);


--
-- Name: stoptitleindexes_stoptitle_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX stoptitleindexes_stoptitle_idx ON stoptitleindexes USING btree (stoptitle);


--
-- Name: triptagindexes_triptag_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX triptagindexes_triptag_idx ON triptagindexes USING btree (triptag);


--
-- Name: vehicleidindexes_vehicleid_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX vehicleidindexes_vehicleid_idx ON vehicleidindexes USING btree (vehicleid);


--
-- Name: vehiclelocations_fetchdatetime_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX vehiclelocations_fetchdatetime_idx ON vehiclelocations USING btree (fetchdatetime);


--
-- Name: vehiclelocations_routetagindex_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX vehiclelocations_routetagindex_idx ON vehiclelocations USING btree (routetagindex);


--
-- Name: vehiclelocations_vehicleidindex_idx; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX vehiclelocations_vehicleidindex_idx ON vehiclelocations USING btree (vehicleidindex);

--
-- PostgreSQL database dump complete
--

