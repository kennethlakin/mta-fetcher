# mta-fetcher
## Purpose
Reliably fetch a single agency's transit information from the NextBus Public XML Feed and squirrel it away in a Postgres DB for later analysis.

## Front matter
* NextBus is a registered trademark of NextBus Inc. This software is *not* supplied, endorsed, supported, or condoned by NextBus, Inc. If this software malfunctions in any way, don't ask NextBus Inc. for help.
* If you're considering *actually* using this software (as opposed to just trying it out), *please* coordinate with me first. There's no reason to run more than two copies of this software for any given transit agency. Also, make sure to check out the caveats section below.

## Building
### Prerequisites
* Erlang 18.x
* Rebar 2.x 
* Postgres 9.3 or later

### Build instructions
* Check out the repo
* Ensure that rebar is in your PATH
* At the top level:
```
make deps && make
```
## Getting started
### Postgres Database Configuration
* Create a new Postgres database. Grant ALL access to that database to a Postgres user.
  * EXECUTE ALL *should* be the only permission required, but I've not yet tested that configuration.
  * I don't know whether a passwordless Postgres user will work correctly.
* Execute schema/mta_fetcher_schema.sql against your new Postgres database.

### App.config customization
* Copy app.config.example to app.config.
* Open app.config in a text editor and change the user, pass, host, and
  database values to match those required to access your newly-configured
  Postgres database.
* If you want mta-fetcher to also log to syslog, uncomment the
  lager_syslog_backend line.

### Mnesia database configuration
The Mnesia tables need to be initialized. (Making this automatic is on the TODO list.)
* Run
```
make justload
```
* At the Erlang shell, do
```
ok=mnesia:create_schema([node()]).
ok=mnesia:start().
{atomic,ok}=process:createAllTables().
q().
```
Now your Mnesia tables *should* be initialized.
### Starting the fetcher
Run
```
make start
```
You should see notifications about the start of lager, mta_fetcher, and mta_fetcher_database. A crash shortly after startup is *not* normal.

## Interesting makefile targets
* justload
  * Loads all the modules require to run the fetcher but does not start the applications.
* monitor
  * Starts a hidden Erlang node, then runs observer:start(). This is a tool somewhat similar to Java's jconsole. You can use it to connect to the mta_fetcher node and inspect its CPU and memory usage, and the state of its ETS and Mnesia tables.

## Caveats
* mta_fetcher_database will almost certainly lose data if
  * It is stopped before it has moved all vehicle and prediction data from Mnesia to Postgres.
* The Postgres schema (particularly the stored procedures) is not very good.
  * The current schema file is a pg_dump with the GRANT statement blocks removed and is pretty ugly.

Addressing these caveats is on the TODO list.

## Credits
* Makes use of and builds upon Jim Doyle's XSD files for working with the NextBus XML feed found at http://sourceforge.net/p/nextbusapi/code/30/tree/trunk/adapter/src/main/resources/
* Makes use of and builds upon Tim Gebhart's code to request and handle gzipped HTTP found at http://blog.gebhardtcomputing.com/2007/09/grab-webpage-in-erlang-which-is-gzipped.html
* This software would not have been possible without Nextbus Inc's Public XML Feed documentation found at https://www.nextbus.com/xmlFeedDocs/NextBusXMLFeed.pdf

## License
Distributed under the terms of the LGPL or GPL v3 (your choice) or later (also your choice).
