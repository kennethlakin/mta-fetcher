EPMD_ADDRESS=ERL_EPMD_ADDRESS="127.0.0.1"
FETCHER_NODE_NAME=mta-data-fetcher
MONITOR_NODE_NAME=observer
ERL_BASE=$(EPMD_ADDRESS) erl
FETCHER_BASE_COMMAND=$(ERL_BASE) -name $(FETCHER_NODE_NAME) -mnesia dir "'db-$(FETCHER_NODE_NAME)'" -mnesia dump_log_write_threshold 1000 -pa ebin deps/*/ebin apps/*/ebin -config app

all:
	rebar co

deps:
	rebar g-d

start:
	 $(FETCHER_BASE_COMMAND) -s mta_fetcher -s database

justload:
	$(FETCHER_BASE_COMMAND)

monitor:
	$(ERL_BASE) -name $(MONITOR_NODE_NAME) -hidden -eval 'observer:start()'

distclean: clean

clean:
	rebar -r clean

