all: deps compile

deps:
	./rebar get-deps

compile:
	./rebar compile

clean:
	./rebar clean

run:
	erl \
	  -pa ebin deps/*/ebin \
	  -ernie_server log_level 1 \
	  -eval "application:start(rpc_demo)"
