.PHONY: all compile test dialyzer

all:
	@ echo "Building escript..."
	@ ./rebar3 as prod escriptize
	@ cp _build/prod/bin/erlang_srv ./

compile:
	@ echo "Compiling..."
	@ ./rebar3 compile

test:
	@ echo "Testing..."
	@ ./rebar3 ct

dialyzer:
	@ echo "Running dialyzer"
	@ ./rebar3 dialyzer

