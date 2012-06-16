.PHONY: deps test

all: deps compile

deps:
	@./rebar get-deps

compile: deps
	@./rebar compile

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

test: compile
ifdef suite
	@./rebar skip_deps=true eunit suite=$(suite)
else
	@./rebar skip_deps=true eunit
endif
