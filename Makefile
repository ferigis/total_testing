.PHONY: all compile dialyzer clean test

all: compile

compile:
	@./rebar3 compile

dialyzer:
	@./rebar3 dialyzer

clean:
	@rm -rf _build
	@rm -rf Mnesia*
	@rm -f test/*.beam
	@rm -f *.dump rebar.lock
test: compile
	#@./rebar3 eunitl
	@./rebar3 ct