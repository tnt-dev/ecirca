# See LICENSE for licensing information.

DIALYZER = dialyzer
REBAR = rebar

all: app

app: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

# dev

devdeps:
	$(REBAR) -C rebar_dev.config get-deps


devapp: devdeps
	$(REBAR) -C rebar_dev.config compile

devclean:
	$(REBAR) -C rebar_dev.config clean

tests: clean app eunit # ct

eunit:
	@$(REBAR) -C rebar_dev.config eunit skip_deps=true

ct:
	@$(REBAR) ct

build-plt:
	@$(DIALYZER) --build_plt \
		--apps kernel stdlib sasl inets crypto public_key ssl wx mnesia

dialyze:
	@$(DIALYZER) -r ebin \
		-Wbehaviours -Werror_handling \
		-Wrace_conditions -Wunmatched_returns -Wunderspecs #-Wbehaviours

docs:
	@$(REBAR) -C rebar_dev.config doc
