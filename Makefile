all: get-deps compile

get-deps:
	./rebar get-deps

compile:
	./rebar compile

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps

run:
	erl -pa deps/*/ebin deps/*/include ebin -name rmud@127.0.0.1 -config etc/app.config


##
## Dialyzer
##
APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.mud_context_combo_dialyzer_plt

check_plt: deps compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin ebin

build_plt: deps compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin ebin

doc: deps compile
	./rebar skip_deps=true doc

dialyzer_apps: deps compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wunmatched_returns -Werror_handling -Wrace_conditions -Wbehaviors -Wunderspecs -Wno_return -Wno_undefined_callbacks --plt $(COMBO_PLT) ebin

dialyzer: deps compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return -Wno_undefined_callbacks --plt $(COMBO_PLT) deps/*/ebin ebin

eunit: deps compile
	ERL_FLAGS="-name rmud@127.0.0.1 -config etc/app.config" ./rebar eunit skip_deps=true

cleanplt:
	@echo
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)

