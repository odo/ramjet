HOST=$(shell hostname)
ifdef config
	CONFIG=$(config)
else
	CONFIG="config/default.config"
endif

all:
	./rebar get com

start:
	./rebar com skip_deps=true && erl -noshell -name ramjet@$(HOST) -setcookie zeramjet -pz ebin deps/*/ebin $(incl) -config $(CONFIG) -s ramjet

slave:
	./rebar com skip_deps=true && erl -noshell -name ramjet_slave@$(HOST) -setcookie zeramjet -pz ebin deps/*/ebin $(incl) -config $(CONFIG) -eval "ramjet:start_slave()"

graph:
	Rscript --vanilla priv/summary.r -i tests/current

debug_run:
	./rebar com skip_deps=true && erl -noshell -name ramjet@$(HOST) -setcookie zeramjet -pz ebin deps/*/ebin $(incl) -config $(CONFIG) -eval "ramjet_debug:run()."
