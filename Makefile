HOST=$(shell hostname)

all:
	./rebar get com

start:
	./rebar com skip_deps=true && erl -name ramjet@$(HOST) -pz ebin deps/*/ebin -config config/default.config -s ramjet

slave:
	./rebar com skip_deps=true && erl -noshell -name ramjet_slave@$(HOST) -pz ebin deps/*/ebin -config config/default.config -eval "ramjet:start_slave()"

graph:
	Rscript --vanilla priv/summary.r -i tests/current
