start:
	./rebar com skip_deps=true && erl -pz ebin deps/*/ebin -config config/default.config -s ramjet

graph:
	Rscript --vanilla priv/summary.r -i tests/current
