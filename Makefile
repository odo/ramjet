shell:
	./rebar com skip_deps=true && erl -pz ebin deps/*/ebin -config config/default.config -s ramjet
