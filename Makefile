HOST=$(shell hostname)
config?="config/default.config"

export ERL_LIBS:=$(ERL_LIBS):_build/default/lib
ERL_FLAGS=-noshell -setcookie zeramjet -config $(config) -pz $(incl)

all:
	@rebar3 compile

start: all
	@erl $(ERL_FLAGS) -name ramjet@$(HOST) $(ERL_FLAGS) -s ramjet

slave: all
	@erl $(ERL_FLAGS) -name ramjet_slave@$(HOST) $(ERL_FLAGS) -s ramjet start_slave

graph:
	@Rscript --vanilla priv/summary.r -i tests/current

debug_run: all
	@erl $(ERL_FLAGS) -name ramjet@$(HOST) -eval "ramjet_debug:run()."
