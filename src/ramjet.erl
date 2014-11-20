-module(ramjet).

-export([start/0, config/1]).

start() ->
    application:start(bear),
    application:start(folsom),
    application:start(ponos),
    application:start(ramjet),
    start_sessions().

start_sessions() ->
    LoadGeneratorCount = config(load_generator_count),
    Names              = [generator_name(ID) || ID <- lists:seq(1, LoadGeneratorCount)],
    StartSession       = fun() ->
        ramjet_session:start(config(tasks), config(handler))
    end,
    LoadSpec           = apply(ponos_load_specs, config(load_fun), config(load_fun_args)),
    Options            = [{duration, config(load_duration)}, {auto_init, false}],
    Args               = [
        [ {name, Name}, {task, StartSession}, {load_spec,LoadSpec}, {options, Options}]
        || Name <- Names
    ],
    [[ok]              = ponos:add_load_generators([Arg]) || Arg <- Args],
    ponos:init_load_generators(Names).

config(metrics) ->
    lists:usort([element(1, T) || T <- config(tasks)]);

config(Key) ->
    proplists:get_value(
      Key,
      application:get_all_env(ramjet)
    ).

generator_name(ID) ->
    list_to_atom(atom_to_list(ramjet_generator_) ++ integer_to_list(ID)).

