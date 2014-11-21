-module(ramjet).

-export([start/0, start_slave/0, apply_on_all_nodes/3, config/1]).

start() ->
    ok = application:start(bear),
    ok = application:start(folsom),
    ok = application:start(ponos),
    ok = application:set_env(ramjet, report, true),
    ok = application:start(ramjet),
    connect_slaves(),
    start_sessions().

start_slave() ->
    ok = application:start(bear),
    ok = application:start(folsom),
    ok = application:start(ponos),
    ok = application:set_env(ramjet, report, false),
    ok = application:start(ramjet),
    io:format("slave ready.\n", []).

connect_slaves() ->
    [connect_slave(N) || N <- config(slave_nodes)].

connect_slave(NodeName) ->
    io:format("Connecting to ~p.\n", [NodeName]),
    pong = net_adm:ping(NodeName).

start_sessions() ->
    io:format("~p: starting sessions\n", [?MODULE]),
    LoadGeneratorCount = config(load_generator_count),
    Names              = [generator_name(ID) || ID <- lists:seq(1, LoadGeneratorCount)],
    StartSession       = fun() ->
        ramjet_session_sup:start_child(config(tasks), config(handler))
    end,
    LoadSpec           = apply(ponos_load_specs, config(load_fun), config(load_fun_args)),
    Options            = [{duration, config(load_duration)}, {auto_init, false}],
    Args               = [
        [ {name, Name}, {task, StartSession}, {load_spec,LoadSpec}, {options, Options}]
        || Name <- Names
    ],
    [apply_on_all_nodes(ponos, add_load_generators, [[Arg]]) || Arg <- Args],
    apply_on_all_nodes(ponos, init_load_generators, [Names]).

apply_on_all_nodes(Module, Fun, Args) ->
    [rpc:call(Node, Module, Fun, Args) || Node <- config(nodes)].

config(nodes) ->
    [node() | config(slave_nodes)];

config(slave_nodes) ->
    [node_name(SH) || SH <- config(slave_hosts)];

config(metrics) ->
    lists:usort([element(1, T) || T <- config(tasks)]);

config(Key) ->
    proplists:get_value(
      Key,
      application:get_all_env(ramjet)
    ).

generator_name(ID) ->
    list_to_atom(atom_to_list(ramjet_generator_) ++ integer_to_list(ID)).

node_name(Host) ->
    list_to_atom("ramjet_slave@" ++ atom_to_list(Host)).
