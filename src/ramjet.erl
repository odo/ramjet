-module(ramjet).

-export([start/0, start_slave/0, apply_on_all_nodes/3, config/1, tasks/0, expand_tasks/1]).

start() ->
    ok = application:start(bear),
    ok = application:start(folsom),
    ok = application:start(ponos),
    ok = application:set_env(ramjet, report, true),
    ok = application:start(ramjet),
    connect_slaves(),
    ramjet_inc:start(),
    init_handler(),
    start_sessions().

start_slave() ->
    ok = application:start(bear),
    ok = application:start(folsom),
    ok = application:start(ponos),
    ok = application:set_env(ramjet, report, false),
    ok = application:start(ramjet),
    io:format("slave ready on ~p.\n", [host()]).

connect_slaves() ->
    [connect_slave(N) || N <- config(slave_nodes)].

connect_slave(NodeName) ->
    io:format("Connecting to ~p.\n", [NodeName]),
    pong = net_adm:ping(NodeName).

start_sessions() ->
    Tasks = tasks(),
    io:format("~p: starting sessions\n", [?MODULE]),
    io:format("tasks:~p\n", [Tasks]),
    Handler            = config(handler),
    LoadGeneratorCount = config(load_generator_count),
    Names              = [generator_name(ID) || ID <- lists:seq(1, LoadGeneratorCount)],
    StartSession       = fun() -> ramjet_session_sup:start_child(Tasks, Handler) end,
    LoadSpec           = apply(ponos_load_specs, config(load_fun), config(load_fun_args)),
    Options            = [{duration, config(load_duration)}, {auto_init, false}],
    Args               = [
        [ {name, Name}, {task, StartSession}, {load_spec,LoadSpec}, {options, Options}]
        || Name <- Names
    ],
    [apply_on_all_nodes(ponos, add_load_generators, [[Arg]]) || Arg <- Args],
    apply_on_all_nodes(ponos, init_load_generators, [Names]).

init_handler() ->
    Handler = config(handler),
    apply_on_all_nodes(Handler, init_once, []).

apply_on_all_nodes(Module, Fun, Args) ->
    [rpc:call(Node, Module, Fun, Args) || Node <- config(nodes)].

tasks() ->
   expand_tasks(config(tasks)).

config(nodes) ->
    [node() | config(slave_nodes)];

config(slave_nodes) ->
    [node_name(SH) || SH <- config(slave_hosts)];

config(metrics) ->
    lists:usort([element(1, T) || T <- tasks()]);

config(Key) ->
    proplists:get_value(
      Key,
      application:get_all_env(ramjet)
    ).

generator_name(ID) ->
    list_to_atom(atom_to_list(ramjet_generator_) ++ integer_to_list(ID)).

node_name(Host) ->
    list_to_atom("ramjet_slave@" ++ atom_to_list(Host)).

host() ->
    [_, Host] = re:split(atom_to_list(node()), "@"),
    list_to_atom(binary_to_list(Host)).

expand_tasks([]) ->
    [];
expand_tasks([NextTask|Tasks]) ->
    lists:flatten([expand_task(NextTask)|expand_tasks(Tasks)]).

expand_task(Task) ->
    case Task of
        {Number, MultiTask} when is_number(Number) ->
            SubTasks = [MultiTask||_<-lists:seq(1, Number)],
            expand_tasks(SubTasks);
        List when is_list(List) ->
            expand_tasks(List);
        Task when is_tuple(Task) ->
            Task
    end.
