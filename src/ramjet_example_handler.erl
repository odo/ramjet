-module(ramjet_example_handler).

-export([init/0, handle_task/2]).

-behaviour(ramjet_handler).

init() ->
    0.

handle_task({wait, Millisecords}, TaskState) ->
    timer:sleep((round(Millisecords - random:uniform() * Millisecords / 2))),
    {ok, TaskState + 1};

handle_task({print, Data}, TaskState) ->
    io:format("~p\n", [Data]),
    {ok, TaskState + 1};

handle_task({print_call_count}, TaskState) ->
    io:format("Call count: ~p\n", [TaskState]),
    {ok, TaskState + 1};

handle_task({noop}, TaskState) ->
    {ok, TaskState}.
