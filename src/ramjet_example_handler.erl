-module(ramjet_example_handler).

-export([init/0, handle_task/2]).

-behaviour(ramjet_handler).

init() ->
    random:seed(os:timestamp()),
    0.

handle_task({wait, Millisecords}, TaskState) ->
    WaitFor = (round(
        Millisecords + (random:uniform() * Millisecords) - Millisecords / 2
    )),
    timer:sleep(WaitFor),
    {ok, TaskState + 1};

handle_task({print_call_count}, TaskState) ->
    io:format("~p: Call count: ~p\n", [self(), TaskState]),
    {ok, TaskState + 1}.
