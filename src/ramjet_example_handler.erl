-module(ramjet_example_handler).

-export([init_once/0, init/1, handle_task/2, terminate/1]).

-behaviour(ramjet_handler).

init_once() ->
    noop.

init(Id) ->
    io:format("id: ~p\n", [Id]),
    0.

handle_task({wait, Millisecords}, TaskState) ->
    WaitFor = (round(
        Millisecords + (rand:uniform() * Millisecords) - Millisecords / 2
    )),
    timer:sleep(WaitFor),
    {ok, TaskState + 1};

handle_task({print_call_count}, TaskState) ->
    io:format("~p: Call count: ~p\n", [self(), TaskState]),
    {ok, TaskState + 1};

handle_task({noop}, TaskState) ->
    {ok, TaskState + 1}.

terminate(_State) ->
    noop.
