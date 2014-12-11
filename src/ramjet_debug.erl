-module(ramjet_debug).

-export([run/0]).

run() ->
    application:load(ramjet),
    Tasks   = ramjet:tasks(),
    Handler = ramjet:config(handler),
    io:format("\ndebug_run: tasks: ~p\n", [Tasks]),
    io:format("debug_run: calling ~p:init(1)\n", [Handler]),
    InitState = Handler:init(1),
    io:format("debug_run: initial state is: ~p\n\n", [InitState]),
    next_task(Tasks, InitState, Handler),
    io:format("\ndebug_run: done.\n"),
    init:stop().

next_task([], State, Handler) ->
    io:format("debug_run: calling ~p:terminate(~p)\n", [Handler, State]),
    Result = Handler:terminate(State),
    io:format("debug_run: return value is: ~p\n", [Result]),
    noop;
next_task([Task|Tasks], State, Handler) ->
    io:format("debug_run: calling ~p:handle_task(~p, ~p)\n", [Handler, Task, State]),
    {ok, NewState} = Handler:handle_task(Task, State),
    io:format("debug_run: new state is: ~p\n\n", [NewState]),
    next_task(Tasks, NewState, Handler).

