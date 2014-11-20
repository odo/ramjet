-module(ramjet_session).

-behaviour(gen_server).
-export([start/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tasks :: list(), task_state :: any(), handler :: atom() }).

%% == usage ==
%% 1> ponos_session:start([{print, "Hello"}, {wait, 1000}, {print, "World"}, print_call_count], 0).
%% "Hello"
%% {ok,<0.43.0>}
%% "World"
%% Call count: 3

%% == from ponos ==
%% StartSession = fun() -> ponos_session:start([{print, "Hello"}, {wait, 1000}, {print, "World"}, print_call_count], 0) end.
%% Name         = session_test.
%% LoadSpec     = ponos_load_specs:make_constant(10.0).
%% Options  =  [{duration, 10 * 1000}, {auto_init, false}].
%% Args = [ {name, Name}, {task, StartSession}, {load_spec,LoadSpec}, {options, Options}].
%% [ok] = ponos:add_load_generators([Args]).
%% ponos:init_load_generators([Name]).

%% Public API

start(Tasks, Handler) ->
    gen_server:start_link(?MODULE, [Tasks, Handler], []).

%% Callbacks

init([Tasks, Handler]) ->
    random:seed(os:timestamp()),
    self() ! next_task,
    TaskState = Handler:init(),
    {ok, #state{ task_state = TaskState, tasks = Tasks, handler = Handler}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(next_task, State = #state{tasks = []}) ->
     {stop, normal, State};
handle_info(next_task, State = #state{task_state = TaskState, handler = Handler, tasks = [NextTask | Tasks]}) ->
    Command = element(1, NextTask),
    Before  = os:timestamp(),
    {Outcome, NewTaskState}   =  Handler:handle_task(NextTask, TaskState),
    Elapsed = timer:now_diff(os:timestamp(), Before) / 1000,
    case Outcome of
        ok ->
            ramjet_stats:record(Command, Elapsed);
        error ->
            ramjet_stats:record(Command, error)
    end,

    self() ! next_task,
    {noreply, State#state{task_state = NewTaskState, tasks = Tasks}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
