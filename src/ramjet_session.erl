-module(ramjet_session).

-behaviour(gen_server).
-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tasks :: list(), task_state :: any(), handler :: atom() }).

%% Public API

start_link(Tasks, Handler) ->
    gen_server:start_link(?MODULE, [Tasks, Handler], []).

%% Callbacks

init([Tasks, Handler]) ->
    self() ! next_task,
    Id = ramjet_inc:inc(),
    TaskState = Handler:init(Id),
    ramjet_stats:record(ramjet_session_start, 0),
    {ok, #state{ task_state = TaskState, tasks = Tasks, handler = Handler}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(next_task, State = #state{tasks = [], task_state = TaskState, handler = Handler}) ->
    Handler:terminate(TaskState),
    {stop, normal, State};
handle_info(next_task, State = #state{task_state = TaskState, handler = Handler, tasks = [NextTask | Tasks]}) ->
    Command = element(1, NextTask),
    Before  = erlang:system_time(),
    {Outcome, NewTaskState}   =  Handler:handle_task(NextTask, TaskState),
    Elapsed = float(erlang:convert_time_unit(erlang:system_time() - Before, native, microsecond)),
    case Outcome of
        ok ->
            ramjet_stats:record(Command, Elapsed);
        halt ->
            ramjet_stats:record(Command, Elapsed),
            Handler:terminate(TaskState);
        error ->
            ramjet_stats:record(Command, error),
            Handler:terminate(TaskState)
    end,

    self() ! next_task,
    {noreply, State#state{task_state = NewTaskState, tasks = Tasks}}.

terminate(normal, _State) ->
    ok;
terminate(_Reason, #state{tasks = [CrashingTask|_]}) ->
    Command = element(1, CrashingTask),
    ramjet_stats:record(Command, error),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
