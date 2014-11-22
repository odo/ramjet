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
    TaskState = Handler:init(),
    ramjet_stats:record(ramjet_session_start, 0),
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
    Elapsed = timer:now_diff(os:timestamp(), Before) * 1.0,
    case Outcome of
        ok ->
            ramjet_stats:record(Command, Elapsed);
        error ->
            ramjet_stats:record(Command, error)
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
