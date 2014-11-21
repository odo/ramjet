-module(ramjet_monitor).

-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CHECKINTERVAL, 1000).

%% Public API

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% Callbacks

init([]) ->
    schedule_check(5 * 1000),
    {ok, never_active}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(check, Activity) ->
    case {has_children(ramjet_session_sup), has_children(ponos_load_generator_sup), Activity} of
        {false, false, was_active} ->
            io:format("test run finished, exiting.\n", []),
            init:stop(),
            {noreply, stopped};
        {true, _, _} ->
            schedule_check(),
            {noreply, was_active};
        {_, true, _} ->
            schedule_check(),
            {noreply, was_active};
        {false, false, never_active} ->
            schedule_check(),
            {noreply, never_active}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

schedule_check() ->
    schedule_check(?CHECKINTERVAL).

schedule_check(Delay) ->
    erlang:send_after(Delay, self(), check).

has_children(Supervisor) ->
    length(supervisor:which_children(Supervisor)) > 0.
