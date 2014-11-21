-module(ramjet_session_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2, child_count/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = {simple_one_for_one, 10, 60},
    SessionServer   = {
        ramjet_session,
        {ramjet_session, start_link, []},
        transient,
        1000,
        worker,
        [ramjet_session]
    },
    {ok, {RestartStrategy, [SessionServer]}}.

start_child(Tasks, Handler) ->
     supervisor:start_child(?MODULE, [Tasks, Handler]).

child_count() ->
    length(supervisor:which_children(?MODULE)).
