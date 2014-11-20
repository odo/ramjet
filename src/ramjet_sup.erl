-module(ramjet_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
    Metrics = ramjet:config(metrics),
    Stats = {
      ramjet_stats, {
        ramjet_stats,
        start_link,
        [Metrics, 10000]
       },
        permanent,
        2000,
        worker,
        [ramjet_stats]},
    {ok, { {one_for_one, 5, 10}, [Stats]} }.


