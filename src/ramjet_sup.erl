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
    StatsInterval = ramjet:config(stats_interval),

    Stats = {
      ramjet_stats, {
        ramjet_stats,
        start_link,
        [Metrics, StatsInterval]
       },
        permanent,
        2000,
        worker,
        [ramjet_stats]},

    SessionSup = {
      ramjet_session_sup, {
        ramjet_session_sup,
        start_link,
        []
       },
        permanent,
        2000,
        supervisor,
        [ramjet_session_sup, ramjet_session]},

    Children =
    case ramjet:config(report) of
        true ->
            [Stats, SessionSup];
        false ->
            [SessionSup]
    end,

    {ok, { {one_for_one, 5, 10}, Children} }.


