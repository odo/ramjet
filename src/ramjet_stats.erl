-module(ramjet_stats).

-behaviour(gen_server).
-export([record/2, ensure_delete/1]).
-export([start_link/3, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, prepare_dir/0]).

-define(COMPRESSEVERY, 5).

-record(state, {
    started_at     :: number(),
    dump_interval  :: number(),
    last_dump      :: tuple(),
    metrics        :: list(),
    ignore_metrics :: list(),
    total_sessions :: number(),
    csv_dir        :: list()
}).

%% Public API

% ramjet_stats resets the histograms and counters
% so they might be unavailable for a brief time
% we ignore those errors

record(Metric, error) ->
    try
        folsom_metrics:notify({counter_name(Metric), {inc, 1}})
    catch
        _:badarg -> noop
    end,
    try
        folsom_metrics:notify({error_counter_name(Metric), {inc, 1}})
    catch
        _:badarg -> noop
    end;

record(Metric, Duration) when is_number(Duration) ->
    try
        folsom_metrics:notify({histogram_name(Metric), Duration})
    catch
        _:{badmatch, []} -> noop
    end,
    try
        folsom_metrics:notify({counter_name(Metric), {inc, 1}})
    catch
        _:badarg -> noop
    end.

start_link(Metrics, IgnoreMetrics, DumpInterval) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Metrics, IgnoreMetrics, DumpInterval], []).

%% Callbacks

init([Metrics, IgnoreMetrics, DumpInterval]) ->
    io:format("~p: starting with metrics:~p, ignoring:~p\n", [?MODULE, Metrics, IgnoreMetrics]),
    schedule_dump(DumpInterval),
    reset_metrics(Metrics, DumpInterval),
    CSVDir = prepare_dir(),
    write_csv_headers(Metrics, CSVDir),
    {ok, #state{ metrics = Metrics, ignore_metrics = IgnoreMetrics, started_at = timestamp(), dump_interval = DumpInterval, last_dump = timestamp(), total_sessions = 0, csv_dir = CSVDir }}.

handle_call({total_sessions}, _From, State = #state{ total_sessions = TotalSessions }) ->
    {reply, TotalSessions, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(dump, State = #state{ metrics = Metrics, dump_interval = DumpInterval, total_sessions = TotalSessions }) ->
    SessionStarts = dump(State),
    reset_metrics(Metrics, DumpInterval),
    schedule_dump(DumpInterval),
    {noreply, State#state{ last_dump = timestamp(), total_sessions = TotalSessions + SessionStarts }}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

prepare_dir() ->
    CSVDir       = csv_dir(),
    ok           = file:make_dir(CSVDir),
    {ok, WD}     = file:get_cwd(),
    CurrentDir   = "tests/current",
    file:delete(CurrentDir),
    Config       = application:get_all_env(ramjet),
    file:write_file(CSVDir ++ "/test.config", io_lib:fwrite("~p.\n",[Config])),
    ok           = file:make_symlink(WD ++ "/" ++ CSVDir, CurrentDir),
    CSVDir.

csv_dir() ->
    {{Ye, Mo, Da}, {Ho, Mi, Se}} = calendar:gregorian_seconds_to_datetime(timestamp()),
    Dir = lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B", [Ye, Mo, Da, Ho, Mi, Se])),
    "tests/" ++ Dir.

reset_metrics(Metrics, DumpInterval) ->
    [reset_metric(M, DumpInterval) || M <- [ramjet_session_start | Metrics] ].

reset_metric(Metric, DumpInterval) ->
    ensure_delete_on_all_nodes(histogram_name(Metric)),
    ensure_delete_on_all_nodes(counter_name(Metric)),
    ensure_delete_on_all_nodes(error_counter_name(Metric)),
    ramjet:apply_on_all_nodes(folsom_metrics, new_histogram, [histogram_name(Metric), slide, round(DumpInterval / 1000 * 2)]),
    ramjet:apply_on_all_nodes(folsom_metrics, new_counter, [counter_name(Metric)]),
    ramjet:apply_on_all_nodes(folsom_metrics, new_counter, [error_counter_name(Metric)]).

ensure_delete_on_all_nodes(MetricName) ->
    ramjet:apply_on_all_nodes(ramjet_stats, ensure_delete, [MetricName]).

ensure_delete(MetricName) ->
    try folsom_metrics:delete_metric(MetricName)
    catch _:{badmatch, {error, MetricName, nonexistent_metric}} ->
        noop
    end.

histogram_name(Metric) ->
    metric_name(Metric, histogram).

counter_name(Metric) ->
    metric_name(Metric, counter).

error_counter_name(Metric) ->
    metric_name(Metric, error_counter).

metric_name(Metric, Type) ->
    list_to_atom(lists:flatten(io_lib:format("~p_~p", [Metric, Type]))).

schedule_dump(DumpInterval) ->
    erlang:send_after(DumpInterval, self(), dump).

dump(#state{ started_at = StartedAt, last_dump = LastDump, metrics = Metrics, ignore_metrics = IgnoreMetrics, csv_dir = CSVDir }) ->
    MetricsToDump = lists:subtract(Metrics, IgnoreMetrics),
    End = timestamp(),
    Elapsed = float(erlang:convert_time_unit(End - StartedAt, native, second)),
    Window  = float(erlang:convert_time_unit(End - LastDump, native, second)),

    DumpLine =
    fun(Metric) ->
        Count = counter_sum_from_all_nodes(counter_name(Metric)),
        Error = counter_sum_from_all_nodes(error_counter_name(Metric)),
        case Count > 0 of
            true ->
                Histogram   = folsom_metrics:get_histogram_statistics(histogram_name(Metric)),
                Percentiles = proplists:get_value(percentile, Histogram),
                Min         = proplists:get_value(min, Histogram),
                Max         = proplists:get_value(max, Histogram),
                Mean        = proplists:get_value(arithmetic_mean, Histogram),
                Median      = proplists:get_value(median, Histogram),
                Q95         = proplists:get_value(95,  Percentiles),
                Q99         = proplists:get_value(99,  Percentiles),
                Q999        = proplists:get_value(999, Percentiles),
                {
                    Metric, Count, Error,
                    io_lib:format(
                        "~.8f, ~.8f, ~p, ~.8f, ~.8f, ~.8f, ~.8f, ~.8f, ~.8f, ~.8f, ~p\n",
                        [Elapsed, Window, Count, Min, Mean, Median, Q95, Q99, Q999, Max, Error]
                    )
                };
            false ->
                {
                    Metric, 0, Error,
                    io_lib:format(
                        "~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p\n",
                        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, Error]
                    )
                }
        end
    end,

    Lines = lists:map(DumpLine, MetricsToDump),
    {TotalCount, TotalErrors} =
    lists:foldl(
        fun({Metric, C, E, Line}, {CSum, ESum}) ->
              file:write_file(CSVDir ++ "/" ++ atom_to_list(Metric) ++ "_latencies.csv", Line, [append]),
              {CSum + C, ESum + E}
        end,
        {0, 0},
        Lines),
    SummaryLine   = io_lib:format("~.8f, ~.8f, ~p, ~p, ~p\n", [Elapsed, Window, TotalCount, TotalCount - TotalErrors, TotalErrors]),
    file:write_file(CSVDir ++ "/summary.csv", SummaryLine, [append]),
    SessionStarts  = counter_sum_from_all_nodes(counter_name(ramjet_session_start)),
    SessionRunning = lists:sum(ramjet:apply_on_all_nodes(ramjet_session_sup, child_count, [])),
    SessionLine    = io_lib:format("~.8f, ~.8f, ~p, ~p\n", [Elapsed, Window, SessionStarts, SessionRunning]),
    file:write_file(CSVDir ++ "/sessions.csv", SessionLine, [append]),
    SessionStarts.

write_csv_headers(Metrics, CSVDir) ->
    SummaryHeader = "elapsed, window, total, successful, failed\n",
    file:write_file(CSVDir ++ "/summary.csv", SummaryHeader),
    SessionHeader = "elapsed, window, starts, running\n",
    file:write_file(CSVDir ++ "/sessions.csv", SessionHeader),
    StatsHeader   = "elapsed, window, n, min, mean, median, 95th, 99th, 99_9th, max, errors\n",
    [file:write_file(CSVDir ++ "/" ++ atom_to_list(Metric) ++ "_latencies.csv", StatsHeader)
     || Metric <- Metrics].

timestamp() -> erlang:system_time().

counter_sum_from_all_nodes(CounterName) ->
    lists:sum(ramjet:apply_on_all_nodes(folsom_metrics, get_metric_value, [CounterName])).
