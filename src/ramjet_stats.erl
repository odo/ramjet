-module(ramjet_stats).

-behaviour(gen_server).
-export([record/2]).
-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-compile({no_auto_import,[now/0]}).

-define(COMPRESSEVERY, 5).

-record(state, {
    started_at    :: number(),
    dump_interval :: number(),
    last_dump     :: tuple(),
    metrics       :: list(),
    csv_dir       :: list()
}).

%% Public API

record(Metric, error) ->
    folsom_metrics:notify({counter_name(Metric), {inc, 1}}),
    folsom_metrics:notify({error_counter_name(Metric), {inc, 1}});

record(Metric, Duration) when is_number(Duration) ->
    folsom_metrics:notify({histogram_name(Metric), Duration}),
    folsom_metrics:notify({counter_name(Metric), {inc, 1}}).

start_link(Metrics, DumpInterval) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Metrics, DumpInterval], []).

%% Callbacks

init([Metrics, DumpInterval]) ->
    schedule_dump(DumpInterval),
    reset_metrics(Metrics, DumpInterval),
    CSVDir = prepare_dir(),
    write_csv_headers(Metrics, CSVDir),
    {ok, #state{ metrics = Metrics, started_at = now(), dump_interval = DumpInterval, last_dump = now(), csv_dir = CSVDir }}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(dump, State = #state{ metrics = Metrics, dump_interval = DumpInterval }) ->
    dump(State),
    reset_metrics(Metrics, DumpInterval),
    schedule_dump(DumpInterval),
    {noreply, State#state{ last_dump = now() }}.

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
    {ok, Config} = init:get_argument(config),
    {ok, _}      = file:copy(Config, CSVDir ++ "/test.config"),
    ok           = file:make_symlink(WD ++ "/" ++ CSVDir, CurrentDir),
    CSVDir.

csv_dir() ->
    {{Ye, Mo, Da}, {Ho, Mi, Se}} = calendar:now_to_local_time(now()),
    Dir = lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B", [Ye, Mo, Da, Ho, Mi, Se])),
    "tests/" ++ Dir.

reset_metrics(Metrics, DumpInterval) ->
    [reset_metric(M, DumpInterval) || M <- Metrics].

reset_metric(Metric, DumpInterval) ->
    ensure_delete(histogram_name(Metric)),
    ensure_delete(counter_name(Metric)),
    ensure_delete(error_counter_name(Metric)),
    folsom_metrics:new_histogram(histogram_name(Metric), slide, round(DumpInterval / 1000 * 2)),
    folsom_metrics:new_counter(counter_name(Metric)),
    folsom_metrics:new_counter(error_counter_name(Metric)).

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

dump(#state{ started_at = StartedAt, last_dump = LastDump, metrics = Metrics, csv_dir = CSVDir }) ->
    Now = now(),
    Elapsed = timer:now_diff(Now, StartedAt) / 1000 / 1000,
    Window  = timer:now_diff(Now, LastDump) / 1000 / 1000,

    DumpLine =
    fun(Metric) ->
        Count = folsom_metrics:get_metric_value(counter_name(Metric)),
        case Count > 0 of
            true ->
                Error       = folsom_metrics:get_metric_value(error_counter_name(Metric)),
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
                    Metric, 0, 0,
                    io_lib:format(
                        "~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p\n",
                        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
                    )
                }
        end
    end,

    Lines = lists:map(DumpLine, Metrics),
    {TotalCount, TotalErrors} =
    lists:foldl(
        fun({Metric, C, E, Line}, {CSum, ESum}) ->
              file:write_file(CSVDir ++ "/" ++ atom_to_list(Metric) ++ "_latencies.csv", Line, [append]),
              {CSum + C, ESum + E}
        end,
        {0, 0},
        Lines),
    SummaryLine = io_lib:format("~.8f, ~.8f, ~p, ~p, ~p\n", [Elapsed, Window, TotalCount, TotalCount - TotalErrors, TotalErrors]),
    file:write_file(CSVDir ++ "/summary.csv", SummaryLine, [append]).

write_csv_headers(Metrics, CSVDir) ->
    SummaryHeader = "elapsed, window, total, successful, failed\n",
    file:write_file(CSVDir ++ "/summary.csv", SummaryHeader),
    StatsHeader   = "elapsed, window, n, min, mean, median, 95th, 99th, 99_9th, max, errors\n",
    [file:write_file(CSVDir ++ "/" ++ atom_to_list(Metric) ++ "_latencies.csv", StatsHeader)
     || Metric <- Metrics].

now() ->
    os:timestamp().
