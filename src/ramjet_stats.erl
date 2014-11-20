-module(ramjet_stats).

-behaviour(gen_server).
-export([record/3]).
-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(COMPRESSEVERY, 5).

-record(state, {
    started_at    :: number(),
    dump_interval :: number(),
    last_reset    :: tuple(),
    metrics       :: list(),
    histograms    :: map(),
    counts        :: map(),
    errors        :: map()
}).

%% Public API

record(Metric, error, Server) ->
    gen_server:cast(Server, {record, Metric, error, os:timestamp()});
record(Metric, Value, Server) when is_number(Value) ->
    gen_server:cast(Server, {record, Metric, Value, os:timestamp()}).

start_link(Metrics, DumpInterval) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Metrics, DumpInterval], []).

%% Callbacks

init([Metrics, DumpInterval]) ->
    self() ! maybe_dump,
    write_csv_headers(Metrics),
    {ok, reset(#state{ metrics = Metrics, started_at = os:timestamp(), dump_interval = DumpInterval })}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({record, Metric, error, Timestamp}, State = #state{ errors = Errors, counts = Counts }) ->
    ErrorCount = maps:get(Metric, Errors),
    NewErrors  = maps:put(Metric, ErrorCount + 1, Errors),
    Count      = maps:get(Metric, Counts),
    NewCounts  = maps:put(Metric, Count + 1, Counts),
    {noreply, maybe_dump(State#state{ errors = NewErrors, counts = NewCounts }, Timestamp)};

handle_cast({record, Metric, Value, Timestamp}, State = #state{ counts = Counts, histograms = Histograms }) when is_number(Value) ->
    Count         = maps:get(Metric, Counts),
    NewCounts     = maps:put(Metric, Count + 1, Counts),
    Histogram     = maps:get(Metric, Histograms),
    NewHistogram  = quantile_estimator:insert(Value, Histogram),
    CompressedHistogram =
    case (Count rem ?COMPRESSEVERY) =:= 0 of
        true ->
            quantile_estimator:compress(NewHistogram);
        false ->
            NewHistogram
    end,
    NewHistograms = maps:put(Metric, CompressedHistogram, Histograms),
    {noreply, maybe_dump(State#state{ counts = NewCounts, histograms = NewHistograms }, Timestamp)}.


handle_info(maybe_dump, State = #state{ dump_interval = DumpInterval }) ->
    Delay = round(DumpInterval / 10),
    erlang:send_after(Delay, self(), maybe_dump),
    {noreply, maybe_dump(State, os:timestamp())}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

maybe_dump(State = #state{ last_reset = LastReset, dump_interval = DumpInterval }, Timestamp) ->
    case timer:now_diff(Timestamp, LastReset) >= DumpInterval * 1000 of
        true ->
            dump(State),
            reset(State);
        false ->
            State
    end.

dump(#state{ started_at = StartedAt, last_reset = LastReset, metrics = Metrics, histograms = Histograms, counts = Counts, errors = Errors }) ->
    Now = os:timestamp(),
    Elapsed = timer:now_diff(Now, StartedAt) / 1000 / 1000,
    Window  = timer:now_diff(Now, LastReset) / 1000 / 1000,

    DumpLine =
    fun(Metric) ->
        Count     = maps:get(Metric, Counts),
        case Count > 0 of
            true ->
                Error     = maps:get(Metric, Errors),
                Histogram = maps:get(Metric, Histograms),
                Min       = quantile(0.0, Histogram),
                io:format("Error ~p\n", [{Metric, Error}]),
                Median    = quantile(0.5, Histogram),
                Mean      = Median,
                Q95       = quantile(0.95, Histogram),
                Q99       = quantile(0.99, Histogram),
                Q999      = quantile(0.999, Histogram),
                Max       = quantile(1.00, Histogram),
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
              file:write_file("runs/" ++ atom_to_list(Metric) ++ "_latencies.csv", Line, [append]),
              {CSum + C, ESum + E}
        end,
        {0, 0},
        Lines),
    SummaryLine = io_lib:format("~.8f, ~.8f, ~p, ~p, ~p\n", [Elapsed, Window, TotalCount, TotalCount - TotalErrors, TotalErrors]),
    file:write_file("runs/summary.csv", SummaryLine, [append]).

reset(State = #state{ metrics = Metrics }) ->
    State#state{
      last_reset = os:timestamp(),
      counts     = new_counts(Metrics),
      histograms = new_histograms(Metrics),
      errors     = new_errors(Metrics)
    }.

new_errors(Metrics) ->
    new_map(Metrics, 0).

new_counts(Metrics) ->
    new_map(Metrics, 0).

new_histograms(Metrics) ->
    IV = quantile_estimator:f_targeted([{0.5, 0.0002}, {0.95, 0.0002}, {0.99, 0.0002}, {0.999, 0.0002}]),
    new_map(Metrics, quantile_estimator:new(IV)).

new_map(Metrics, Value) ->
    lists:foldl(
        fun(Metric, Map) -> maps:put(Metric, Value, Map) end,
        maps:new(),
        Metrics
).

write_csv_headers(Metrics) ->
    SummaryHeader = "elapsed, window, total, successful, failed\n",
    file:write_file("runs/summary.csv", SummaryHeader),
    StatsHeader   = "elapsed, window, n, min, mean, median, 95th, 99th, 99_9th, max, errors\n",
    [file:write_file("runs/" ++ atom_to_list(Metric) ++ "_latencies.csv", StatsHeader)
     || Metric <- Metrics].

quantile(Quantile, Histogram) ->
    try quantile_estimator:quantile(Quantile, Histogram) of
        Value ->
            Value
    catch
        {error, empty_stats} ->
            0.0
    end.

