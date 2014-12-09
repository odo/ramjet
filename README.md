![ramjet](../master/doc/sr71.png?raw=true "ramjet")

# ramjet

ramjet is a clustered load testing tool with stateful sessions.

## Motivation

I needed to test a database application using [protocol buffers](https://code.google.com/p/protobuf/) where users register, do some operations and then log off after some time.

## Inspiration

ramjet is inspired by other tools:

* [Tsung](http://tsung.erlang-projects.org/) is a distributed solution with many drivers and supports sessions and recording. Writing custom drivers is not straight forward, though.
* [basho_bench](https://github.com/basho/basho_bench/) is stand alone, comes with many drivers and has very nice graphing functionality, which ramjet heavily borrows from.
* [ponos](https://github.com/klarna/ponos) is a generator for different shapes of load. ramjet uses ponos internally.

## Requirements

* [Erlang/OTP](http://www.erlang.org/), tested with version 17.3 .
* [R](http://www.r-project.org/) for plotting result graphs.

## Installation

```
git clone git@github.com:odo/ramjet.git
cd ramjet
make
```

Note: On FreeBSD, use gmake.

## Running an example test

Run the build in example:

`make start`

After it finishes, plot the result graph:

`make graph`

Now open the [result graph](https://raw.githubusercontent.com/odo/ramjet/master/doc/summary.png)

`open tests/current/summary.png`

## Understanding the configuration

What did we just do? Let's look at the [config file](https://github.com/odo/ramjet/blob/master/config/default.config):
```
[
    {ramjet, [
        { handler, ramjet_example_handler },
        { tasks, [{wait, 5000}, {print_call_count}] },
        { load_fun, make_sawtooth }, % see https://github.com/klarna/ponos
        { load_fun_args, [5, 10] }, % session_starts/second/generator
        { load_generator_count, 10}, % start n generators, hence n * session_starts/second
        { load_duration, 20000 },    % ms
        { stats_interval, 1000 },    % ms
        { slave_hosts, []}
    ]}
].
```

So we ran using a handler named `ramjet_example_handler` with a set of two tasks, `{wait, 5000}` and `{print_call_count}`.
The sessions where spawned using a sawtooth shape ramping up for 5s to 10ops/s. This was done by 10 load generators in parallel.
The total duration of the spawning was 20s and we collected stats every second. No slaves were used for this test.

You can also define repeating tasks as `{NumerOfRepeats, Tasks(s)}`:

`[{2, {hello}}]` generates `[{hello},{hello}]` and

`[{3, [{wait}, {2, {get}}]}]` will give you
`[{wait},{get},{get},{wait},{get},{get},{wait},{get},{get}]`

## Session handler

The handler defines the actual execution of the tasks.

Let's look at a handler [ramjet_example_handler](https://github.com/odo/ramjet/blob/master/src/ramjet_example_handler.erl):

```erlang
-module(ramjet_example_handler).

-export([init/1, handle_task/2, terminate/1]).

-behaviour(ramjet_handler).

init(Id) ->
    io:format("id: ~p\n", [Id]),
    random:seed(os:timestamp()),
    0.

handle_task({wait, Millisecords}, TaskState) ->
    WaitFor = (round(
        Millisecords + (random:uniform() * Millisecords) - Millisecords / 2
    )),
    timer:sleep(WaitFor),
    {ok, TaskState + 1};

handle_task({print_call_count}, TaskState) ->
    io:format("~p: Call count: ~p\n", [self(), TaskState]),
    {ok, TaskState + 1}.

terminate(_State) ->
    noop.
```

Within each session, `init/1` is called with an ID starting at 1 and increasing monotonically (also across slaves).

`init/1` then returns 0 as the initial state, and `handle_task/2` either waits for a while or prints the state, depending on the task.

Note:

* **Tasks** must be tuples with an atom as the first element.
* **handle_task/2** must either return `{ok, NewState}` or `{error, NewState}`.
* when **handle_task/2** crashes, the session dies and will not process any further tasks.

## Results

The first panel in the [result graph](https://raw.githubusercontent.com/odo/ramjet/master/doc/summary.png)
shows the total throughput of all tasks and total errors over time. The second panel shows the rate of sessions spawning and the number of sessions running at any given time.
The panels below show different percentiles of latencies for every type of task.

In the example we see a maximum throughput of 100 ops with no errors, a sawtooth - like spawning and a "damped" shape of the total sessions metric due to the random element in our waiting task.
As expected latency for the `print_call_count` is 0 and for `wait` it's a uniform distribution beween 2.5 s and 7.5 s.

As the latency of the `wait` task is not very interesting and might screw up the scaling of the axes you might ignore it in the config file:

`{ no_stats_for, [wait] },`

## Rolling your own

To run tests with your own handler and config file, you can pass ramjet a set of include paths and the location of your config file:

`make start incl="~/src/my_app/ebin ~/src/my_app/deps/*/ebin" config=~/src/my_app/config/my_app.config`

## Clustering

ramjet can be run in a cluster with one master and several slave nodes.

Start a slave on another machine:

`make slave`

it will start a node ramjet_slave@$HOST where $HOST is what the shell command `hostname` returns.
This name must be resolvabel by the master.

Add all slave hosts to the masters configuration:

```
{ slave_hosts, ['test-host1.local', 'test-host2.local']}
```

When starting the master with `make start` it will connect to the slaves and start the same
program remotely, effectivly multiplying the generated load.

Note: The counters are collected from all nodes (throughput, errros, session starts, sessions running).
Latency information is collected only from the master, so if a slave has higher latency than the master this will not be visible.

