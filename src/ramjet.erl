-module(ramjet).

-export([start/0, config/1]).

start() ->
    application:start(ponos),
    application:start(ramjet).

config(Key) ->
    proplists:get_value(
      Key,
      application:get_all_env(ramjet)
    ).
