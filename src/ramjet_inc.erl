-module(ramjet_inc).

-export([start/0, init/0, inc/0]).

inc() ->
    case global:whereis_name(?MODULE) of
      undefined ->
          {error, not_started};
      Pid ->
          Pid ! {inc_req, self()},
          receive
              {inc, Inc} ->
              Inc
          after
              100 ->
                  {error, timeout}
          end
    end.

start() ->
    Pid = spawn(?MODULE, init, []),
    yes = global:register_name(?MODULE, Pid),
    Pid.

init() ->
    loop(0).

loop(Inc) ->
    receive
        {inc_req, From} ->
            IncNew = Inc + 1,
            From ! {inc, IncNew},
            loop(IncNew)
    end.

