-module(ramjet_handler).

-callback init() -> InitState :: any().

-callback handle_task(Event :: tuple(), State :: any()) -> {ok, NextState :: any()} | {error, NextState :: any()}.

-callback terminate(State :: any()) -> any().
