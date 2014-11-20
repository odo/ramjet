-module(ramjet_handler).

-callback init() -> InitState :: any().

-callback handle_task(Event :: tuple(), State :: any()) -> NextState :: any().
