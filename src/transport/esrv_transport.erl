-module(esrv_transport).

-callback start_listener() -> {ok, pid()}.
-callback send(connection(), iolist()) -> ok.

-type connection() :: any().

-export_type([connection/0]).
