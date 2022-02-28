-module(esrv_stdio).

-behaviour(esrv_transport).

-include("records.hrl").
-include("log.hrl").

%% API
-export([start_loop/0,
         loop/1]).

%% esrv_transport callbacks
-export([start_listener/0,
         send/2]).

%%%===================================================================
%%% esrv_transport callbacks
%%%===================================================================
-spec start_listener() -> {ok, pid()}.
start_listener() ->
    {ok, proc_lib:spawn_link(?MODULE, start_loop, [])}.

-spec send(IoDevice :: io:device(), Data :: iolist()) -> ok.
send(IoDevice, Data) ->
    file:write(IoDevice, Data).

%%%===================================================================
%%% Listener loop
%%%===================================================================
-spec start_loop() -> ok.
start_loop() ->
    IoDevice = erlang:group_leader(),
    ok = io:setopts(IoDevice, [binary]),
    ok = esrv_client_gw:set_connection(IoDevice),
    loop(IoDevice).

-spec loop(io:device()) -> ok.
loop(IoDevice) ->
    case read_headers(IoDevice, #{}) of
        {ok, #{<<"content-length">> := ContentLength}} ->
            BytesNumber = binary_to_integer(ContentLength),
            {ok, Payload} = file:read(IoDevice, BytesNumber),
            Message = esrv_msg_coder:decode(Payload),
            ok = esrv_client_gw:message_in(Message),
            ?MODULE:loop(IoDevice);
        eof ->
            ?LOG_WARNING("LSP flow interrupted; emulating 'exit' notification"),
            Message = #notification{method = <<"exit">>, params = []},
            ok = esrv_client_gw:message_in(Message)
    end.

-spec read_headers(IoDevice :: io:device(), Acc :: map()) ->
          {ok, Headers :: map()} | eof.
read_headers(IoDevice, Acc) ->
    case file:read_line(IoDevice) of
        {ok, <<"\n">>} ->
            {ok, Acc};
        {ok, Line} ->
            {Name, Value} = parse_header_line(Line),
            read_headers(IoDevice, Acc#{Name => Value});
        eof ->
            eof
    end.

-spec parse_header_line(Line :: binary()) -> {Name :: binary(), Value :: binary()}.
parse_header_line(Line) ->
    [Name, Value] = binary:split(Line, <<":">>),
    {string:lowercase(string:trim(Name)), string:trim(Value)}.
