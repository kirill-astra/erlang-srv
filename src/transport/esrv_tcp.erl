-module(esrv_tcp).

-behaviour(esrv_transport).
-behaviour(ranch_protocol).

-include("records.hrl").
-include("log.hrl").

%% API
-export([start_loop/2,
         loop/1]).

%% esrv_transport callbacks
-export([start_listener/0,
         send/2]).

%% ranch callbacks
-export([start_link/4]).

-record(connection, {socket :: gen_tcp:socket(),
                     transport :: module()}).

%%%===================================================================
%%% esrv_transport callbacks
%%%===================================================================
-spec start_listener() -> {ok, pid()}.
start_listener() ->
    {ok, Port} = application:get_env(erlang_srv, port),
    TransportOptions = #{num_acceptors => 1, socket_opts => [{port, Port}]},
    ranch:start_listener(erlang_srv, ranch_tcp, TransportOptions, ?MODULE, []).

-spec send(Connection :: #connection{}, Data :: iolist()) -> ok.
send(#connection{socket = Socket, transport = Transport}, Data) ->
    Transport:send(Socket, Data).

%%%===================================================================
%%% ranch_protocol callbacks
%%%===================================================================
-spec start_link(RanchRef :: ranch:ref(),
                 Socket :: any(),
                 Transport :: module(),
                 Options :: any()) -> {ok, pid()}.
start_link(RanchRef, _, Transport, _) ->
    {ok, proc_lib:spawn_link(?MODULE, start_loop, [RanchRef, Transport])}.

%%%===================================================================
%%% Listener loop
%%%===================================================================
-record(state, {connection :: #connection{},
                buffer :: binary()}).

-spec start_loop(RanchRef :: ranch:ref(), Transport :: module()) -> ok.
start_loop(RanchRef, Transport) ->
    {ok, Socket} = ranch:handshake(RanchRef),
    ok = Transport:setopts(Socket, [{active, once}]),
    Connection = #connection{socket = Socket, transport = Transport},
    ok = esrv_client_gw:set_connection(Connection),
    loop(#state{connection = Connection,
                buffer = <<>>}).

-spec loop(State :: #state{}) -> ok.
loop(#state{connection = #connection{socket = Socket, transport = Transport},
            buffer = Buffer0} = State0) ->
    receive
        {tcp, Socket, Block} ->
            ok = Transport:setopts(Socket, [{active, once}]),
            Buffer1 = <<Buffer0/binary, Block/binary>>,
            Buffer2 = parse_messages(Buffer1),
            loop(State0#state{buffer = Buffer2});
        {tcp_closed, Socket} ->
            ?LOG_WARNING("LSP flow interrupted; emulating 'exit' notification"),
            Message = #notification{method = <<"exit">>, params = []},
            ok = esrv_client_gw:message_in(Message);
        Unexpected ->
            ?LOG_CRITICAL("Unexpected message received: ~p", [Unexpected]),
            exit(unexpected)
    end.

-spec parse_messages(Buffer :: binary()) -> NewBuffer :: binary().
parse_messages(Buffer0) ->
    case parse_headers(Buffer0, #{}) of
        {ok, #{<<"content-length">> := ContentLength}, Buffer1} ->
            BytesNumber = binary_to_integer(ContentLength),
            case Buffer1 of
                <<Payload:BytesNumber/binary, Buffer2/binary>> ->
                    Message = esrv_msg_coder:decode(Payload),
                    ok = esrv_client_gw:message_in(Message),
                    parse_messages(Buffer2);
                _ ->
                    Buffer0
            end;
        undefined ->
            Buffer0
    end.

-spec parse_headers(Buffer :: binary(), Acc :: map()) ->
          {ok, Headers :: map(), NewBuffer :: binary()} | undefined.
parse_headers(Buffer0, Acc) ->
    case binary:split(Buffer0, <<"\n">>) of
        [Line, Buffer1] ->
            case string:trim(Line) of
                <<>> ->
                    {ok, Acc, Buffer1};
                _ ->
                    {Name, Value} = parse_header_line(Line),
                    parse_headers(Buffer1, Acc#{Name => Value})
            end;
        _ ->
            undefined
    end.

-spec parse_header_line(Line :: binary()) -> {Name :: binary(), Value :: binary()}.
parse_header_line(Line) ->
    [Name, Value] = binary:split(Line, <<":">>),
    {string:lowercase(string:trim(Name)), string:trim(Value)}.
