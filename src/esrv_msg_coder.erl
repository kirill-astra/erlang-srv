-module(esrv_msg_coder).

-include("protocol.hrl").
-include("records.hrl").
-include("log.hrl").

%% API
-export([decode/1,
         encode/1]).

-export([encode_json_term/1]).

%%%===================================================================
%%% Decode
%%%===================================================================
-spec decode(Payload :: binary()) -> message().
decode(Payload) ->
    try
        JsonTerm = jsx:decode(Payload, [return_maps]),
        case JsonTerm of
            #{<<"id">> := Id, <<"method">> := Method} ->
                #request{id = Id,
                         method = Method,
                         params = maps:get(<<"params">>, JsonTerm, undefined)};
            #{<<"id">> := Id} ->
                #response{id = Id,
                          result = maps:get(<<"result">>, JsonTerm, undefined),
                          error = get_value(<<"error">>, JsonTerm, fun decode_error/1, undefined)};
            #{<<"method">> := Method} ->
                #notification{method = Method,
                              params = maps:get(<<"params">>, JsonTerm, undefined)}
        end
    catch
        T:E:S ->
            ?LOG_ERROR("Parse message exception!~n"
                       "type: ~p; error: ~p; stacktrace: ~p~n"
                       "~s", [T, E, S, Payload]),
            esrv_lib:gentle_exit(1)
    end.

-spec decode_error(JsonTerm :: jsx:json_term()) -> response_error().
decode_error(#{<<"code">> := Code, <<"message">> := Message} = JsonTerm) ->
    #response_error{code = Code,
                    message = Message,
                    data = maps:get(<<"data">>, JsonTerm, undefined)}.

-spec get_value(Key :: binary(),
                JsonTerm :: jsx:json_term(),
                Parser :: fun((jsx:json_term()) -> any()),
                Default :: any()) -> any().
get_value(Key, JsonTerm, Parser, _) when is_map_key(Key, JsonTerm) ->
    Parser(maps:get(Key, JsonTerm));
get_value(_, _, _, Default) ->
    Default.

%%%===================================================================
%%% Encode
%%%===================================================================
-spec encode(Message :: message()) -> iolist().
encode(Message) ->
    JsonTerm =
        [{<<"jsonrpc">>, ?JSON_RPC} |
         case Message of
             #request{id = Id, method = Method, params = Params} ->
                 [{<<"id">>, Id}, {<<"method">>, Method}, {<<"params">>, Params}];
             #response{id = Id, result = Result, error = Error} ->
                 [{<<"id">>, Id},
                  {<<"result">>, Result},
                  {<<"error">>, form_value(Error, fun encode_error/1, undefined)}];
             #notification{method = Method, params = Params} ->
                 [{<<"method">>, Method},
                  {<<"params">>, Params}]
         end],
    Payload = encode_json_term(JsonTerm),
    [io_lib:format(<<"Content-Length: ~p\r\n">>, [byte_size(Payload)]),
     <<"\r\n">>,
     Payload].

-spec encode_error(ResponseError :: response_error()) -> jsx:json_term().
encode_error(#response_error{code = Code,
                             message = Message,
                             data = Data}) ->
    [{<<"code">>, Code},
     {<<"message">>, Message},
     {<<"data">>, Data}].

-spec form_value(Value :: any() | undefined,
                 Former :: fun((any()) -> jsx:json_term()),
                 Default :: any()) -> jsx:json_term().
form_value(undefined, _, Default) ->
    Default;
form_value(Value, Former, _) ->
    Former(Value).

-spec encode_json_term(JsonTerm :: jsx:json_term()) -> jsx:json_text().
encode_json_term(JsonTerm) ->
    Stripped = strip_undefined(JsonTerm),
    jsx:encode(Stripped).

-spec strip_undefined(JsonTerm :: jsx:json_term()) -> jsx:json_term().
strip_undefined(JsonTerm) when is_list(JsonTerm) ->
    lists:filtermap(fun({_, undefined}) ->
                            false;
                       ({Key, Value}) ->
                            {true, {Key, strip_undefined(Value)}};
                       (Another) ->
                            {true, strip_undefined(Another)}
                    end, JsonTerm);
strip_undefined(JsonTerm) ->
    JsonTerm.
