-module(esrv_zone_parser).

-include("types.hrl").
-include("parser.hrl").

%% API
-export([zone_begin/1,
         zone_end/2]).

%%%===================================================================
%%% API
%%%===================================================================
-spec zone_begin(Tokens :: [token()]) ->
          {ok, opened_zone(), [zone()], token(), [token()]} | undefined.
zone_begin(Tokens) ->
    case Tokens of
        [{'-', _}, {atom, _, Atom}, {'(', _} = StartToken | T]
          when Atom =:= behavior orelse Atom =:= behaviour  ->
            {ok, opened_zone(behavior, StartToken), [], StartToken, T};
        [{'-', _}, {atom, _, Atom}, {'(', _}, {'[', _} = StartToken | T]
          when Atom =:= export orelse Atom =:= export_type orelse Atom =:= optional_callbacks ->
            {ok, opened_zone(Atom, StartToken), [], StartToken, T};
        [{'-', _}, {atom, _, import},
         {'(', _}, {atom, _, Module}, {',', _},
         {'[', _} = StartToken | T] ->
            {ok, opened_zone({import, Module}, StartToken), [], StartToken, T};
        [{'-', _}, {atom, _, type}, {atom, _, _}, {'(', _} | _] ->
            case lists:dropwhile(fun({'::', _}) -> false; (_) -> true end, Tokens) of
                [{'::', _} = StartToken | T] ->
                    {ok, opened_zone(type, StartToken), [], StartToken, T};
                _ ->
                    undefined
            end;
        [{'-', _}, {atom, _, record} = StartToken, {'(', _},
         {atom, _, _}, {',', _}, {'{', _} = BoundToken | T] ->
            Zones = [zone(record_name, StartToken, BoundToken)],
            {ok, opened_zone(record_body, BoundToken), Zones, StartToken, T};
        [{'-', _}, {atom, _, record} = StartToken | T] ->
            {ok, opened_zone(record_name, StartToken), [], StartToken, T};
        [{'-', _}, {atom, _, callback},
         {atom, _, _}, {'(', _} = BoundToken | T] ->
            {ok, opened_zone(spec_body, BoundToken), [], BoundToken, T};
        [{'-', _} = StartToken, {atom, _, spec},
         {atom, _, _}, {'(', _} = BoundToken | T] ->
            Zones = [zone(spec_name, StartToken, BoundToken)],
            {ok, opened_zone(spec_body, BoundToken), Zones, BoundToken, T};
        [{'-', _}, {atom, _, spec} = StartToken | T] ->
            {ok, opened_zone(spec_name, StartToken), [], StartToken, T};
        _ ->
            undefined
    end.

-spec opened_zone(ZoneType :: zone_type(),
                  StartToken :: erl_scan:token()) -> opened_zone().
opened_zone(ZoneType, StartToken) ->
    #opened_zone{type = ZoneType,
                 start_location = esrv_parser_lib:get_token_location(StartToken)}.

-spec zone(ZoneType :: zone_type(),
           StartToken :: erl_scan:token(),
           EndToken :: erl_scan:token()) -> zone().
zone(ZoneType, StartToken, EndToken) ->
    #zone{type = ZoneType,
          start_location = esrv_parser_lib:get_token_location(StartToken),
          end_location = esrv_parser_lib:get_token_location(EndToken)}.

-spec zone_end(OpenedZone :: opened_zone(), Tokens :: [token()]) -> NewZone :: zone().
zone_end(#opened_zone{type = Type, start_location = StartLocation}, Tokens) ->
    EndLocation =
        case Tokens of
            [{dot, _}, {')', _} = Token | _] when Type =:= behavior ->
                esrv_parser_lib:get_token_location(Token);
            [{')', _} = Token | _] when Type =:= behavior ->
                esrv_parser_lib:get_token_location(Token);
            [{dot, _}, {')', _}, {']', _} = Token | _] when Type =:= export orelse
                                                            Type =:= export_type orelse
                                                            Type =:= optional_callbacks ->
                esrv_parser_lib:get_token_location(Token);
            [{')', _}, {']', _} = Token | _] when Type =:= export orelse
                                                  Type =:= export_type orelse
                                                  Type =:= optional_callbacks ->
                esrv_parser_lib:get_token_location(Token);
            [{']', _} = Token | _] when Type =:= export orelse
                                        Type =:= export_type orelse
                                        Type =:= optional_callbacks ->
                esrv_parser_lib:get_token_location(Token);
            [{dot, _}, {')', _}, {']', _} = Token | _] when is_tuple(Type) andalso
                                                            element(1, Type) =:= import ->
                esrv_parser_lib:get_token_location(Token);
            [{')', _}, {']', _} = Token | _] when is_tuple(Type) andalso
                                                  element(1, Type) =:= import ->
                esrv_parser_lib:get_token_location(Token);
            [{']', _} = Token | _] when is_tuple(Type) andalso
                                        element(1, Type) =:= import ->
                esrv_parser_lib:get_token_location(Token);
            [{dot, _}, {')', _}, {'}', _} = Token | _] when Type =:= record_body ->
                esrv_parser_lib:get_token_location(Token);
            [{')', _}, {'}', _} = Token | _] when Type =:= record_body ->
                esrv_parser_lib:get_token_location(Token);
            [{'}', _} = Token | _] when Type =:= record_body ->
                esrv_parser_lib:get_token_location(Token);
            [{dot, _}, {')', _} = Token | _] when Type =:= spec_body ->
                esrv_parser_lib:get_token_location(Token);
            [{')', _} = Token | _] when Type =:= spec_body ->
                esrv_parser_lib:get_token_location(Token);
            [{dot, _} = Token | _] ->
                esrv_parser_lib:get_token_location(Token);
            [Token | _] ->
                esrv_parser_lib:get_after_token_location(Token)
        end,
    #zone{type = Type,
          start_location = StartLocation,
          end_location = EndLocation}.
