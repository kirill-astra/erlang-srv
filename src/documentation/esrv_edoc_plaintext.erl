-module(esrv_edoc_plaintext).

-export([format/1]).

-include_lib("xmerl/include/xmerl.hrl").

-type content() :: [#xmlText{} | #xmlElement{} | #xmlPI{} | #xmlComment{} | #xmlDecl{}].
-type wrapped_token() :: {br} | {i, iolist()}.

-spec format(XmlElement :: #xmlElement{}) -> binary().
format(#xmlElement{content = Content}) ->
    WrappedToken0 = format_content(Content),
    WrappedToken1 = ensure_end_br(WrappedToken0, 1),
    iolist_to_binary(lists:map(fun({br}) ->
                                       <<"\n">>;
                                  ({i, Text}) ->
                                       Text
                               end, WrappedToken1)).

-spec format_content(Content :: content()) -> [wrapped_token()].
format_content([]) ->
    [];
format_content([#xmlText{} = XmlText | T]) ->
    format_xml_text(XmlText) ++ format_content(T);
format_content([#xmlElement{} = XmlElement | T]) ->
    format_xml_element_plaintext(XmlElement) ++ format_content(T);
format_content([_ | T]) ->
    format_content(T).

-spec format_xml_text(XmlText :: #xmlText{}) -> [wrapped_token()].
format_xml_text(#xmlText{parents = Parents, value = Value}) ->
    case edoc_lib:is_space(Value) of
        true ->
            [];
        false ->
            Tokens =
                case lists:any(fun({pre, _}) -> true; (_) -> false end, Parents) of
                    true ->
                        re:split(Value, "(\n)", [{return, list}]);
                    false ->
                        lists:filter(fun(V) ->
                                             V =:= "\n" orelse not edoc_lib:is_space(V)
                                     end, re:split(Value, "\s*(\n)\s*", [trim, {return, list}]))
                end,
            lists:map(fun("\n") ->
                              {br};
                         (Token) ->
                              {i, Token}
                      end, Tokens)
    end.

-spec format_xml_element_plaintext(XmlElement :: #xmlElement{}) -> [wrapped_token()].
format_xml_element_plaintext(#xmlElement{name = Name, parents = Parents, content = Content}) ->
    WrappedToken0 = format_content(Content),
    if
        Name =:= h1 ->
            ensure_end_br([{br}, {i, "# "} | WrappedToken0], 2);
        Name =:= h2 ->
            ensure_end_br([{br}, {i, "## "} | WrappedToken0], 2);
        Name =:= h3 ->
            ensure_end_br([{br}, {i, "### "} | WrappedToken0], 2);
        Name =:= h4 ->
            ensure_end_br([{br}, {i, "#### "} | WrappedToken0], 2);
        Name =:= h5 ->
            ensure_end_br([{br}, {i, "##### "} | WrappedToken0], 2);
        Name =:= h6 ->
            ensure_end_br([{br}, {i, "###### "} | WrappedToken0], 2);
        Name =:= p ->
            WrappedToken1 = drop_while_br(WrappedToken0),
            ensure_end_br(WrappedToken1, 2);
        Name =:= pre ->
            ensure_end_br(WrappedToken0, 2);
        Name =:= dl ->
            ensure_end_br(WrappedToken0, 2);
        Name =:= dt ->
            WrappedToken1 = drop_while_br(WrappedToken0),
            WrappedToken2 = add_prefix(fun(_) -> "  " end, WrappedToken1),
            ensure_end_br(WrappedToken2, 2);
        Name =:= dd ->
            WrappedToken1 = drop_while_br(WrappedToken0),
            WrappedToken2 = add_prefix(fun(_) -> "      " end, WrappedToken1),
            ensure_end_br(WrappedToken2, 2);
        Name =:= ul orelse
        Name =:= ol ->
            ensure_end_br(WrappedToken0, 2);
        Name =:= li ->
            case Parents of
                [{ul, _} | _] ->
                    WrappedToken1 = add_prefix(fun(1) -> "  - "; (_) -> "    " end, WrappedToken0),
                    ensure_end_br(WrappedToken1, 1);
                [{ol, _} | _] ->
                    WrappedToken1 =
                        add_prefix(fun(LineNumber) ->
                                           io_lib:format("  ~b. ", [LineNumber])
                                   end, WrappedToken0),
                    ensure_end_br(WrappedToken1, 1);
                _ ->
                    []
            end;
        true ->
            WrappedToken0
    end.

-spec drop_while_br(WrappedTokens :: [wrapped_token()]) -> [wrapped_token()].
drop_while_br(WrappedToken) ->
    lists:dropwhile(fun({br}) -> true; (_) -> false end, WrappedToken).

-spec ensure_end_br(WrappedTokens :: [wrapped_token()], N :: integer()) -> [wrapped_token()].
ensure_end_br(WrappedTokens0, N) ->
    WrappedTokens1 =
        lists:dropwhile(fun(WrappedToken) ->
                                WrappedToken =:= {br}
                        end, lists:reverse(WrappedTokens0)),
    WrappedTokens2 = lists:duplicate(N, {br}) ++ WrappedTokens1,
    lists:reverse(WrappedTokens2).

-spec add_prefix(PrefixFun :: fun((integer()) -> string()),
                 WrappedTokens :: [wrapped_token()]) -> [wrapped_token()].
add_prefix(PrefixFun, [{i, Text} | T]) ->
    [{i, PrefixFun(1)}, {i, Text} | add_prefix2(2, PrefixFun, T)];
add_prefix(_, []) ->
    [].

-spec add_prefix2(LineNumber :: integer(),
                  PrefixFun :: fun((integer()) -> string()),
                  WrappedTokens :: [wrapped_token()]) -> [wrapped_token()].
add_prefix2(_, _, []) ->
    [];
add_prefix2(LineNumber, PrefixFun, [{br}, {i, Text} | T]) ->
    [{br}, {i, PrefixFun(LineNumber)}, {i, Text} | add_prefix2(LineNumber + 1, PrefixFun, T)];
add_prefix2(LineNumber, PrefixFun, [WrappedToken | T]) ->
    [WrappedToken | add_prefix2(LineNumber, PrefixFun, T)].
