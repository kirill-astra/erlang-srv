-module(esrv_edoc_markdown).

-export([format/1]).

-include_lib("xmerl/include/xmerl.hrl").

-type content() :: [#xmlText{} | #xmlElement{} | #xmlPI{} | #xmlComment{} | #xmlDecl{}].
-type wrapped_token() :: {br} | {i, iolist()}.

-spec format(XmlElement :: #xmlElement{}) -> binary().
format(#xmlElement{content = Content}) ->
    WrappedToken = format_content_markdown(Content),
    iolist_to_binary(lists:map(fun({br}) ->
                                       <<"\n">>;
                                  ({i, Text}) ->
                                       Text
                               end, WrappedToken)).

%%%-------------------------------------------------------------------
%%% Markdown formatting
%%%-------------------------------------------------------------------
-record(state, {code_mode :: boolean(),
                inline_mode :: boolean(),
                one_line_mode :: boolean(),
                ol_number :: integer() | undefined}).

-spec format_content_markdown(Content :: content()) -> [wrapped_token()].
format_content_markdown(Content) ->
    Acc0 = [],
    State = #state{code_mode = false,
                   inline_mode = false,
                   one_line_mode = false},
    Acc1 = format_content_markdown(Content, Acc0, State),
    Acc2 = ensure_next_br(Acc1, 1),
    lists:reverse(Acc2).

-spec format_content_markdown(Content :: content(),
                              Acc :: [wrapped_token()],
                              State :: #state{}) -> [wrapped_token()].
format_content_markdown([], Acc, _) ->
    Acc;
format_content_markdown([#xmlText{} = XmlText | T], Acc0, State) ->
    Acc1 = format_xml_text_markdown(XmlText, Acc0, State),
    format_content_markdown(T, Acc1, State);
format_content_markdown([#xmlElement{} = XmlElement | T], Acc0, State) ->
    Acc1 = format_xml_element_markdown(XmlElement, Acc0, State),
    format_content_markdown(T, Acc1, State);
format_content_markdown([_ | T], Acc, State) ->
    format_content_markdown(T, Acc, State).

-spec format_xml_text_markdown(XmlText :: #xmlText{},
                               Acc :: [wrapped_token()],
                               State :: #state{}) -> [wrapped_token()].
format_xml_text_markdown(#xmlText{value = Value0}, Acc0, State) ->
    case edoc_lib:is_space(Value0) of
        true ->
            Acc0;
        false ->
            Value1 = escape_value(Value0, State),
            IsNewLine = case Acc0 of [] -> true; [{br} | _] -> true; _ -> false end,
            lists:foldl(fun("\n", Acc00) ->
                                case Acc00 of [] -> []; _ -> [{br} | Acc00] end;
                           (Token, Acc00) ->
                                [{i, Token} | Acc00]
                        end, Acc0, text_tokens(Value1, IsNewLine, State))
    end.

-spec escape_value(Value :: string(), State :: #state{}) -> string().
escape_value(Value, #state{code_mode = false}) ->
    escape_value(Value);
escape_value(Value, _) ->
    Value.

-spec escape_value(Value :: string()) -> string().
escape_value([]) ->
    [];
escape_value([$` | T]) ->
    [$\\, $\\, $` | escape_value(T)];
escape_value([C | T]) ->
    [C | escape_value(T)].

-spec text_tokens(Value :: string(), IsNewLine :: boolean(), State :: #state{}) -> [string()].
text_tokens(Value, _, #state{code_mode = true}) ->
    Tokens0 = re:split(Value, "(\n)", [{return, list}]),
    lists:filter(fun("") -> false; (Token) -> re:run(Token, "^[\s\t]+$") =:= nomatch end, Tokens0);
text_tokens(Value0, _, #state{one_line_mode = true}) ->
    Value1 = re:replace(Value0, "\n", " ", [{return, list}, global]),
    [re:replace(Value1, "[\s\t]+", "\s", [{return, list}, global])];
text_tokens(Value, IsNewLine, _) ->
    Tokens0 = re:split(Value, "[\s\t]*(\n)[\s\t]*", [{return, list}, trim]),
    Tokens1 = lists:filter(fun(V) ->
                                   V =:= "\n" orelse not edoc_lib:is_space(V)
                           end, Tokens0),
    case lists:dropwhile(fun(Token) -> Token =:= "\n" end, Tokens1) of
        [Token | T] when IsNewLine ->
            [re:replace(Token, "^[\s\t]*", "", [{return, list}]) | T];
        _ ->
            Tokens1
    end.

-spec format_xml_element_markdown(XmlElement :: #xmlElement{},
                                  Acc :: [wrapped_token()],
                                  State :: #state{}) -> [wrapped_token()].
format_xml_element_markdown(#xmlElement{name = Name, content = Content}, Acc0,
                            #state{code_mode = CodeMode,
                                   inline_mode = InlineMode,
                                   ol_number = OlNumber} = State0) ->
    if
        CodeMode ->
            format_content_markdown(Content, Acc0, State0);
        Name =:= h1 ->
            State1 = State0#state{one_line_mode = true},
            Acc1 = format_content_markdown(Content, [{i, "# "} | Acc0], State1),
            ensure_next_br(Acc1, 2);
        Name =:= h2 ->
            State1 = State0#state{one_line_mode = true},
            Acc1 = format_content_markdown(Content, [{i, "##"}], State1),
            ensure_next_br(Acc1, 2);
        Name =:= h3 ->
            State1 = State0#state{one_line_mode = true},
            Acc1 = format_content_markdown(Content, [{i, "### "} | Acc0], State1),
            ensure_next_br(Acc1, 2);
        Name =:= h4 ->
            State1 = State0#state{one_line_mode = true},
            Acc1 = format_content_markdown(Content, [{i, "#### "} | Acc0], State1),
            ensure_next_br(Acc1, 2);
        Name =:= h5 ->
            State1 = State0#state{one_line_mode = true},
            Acc1 = format_content_markdown(Content, [{i, "##### "} | Acc0], State1),
            ensure_next_br(Acc1, 2);
        Name =:= h6 ->
            State1 = State0#state{one_line_mode = true},
            Acc1 = format_content_markdown(Content, [{i, "###### "} | Acc0], State1),
            ensure_next_br(Acc1, 2);
        Name =:= p ->
            Acc1 = ensure_next_br(Acc0, 2),
            Acc2 = format_content_markdown(Content, Acc1, State0),
            ensure_next_br(Acc2, 2);
        Name =:= pre ->
            State1 = State0#state{code_mode = true},
            Acc1 = ensure_next_br(Acc0, 1),
            Acc2 = [{i, "``` erlang"} | Acc1],
            Acc3 = ensure_next_br(Acc2, 2),
            Acc4 = format_content_markdown(Content, [], State1) ++ Acc3,
            Acc5 = ensure_next_br(Acc4, 1),
            Acc6 = [{i, "```"} | Acc5],
            ensure_next_br(Acc6, 2);
        (Name =:= a orelse
         Name =:= b orelse
         Name =:= em orelse
         Name =:= i orelse
         Name =:= s orelse
         Name =:= u orelse
         Name =:= code) andalso InlineMode =:= false ->
            Acc1 =
                case Acc0 of
                    [{i, "``"} | Acc00] ->
                        [{i, " "} | Acc00];
                    _ ->
                        [{i, "``"} | Acc0]
                end,
            Acc2 = format_content_markdown(Content, Acc1, State0#state{inline_mode = true}),
            [{i, "``"} | Acc2];
        Name =:= dl ->
            Acc1 = ensure_next_br(Acc0, 2),
            Acc2 = format_content_markdown(Content, Acc1, State0),
            ensure_next_br(Acc2, 2);
        Name =:= dt ->
            Acc1 = [{i, "* "} | Acc0],
            Acc2 = format_content_markdown(Content, Acc1, State0#state{one_line_mode = true}),
            ensure_next_br(Acc2, 1);
        Name =:= dd ->
            Acc1 = format_content_markdown(Content, Acc0, State0),
            ensure_next_br(Acc1, 2);
        Name =:= ul ->
            Acc1 = ensure_next_br(Acc0, 2),
            Acc2 = format_content_markdown(Content, Acc1, State0),
            ensure_next_br(Acc2, 2);
        Name =:= ol ->
            State1 = State0#state{ol_number = 1},
            Acc1 = ensure_next_br(Acc0, 2),
            Acc2 = format_content_markdown(Content, Acc1, State1),
            ensure_next_br(Acc2, 2);
        Name =:= li ->
            Acc1 = [if
                        is_integer(OlNumber) ->
                            integer_to_list(OlNumber) ++ ". ";
                        true ->
                            {i, "* "}
                    end | Acc0],
            Acc2 = format_content_markdown(Content, Acc1, State0),
            ensure_next_br(Acc2, 2);
        true ->
            format_content_markdown(Content, Acc0, State0)
    end.

-spec ensure_next_br(Acc :: [wrapped_token()], N :: integer())  -> [wrapped_token()].
ensure_next_br([], _) ->
    [];
ensure_next_br(Acc, N) ->
    lists:duplicate(N, {br}) ++ lists:dropwhile(fun({br}) -> true; (_) -> false end, Acc).
