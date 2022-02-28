-module(esrv_man_plaintext).

-behaviour(esrv_man).

-export([init/0,
         merge_line/3,
         finalize/2]).

-define(TEXT_WIDTH, 70).
-define(DEFAULT_INDENT, 4).

-type line_prefix() :: {indent, binary()} |
                       {tag, binary()}.

-type tagged_paragraph() :: {indent, non_neg_integer()} |
                            {indent_and_tag, non_neg_integer(), binary()}.

-record(state, {indent :: non_neg_integer(),
                indent_stack :: [non_neg_integer()],
                extra_indent :: non_neg_integer(),
                prefix_stash :: line_prefix() | undefined,
                tagged_paragraph :: tagged_paragraph() | undefined,
                code_mode :: boolean()}).

-spec init() -> #state{}.
init() ->
    #state{indent = 0,
           indent_stack = [],
           extra_indent = 0,
           code_mode = false}.

-spec merge_line(Line :: binary(),
                 Acc :: binary(),
                 State :: #state{}) -> {binary(), #state{}}.
merge_line(<<".nf", _/binary>>, Acc0, #state{code_mode = false} = State0) ->
    {esrv_man_lib:ensure_new_line(Acc0), State0#state{code_mode = true}};
merge_line(<<".nf", _/binary>>, Acc, #state{code_mode = true} = State) ->
    {Acc, State};

merge_line(<<".fi", _/binary>>, Acc0, #state{code_mode = true} = State0) ->
    {esrv_man_lib:ensure_new_line(Acc0), State0#state{code_mode = false}};
merge_line(<<".fi", _/binary>>, Acc, #state{code_mode = false} = State) ->
    {Acc, State};

merge_line(Line, Acc0, #state{code_mode = true} = State) ->
    case esrv_man_lib:check_ignore_list(Line) of
        undefined ->
            Acc1 = esrv_man_lib:filter_code_line(Acc0),
            {<<Acc1/binary, $\n, Line/binary>>, State};
        _ ->
            {Acc0, State}
    end;

merge_line(<<".LP", _/binary>>, Acc0, State) ->
    {esrv_man_lib:ensure_new_line(Acc0), State};
merge_line(<<".P", T/binary>>, Acc, State) ->
    merge_line(<<".LP", T/binary>>, Acc, State);
merge_line(<<".PP", T/binary>>, Acc, State) ->
    merge_line(<<".LP", T/binary>>, Acc, State);

merge_line(<<".RS", T/binary>>, Acc, #state{indent = Indent0,
                                            indent_stack = IndentStack0} = State0) ->
    Indent =
        case re:run(T, <<"^ *(-?[0-9]+)">>, [{capture, all_but_first, binary}]) of
            {match, [Match]} ->
                binary_to_integer(Match);
            nomatch ->
                ?DEFAULT_INDENT
        end,
    IndentShift = erlang:max(-1 * Indent0, Indent),
    {Acc, State0#state{indent = Indent0 + IndentShift,
                       indent_stack = [IndentShift | IndentStack0]}};

merge_line(<<".RE", _/binary>>, Acc, #state{indent = Indent0,
                                            indent_stack = IndentStack0} = State0) ->
    case IndentStack0 of
        [IndentShift | IndentStack1] ->
            {Acc, State0#state{indent = Indent0 - IndentShift,
                               indent_stack = IndentStack1}};
        [] ->
            {Acc, State0}
    end;

merge_line(Line0, Acc0, State0) ->
    case check_tagged_paragraph(Line0, State0) of
        {take, LinePrefix, #state{indent = Indent,
                                  extra_indent = ExtraIndent} = State1} ->
            Line1 = format_line(Line0, Indent + ExtraIndent),
            add_line(LinePrefix, Line1, Acc0, State1);
        {skip, State1} ->
            {esrv_man_lib:ensure_new_line(Acc0), State1}
    end.

-spec check_tagged_paragraph(Line :: binary(), State :: #state{}) ->
          {take, line_prefix(), #state{}} | {skip, #state{}}.
check_tagged_paragraph(Line, #state{indent = Indent,
                                    extra_indent = ExtraIndent,
                                    prefix_stash = PrefixStash,
                                    tagged_paragraph = TaggedParagraph} = State0) ->
    case TaggedParagraph of
        undefined ->
            case Line of
                <<".TP", T/binary>> ->
                    TagIndent =
                        case re:run(T, <<"^ *([0-9]+)">>, [{capture, all_but_first, binary}]) of
                            {match, [Match]} ->
                                binary_to_integer(Match);
                            nomatch ->
                                ?DEFAULT_INDENT
                        end,
                    {skip, State0#state{tagged_paragraph = {indent, TagIndent}}};
                _ when PrefixStash =/= undefined ->
                    State1 = State0#state{prefix_stash = undefined},
                    {take, PrefixStash, State1};
                _ ->
                    {take, {indent, esrv_man_lib:make_prefix(Indent + ExtraIndent)}, State0}
            end;
        {indent, TagIndent} ->
            case format_line(Line, 0) of
                Tag when Tag =/= empty ->
                    {skip, State0#state{tagged_paragraph = {indent_and_tag, TagIndent, Tag}}};
                empty ->
                    {skip, State0}
            end;
        {indent_and_tag, TagIndent, Tag} ->
            State1 = State0#state{extra_indent = TagIndent, tagged_paragraph = undefined},
            Prefix =
                if
                    TagIndent > byte_size(Tag) ->
                        [esrv_man_lib:make_prefix(Indent), Tag, " "];
                    true ->
                        [esrv_man_lib:make_prefix(Indent),
                         Tag, $\n,
                         esrv_man_lib:make_prefix(Indent + TagIndent)]
                end,
            {take, {tag, iolist_to_binary(Prefix)}, State1}
    end.

-spec add_line(LinePrefix :: line_prefix(),
               Line :: binary() | empty,
               Acc :: binary(),
               State :: #state{}) -> {binary(), #state{}}.
add_line({indent, _}, empty, Acc, State) ->
    {Acc, State};
add_line({tag, Prefix}, empty, Acc, State0) ->
    {Acc, State0#state{prefix_stash = {tag, Prefix}}};
add_line({_, Prefix}, Line, Acc0, State0) ->
    Acc1 = case Acc0 of <<>> -> <<>>; _ -> <<Acc0/binary, "\n">> end,
    {<<Acc1/binary, Prefix/binary, Line/binary>>, State0#state{extra_indent = 0}}.

-spec format_line(Line :: binary(), Indent :: non_neg_integer()) -> binary() | empty.
format_line(Line0, Indent)  ->
    Prefix = esrv_man_lib:make_prefix(Indent),
    case format_line(Line0, Prefix, <<>>, <<>>, []) of
        <<>> ->
            empty;
        Line ->
            Line
    end.

-spec format_line(Line :: binary(),
                  Prefix :: binary(),
                  WordAcc :: binary(),
                  LineAcc :: binary(),
                  LinesAcc :: [binary()]) -> binary().
format_line(<<>>,  _, <<>>, <<>>, LinesAcc) ->
    iolist_to_binary(lists:join("\n", lists:reverse(LinesAcc)));
format_line(<<>>, Prefix, <<>>, LineAcc, LinesAcc) ->
    format_line(<<>>, Prefix, <<>>, <<>>, [LineAcc | LinesAcc]);
format_line(<<>>, Prefix, WordAcc, LineAcc, LinesAcc)
  when byte_size(WordAcc) + byte_size(LineAcc) > ?TEXT_WIDTH ->
    format_line(<<>>, Prefix, <<>>, <<Prefix/binary, WordAcc/binary>>, [LineAcc | LinesAcc]);
format_line(<<>>, Prefix, WordAcc, LineAcc, LinesAcc) ->
    format_line(<<>>, Prefix, <<>>, esrv_man_lib:join_words(LineAcc, WordAcc), LinesAcc);
format_line(<<$ , T/binary>>, Prefix, <<>>, LineAcc, LinesAcc) ->
    format_line(T, Prefix, <<>>, LineAcc, LinesAcc);
format_line(<<$ , T/binary>>, Prefix, WordAcc, LineAcc, LinesAcc)
  when byte_size(WordAcc) + byte_size(LineAcc) > ?TEXT_WIDTH ->
    format_line(T, Prefix, <<>>, <<Prefix/binary, WordAcc/binary>>, [LineAcc | LinesAcc]);
format_line(<<$ , T/binary>>, Prefix, WordAcc, LineAcc, LinesAcc) ->
    format_line(T, Prefix, <<>>, esrv_man_lib:join_words(LineAcc, WordAcc), LinesAcc);
format_line(<<$\\, $&, T/binary>>, Prefix, WordAcc, LineAcc, LinesAcc) ->
    format_line(T, Prefix, WordAcc, LineAcc, LinesAcc);
format_line(<<$\\, $f, _, T/binary>>, Prefix, WordAcc, LineAcc, LinesAcc) ->
    format_line(T, Prefix, WordAcc, LineAcc, LinesAcc);
format_line(LineLeft0, Prefix, WordAcc, LineAcc, LinesAcc) ->
    case esrv_man_lib:check_ignore_list(LineLeft0) of
        {ok, LineLeft1} ->
            format_line(LineLeft1, Prefix, WordAcc, LineAcc, LinesAcc);
        undefined ->
            <<C, T/binary>> = LineLeft0,
            format_line(T, Prefix, <<WordAcc/binary, C>>, LineAcc, LinesAcc)
    end.

-spec finalize(Acc :: binary(), State :: #state{}) -> binary().
finalize(Acc, _) ->
    Acc.
