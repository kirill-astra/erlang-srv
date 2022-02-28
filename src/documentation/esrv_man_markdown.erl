-module(esrv_man_markdown).

-behaviour(esrv_man).

-export([init/0,
         merge_line/3,
         finalize/2]).

-include("parser.hrl").

-define(TEXT_WIDTH, 70).

-type line_prefix() :: binary().

-type tagged_paragraph() :: wait_for_tag | {tag, binary()}.

-record(state, {tagged_paragraph :: tagged_paragraph() | undefined,
                code_mode :: boolean()}).

-spec init() -> #state{}.
init() ->
    #state{code_mode = false}.

-spec merge_line(Line :: binary(),
                 Acc :: binary(),
                 State :: #state{}) -> {binary(), #state{}}.
merge_line(<<".nf", _/binary>>, Acc0, #state{code_mode = false} = State0) ->
    Acc1 = esrv_man_lib:ensure_new_line(Acc0),
    {<<Acc1/binary, "``` erlang">>, State0#state{code_mode = true}};
merge_line(<<".nf", _/binary>>, Acc, #state{code_mode = true} = State) ->
    {Acc, State};

merge_line(<<".fi", _/binary>>, Acc0, #state{code_mode = true} = State0) ->
    Acc1 = esrv_man_lib:ensure_new_line(Acc0),
    {<<Acc1/binary, "```", $\n>>, State0#state{code_mode = false}};
merge_line(<<".fi", _/binary>>, Acc, #state{code_mode = false} = State) ->
    {Acc, State};

merge_line(Line0, Acc0, #state{code_mode = true} = State) ->
    case esrv_man_lib:check_ignore_list(Line0) of
        undefined ->
            Acc1 = esrv_man_lib:filter_code_line(Acc0),
            Line1 = re:replace(Line0, <<"[\s\t]+$">>, <<"">>, [{return, binary}]),
            {<<Acc1/binary, $\n, Line1/binary>>, State};
        _ ->
            {Acc0, State}
    end;

merge_line(<<".LP", _/binary>>, Acc0, State) ->
    {esrv_man_lib:ensure_new_line(Acc0), State};
merge_line(<<".P", T/binary>>, Acc, State) ->
    merge_line(<<".LP", T/binary>>, Acc, State);
merge_line(<<".PP", T/binary>>, Acc, State) ->
    merge_line(<<".LP", T/binary>>, Acc, State);

merge_line(<<".RS", _/binary>>, Acc, State) ->
    {Acc, State};
merge_line(<<".RE", _/binary>>, Acc, State) ->
    {Acc, State};

merge_line(Line0, Acc0, State0) ->
    case check_tagged_paragraph(Line0, State0) of
        {take, LinePrefix, State1} ->
            Line1 = format_line(Line0),
            {add_line(LinePrefix, Line1, Acc0), State1};
        {skip, State1} ->
            {esrv_man_lib:ensure_new_line(Acc0), State1}
    end.

-spec check_tagged_paragraph(Line :: binary(), State :: #state{}) ->
          {take, line_prefix(), #state{}} | {skip, #state{}}.
check_tagged_paragraph(Line, #state{tagged_paragraph = TaggedParagraph} = State0) ->
    case TaggedParagraph of
        undefined ->
            case Line of
                <<".TP", _/binary>> ->
                    {skip, State0#state{tagged_paragraph = wait_for_tag}};
                _ ->
                    {take, <<>>, State0}
            end;
        wait_for_tag ->
            case format_line(Line) of
                Tag when Tag =/= empty ->
                    {skip, State0#state{tagged_paragraph = {tag, Tag}}};
                empty ->
                    {skip, State0}
            end;
        {tag, Tag} ->
            State1 = State0#state{tagged_paragraph = undefined},
            LinePrefix =
                case Tag of
                    <<"*", _/binary>> ->
                        <<"* ">>;
                    _ ->
                        <<"* ", Tag/binary, $\n>>
                end,
            {take, LinePrefix, State1}
    end.

-spec add_line(LinePrefix :: line_prefix(),
               Line :: binary() | empty,
               Acc :: binary()) -> binary().
add_line(_, empty, Acc) ->
    Acc;
add_line(LinePrefix, Line, Acc0) ->
    Acc1 = case Acc0 of <<>> -> <<>>; _ -> <<Acc0/binary, "\n">> end,
    <<Acc1/binary, LinePrefix/binary, Line/binary>>.

-spec format_line(Line :: binary()) -> binary() | empty.
format_line(Line0)  ->
    Line1 = format_line(Line0, false, <<>>, <<>>, []),
    case Line1 of <<>> -> empty; _ -> Line1 end.

-spec format_line(Line :: binary(),
                  SelectionMode :: boolean(),
                  WordAcc :: binary(),
                  LineAcc :: binary(),
                  LinesAcc :: [binary()]) -> binary().
format_line(<<>>, _, <<>>, <<>>, LinesAcc) ->
    iolist_to_binary(lists:join("\n", lists:reverse(LinesAcc)));
format_line(<<>>, SelectionMode, <<>>, LineAcc, LinesAcc) ->
    format_line(<<>>, SelectionMode, <<>>, <<>>, [LineAcc | LinesAcc]);
format_line(<<>>, SelectionMode, WordAcc, LineAcc, LinesAcc)
  when byte_size(WordAcc) + byte_size(LineAcc) > ?TEXT_WIDTH ->
    format_line(<<>>, SelectionMode, <<>>, WordAcc, [LineAcc | LinesAcc]);
format_line(<<>>, SelectionMode, WordAcc, LineAcc, LinesAcc) ->
    format_line(<<>>, SelectionMode, <<>>, esrv_man_lib:join_words(LineAcc, WordAcc), LinesAcc);
format_line(<<$ , T/binary>>, SelectionMode, <<>>, LineAcc, LinesAcc) ->
    format_line(T, SelectionMode, <<>>, LineAcc, LinesAcc);
format_line(<<$ , T/binary>>, SelectionMode,  WordAcc, LineAcc, LinesAcc)
  when byte_size(WordAcc) + byte_size(LineAcc) > ?TEXT_WIDTH ->
    format_line(T, SelectionMode, <<>>, WordAcc, [LineAcc | LinesAcc]);
format_line(<<$ , T/binary>>, SelectionMode, WordAcc, LineAcc, LinesAcc) ->
    format_line(T, SelectionMode, <<>>, esrv_man_lib:join_words(LineAcc, WordAcc), LinesAcc);
format_line(<<$\\, $&, $., T/binary>>, SelectionMode, WordAcc, LineAcc, LinesAcc) ->
    format_line(T, SelectionMode, <<WordAcc/binary, $.>>, LineAcc, LinesAcc);
format_line(<<$\\, $&, T/binary>>, SelectionMode, WordAcc, LineAcc, LinesAcc) ->
    format_line(T, SelectionMode, WordAcc, LineAcc, LinesAcc);
format_line(<<$\\, $f, C, T/binary>>, false, <<>>, LineAcc, LinesAcc)
  when C =:= $I orelse C =:= $B ->
    format_line(T, true, <<"``">>, LineAcc, LinesAcc);
format_line(<<$\\, $f, C, T/binary>>, false, WordAcc0, LineAcc, LinesAcc)
  when C =:= $I orelse C =:= $B ->
    case re:run(WordAcc0, <<"^(.*)``$">>, [{capture, all_but_first, binary}]) of
        {match, [WordAcc1]} ->
            format_line(T, true, WordAcc1, LineAcc, LinesAcc);
        nomatch ->
            format_line(T, true, <<WordAcc0/binary, "``">>, LineAcc, LinesAcc)
    end;
format_line(<<$\\, $f, $R, T/binary>>, true, WordAcc, LineAcc, LinesAcc) ->
    format_line(T, false, <<WordAcc/binary, "``">>, LineAcc, LinesAcc);
format_line(<<$\\, $f, C, T/binary>>, SelectionMode, WordAcc, LineAcc, LinesAcc)
  when C =:= $I orelse C =:= $B orelse C =:= $R ->
    format_line(T, SelectionMode, WordAcc, LineAcc, LinesAcc);
format_line(<<$`, T/binary>>, SelectionMode, WordAcc, LineAcc, LinesAcc) ->
    format_line(T, SelectionMode, <<WordAcc/binary, "\\\\`">>, LineAcc, LinesAcc);
format_line(LineLeft0, SelectionMode, WordAcc, LineAcc, LinesAcc) ->
    case esrv_man_lib:check_ignore_list(LineLeft0) of
        {ok, LineLeft1} ->
            format_line(LineLeft1, SelectionMode, WordAcc, LineAcc, LinesAcc);
        undefined ->
            <<C, T/binary>> = LineLeft0,
            format_line(T, SelectionMode, <<WordAcc/binary, C>>, LineAcc, LinesAcc)
    end.

-spec finalize(Acc :: binary(), State :: #state{}) -> binary().
finalize(Acc0, #state{code_mode = true} = State) ->
    case re:run(Acc0, <<"``` erlang[\n\s\t]*$">>) of
        {match, [{From, _}]} ->
            binary:part(Acc0, 0, From - 1);
        nomatch ->
            {Acc1, _} = merge_line(<<".fi">>, Acc0, State),
            Acc1
    end;
finalize(Acc, _) ->
    Acc.
