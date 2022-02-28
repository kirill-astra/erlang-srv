-module(esrv_man_lib).

-export([join_words/2,
         ensure_new_line/1,
         make_prefix/1,
         check_ignore_list/1,
         filter_code_line/1]).

-spec join_words(Words :: binary(), Word :: binary()) -> binary().
join_words(Words, Word) ->
    case re:replace(Words, <<"^ *">>, <<>>, [{return, binary}]) of
        <<>> ->
            <<Words/binary, Word/binary>>;
        _ ->
            <<Words/binary, " ", Word/binary>>
    end.

-spec ensure_new_line(Data :: binary()) -> binary().
ensure_new_line(<<>>) ->
    <<>>;
ensure_new_line(Data) ->
    case binary:last(Data) of
        $\n ->
            Data;
        _ ->
            <<Data/binary, "\n">>
    end.

-spec make_prefix(Indent :: non_neg_integer()) -> binary().
make_prefix(Indent) ->
    iolist_to_binary(lists:duplicate(Indent, " ")).

-spec check_ignore_list(Line :: binary()) -> {ok, binary()} | undefined.
check_ignore_list(<<".SB", T/binary>>) -> {ok, T};
check_ignore_list(<<".sb", T/binary>>) -> {ok, T};
check_ignore_list(<<".SM", T/binary>>) -> {ok, T};
check_ignore_list(<<".RB", T/binary>>) -> {ok, T};
check_ignore_list(<<".rb", T/binary>>) -> {ok, T};
check_ignore_list(<<".RI", T/binary>>) -> {ok, T};
check_ignore_list(<<".ri", T/binary>>) -> {ok, T};
check_ignore_list(<<".BR", T/binary>>) -> {ok, T};
check_ignore_list(<<".br", T/binary>>) -> {ok, T};
check_ignore_list(<<".BI", T/binary>>) -> {ok, T};
check_ignore_list(<<".bi", T/binary>>) -> {ok, T};
check_ignore_list(<<".B", T/binary>>) -> {ok, T};
check_ignore_list(<<".b", T/binary>>) -> {ok, T};
check_ignore_list(<<".IR", T/binary>>) -> {ok, T};
check_ignore_list(<<".ir", T/binary>>) -> {ok, T};
check_ignore_list(<<".IB", T/binary>>) -> {ok, T};
check_ignore_list(<<".ib", T/binary>>) -> {ok, T};
check_ignore_list(<<".I", T/binary>>) -> {ok, T};
check_ignore_list(<<".i", T/binary>>) -> {ok, T};
check_ignore_list(_) -> undefined.

-spec filter_code_line(Line :: binary()) -> binary().
filter_code_line(Line) ->
    filter_code_line(Line, <<>>).

-spec filter_code_line(Line :: binary(), Acc :: binary()) -> binary().
filter_code_line(<<>>, Acc) ->
    Acc;
filter_code_line(<<$\\, $&, T/binary>>, Acc) ->
    filter_code_line(T, Acc);
filter_code_line(<<$\\, $f, C, T/binary>>, Acc)
  when C =:= $I orelse C =:= $B orelse C =:= $R ->
    filter_code_line(T, Acc);
filter_code_line(<<C, T/binary>>, Acc) ->
    filter_code_line(T, <<Acc/binary, C>>).
