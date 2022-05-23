-module(esrv_elvis_diagnostics).

-behaviour(esrv_diagnostics).

%% API
-export([init/1,
         run/3]).

-include("types.hrl").
-include("records.hrl").
-include("diagnostics.hrl").
-include("log.hrl").

-define(SOURCE, <<"elvis">>).

-type problem() :: elvis_result:file() | elvis_result:rule().

-spec init(Options :: map()) -> NewOptions :: map().
init(Options) ->
    ChildSpec = #{id => esrv_elvis_mgr,
                  start => {esrv_elvis_mgr, start_link, []}},
    {ok, _} = esrv_dynamic_sup:start_child(ChildSpec),
    Options.

-spec run(Uri :: uri(), AppId :: app_id() | undefined, Options :: map()) -> [diagnostic()].
run(_, undefined, _) ->
    [];
run(Uri, AppId, Options) ->
    case esrv_lib:get_app_type_and_path(AppId) of
        {ok, AppType, AppPath} when AppType =:= proj orelse AppType =:= sub_proj ->
            do_run(Uri, AppPath, Options);
        _ ->
            []
    end.

-spec do_run(Uri :: uri(), AppPath :: path(), Options :: map()) -> [diagnostic()].
do_run(Uri, AppPath, Options) ->
    case find_config(AppPath, Options) of
        {ok, Hash, ConfigPath} ->
            case esrv_elvis_mgr:ensure_config(Hash, ConfigPath) of
                {ok, Config} ->
                    Problems = analyse(Uri, AppPath, Config),
                    diagnostics(Problems);
                {error, Error} ->
                    [#diagnostic{position = {line, 1},
                                 severity = ?DIAGNOSTIC_ERROR,
                                 source = ?SOURCE,
                                 message = Error}]
            end;
        undefined ->
            []
    end.

-spec find_config(AppPath :: path(), Options :: map()) -> {ok, hash(), path()} | undefined.
find_config(AppPath, Options) ->
    AppConfig = filename:join(AppPath, "elvis.config"),
    DefaultConfig = filename:join(esrv_lib:bdir_user_config(), "elvis.config"),
    case maps:find(<<"config">>, Options) of
        {ok, ElvisConfig} ->
            do_find_config([AppConfig, ElvisConfig, DefaultConfig]);
        error ->
            do_find_config([AppConfig, DefaultConfig])
    end.

-spec do_find_config(Candidates :: [path()]) -> {ok, hash(), path()} | undefined.
do_find_config([]) ->
    undefined;
do_find_config([Candidate | T]) ->
    case file:read_file(Candidate) of
        {ok, Content} ->
            {ok, esrv_lib:hash(Content), Candidate};
        {error, _} ->
            do_find_config(T)
    end.

-spec analyse(Uri :: uri(), AppPath :: path(), Config :: elvis_config:configs()) -> [problem()].
analyse(Uri, AppPath, Config) ->
    Path = esrv_lib:uri_to_path(Uri),
    Filename = rel_filename(Path, AppPath),
    try
        {fail, Fail} = elvis_core:rock_this(Filename, Config),
        Fail
    catch
        _:_ ->
            []
    end.

-spec rel_filename(AbsPath :: path(), DirPath :: path()) -> file:filename().
rel_filename(AbsPath, DirPath) ->
    AbsPathComponents = filename:split(AbsPath),
    DirPathComponents = filename:split(DirPath),
    RelPathComponents = drop_prefix(AbsPathComponents, DirPathComponents),
    RelPath = filename:join(RelPathComponents),
    esrv_lib:path_to_file(RelPath).

-spec drop_prefix(Components :: [binary()], Prefix :: [binary()]) -> [binary()].
drop_prefix([Component | ComponentsT], [Component | PrefixT]) ->
    drop_prefix(ComponentsT, PrefixT);
drop_prefix(ComponentsLeft, _) ->
    ComponentsLeft.

-spec diagnostics(Problems :: [problem()]) -> [diagnostic()].
diagnostics([]) ->
    [];
diagnostics([#{rules := Rules} | T]) ->
    diagnostics(Rules ++ T);
diagnostics([#{name := Name, items := Items} | T]) ->
    lists:foldl(fun(Item, Acc) ->
                        {Line, Message} = parse_item(Name, Item),
                        [#diagnostic{position = {line, Line},
                                     severity = ?DIAGNOSTIC_INFO,
                                     source = ?SOURCE,
                                     message = Message} | Acc]
                end, [], Items) ++ diagnostics(T);
diagnostics([#{error_msg := ErrorMsg, info := Info} | T]) ->
    Message = iolist_to_binary(io_lib:format(ErrorMsg, Info)),
    [#diagnostic{position = {line, 1},
                 severity = ?DIAGNOSTIC_INFO,
                 source = ?SOURCE,
                 message = Message} | diagnostics(T)].

-spec parse_item(Name :: atom(), Item :: elvis_result:item()) -> {line(), binary()}.
parse_item(Name, #{message := Message, info := Info} = Item0) ->
    case Message of
        "Line ~p is too long: ~s." ->
            [LineNum, Line] = Info,
            Item1 = Item0#{message := "Line ~p is too long: ~p.", info := [LineNum, length(Line)]},
            parse_item(Name, Item1);
        "Atom ~p on line ~p does not respect "
        "the format defined by the regular expression '~p'." ->
            [_, LineNum, _] = Info,
            Item1 = Item0#{line_num := LineNum},
            do_parse_item(Name, Item1);
        "The macro named ~p on line ~p does not respect "
        "the format defined by the regular expression '~p'." ->
            [_, LineNum, _] = Info,
            Item1 = Item0#{line_num := LineNum},
            do_parse_item(Name, Item1);
        _ ->
            do_parse_item(Name, Item0)
    end.

-spec do_parse_item(Name :: atom(), Item :: elvis_result:item()) -> {line(), binary()}.
do_parse_item(Name, #{message := Message, line_num := LineNum, info := Info}) ->
    {LineNum, iolist_to_binary(io_lib:format(Message ++ " (~p)", Info ++ [Name]))}.
