-module(esrv_lib).

%% File system related utils
-export([bdir_user_cache/0,
         bdir_user_config/0,
         bdir_user_log/0,
         uri_to_path/1,
         path_to_uri/1,
         uri_to_file/1,
         file_to_uri/1,
         path_to_file/1,
         file_to_path/1,
         patterns_to_dirs/1,
         pattern_to_dirs/1,
         dirs_to_files/1,
         dirs_to_files/2,
         dirs_to_files/3,
         dir_to_files/1,
         dir_to_files/2,
         dir_to_files/3,
         otp_path/0,
         otp_path/1,
         otp_app_paths/1,
         deps_paths/1,
         deps_app_paths/1,
         flatten_path/1,
         recursive_dirs/1,
         rm_rf/1]).

%% LSP utils
-export([version/0,
         get_client_capability/1,
         format_range/1,
         format_range/2]).

%% Parsed data utils
-export([get_module_data/1,
         get_current_content/1,
         fetch_text_document_lines/3,
         fetch_text_document_form/2,
         find_included_uri/2,
         find_included_uri/4,
         find_included_lib_uri/1,
         app_path_and_module_type/1,
         get_escript_src/1]).

%% Misc
-export([hash/1,
         includes/2,
         defines/0,
         parse_term/2,
         is_binary_prefix/2,
         substitute_group_leader/1,
         gentle_exit/1]).

-include_lib("kernel/include/file.hrl").

-include("types.hrl").
-include("records.hrl").
-include("parser.hrl").

-define(BDIR_PREFIX, <<"erlang_srv">>).
-define(EXTENSIONS, [<<".erl">>, <<".hrl">>, <<".escript">>]).

%%%===================================================================
%%% File system related utils
%%%===================================================================
-spec bdir_user_cache() -> path().
bdir_user_cache() ->
    filename:basedir(user_cache, ?BDIR_PREFIX).

-spec bdir_user_config() -> path().
bdir_user_config() ->
    filename:basedir(user_config, ?BDIR_PREFIX).

-spec bdir_user_log() -> path().
bdir_user_log() ->
    filename:basedir(user_log, ?BDIR_PREFIX).

-spec uri_to_path(Uri :: uri()) -> path().
uri_to_path(Uri0) ->
    Uri1 = http_uri:decode(Uri0),
    #{path := Path} = uri_string:parse(Uri1),
    Path.

-spec path_to_uri(Path :: path()) -> uri().
path_to_uri(Path) ->
    UriMap =
        case os:type() of
            {unix, _} ->
                #{scheme => <<"file">>, host => <<>>, path => Path};
            {win32, _} ->
                case filename:split(Path) of
                    [Head, Host | Tail] when Head =:= <<"//">> orelse Head =:= <<"\\\\">> ->
                        #{scheme => <<"file">>,
                          host => Host,
                          path => [<<"/">> | lists:join(<<"/">>, Tail)]};
                    _ ->
                        #{scheme => <<"file">>, host => <<>>, path => Path}
                end
        end,
    URIString = uri_string:recompose(UriMap),
    iolist_to_binary(URIString).

-spec uri_to_file(Uri :: uri()) -> file:filename().
uri_to_file(Uri) ->
    Path = uri_to_path(Uri),
    path_to_file(Path).

-spec file_to_uri(File :: file:filename()) -> uri().
file_to_uri(File) ->
    Path = file_to_path(File),
    path_to_uri(Path).

-spec path_to_file(Path :: path()) -> file:filename().
path_to_file(File) ->
    binary_to_list(File).

-spec file_to_path(File :: file:filename()) -> path().
file_to_path(File) ->
    list_to_binary(File).

-spec patterns_to_dirs(Patterns :: [pattern()]) -> [path()].
patterns_to_dirs(Patterns) ->
    lists:foldl(fun(Pattern, Acc) ->
                        pattern_to_dirs(Pattern, Acc)
                end, [], Patterns).

-spec pattern_to_dirs(Pattern :: pattern()) -> [path()].
pattern_to_dirs(Pattern) ->
    pattern_to_dirs(Pattern, []).

-spec pattern_to_dirs(Pattern :: pattern(), Acc :: [path()]) -> [path()].
pattern_to_dirs(Pattern, Acc0) ->
    lists:foldl(fun(Candidate, Acc00) ->
                        case file:read_link_info(Candidate) of
                            {ok, #file_info{type = directory}} ->
                                [Candidate | Acc00];
                            _ ->
                                Acc00
                        end
                end, Acc0, wildcard_paths(Pattern)).

-spec dirs_to_files(Dirs :: [path()]) -> [path()].
dirs_to_files(Dirs) ->
    dirs_to_files(Dirs, []).

-spec dirs_to_files(Dirs :: [path()], SkipDirs :: [path()]) -> [path()].
dirs_to_files(Dirs, SkipDirs) ->
    dirs_to_files(Dirs, SkipDirs, ?EXTENSIONS).

-spec dirs_to_files(Dirs :: [path()], SkipDirs :: [path()], Extensions :: [binary()]) -> [path()].
dirs_to_files(Dirs, SkipDirs, Extensions) ->
    lists:foldl(fun(Dir, Acc) ->
                        dir_to_files(Dir, SkipDirs, Extensions, Acc)
                end, [], Dirs).

-spec dir_to_files(Dir :: path()) -> [path()].
dir_to_files(Dir) ->
    dir_to_files(Dir, []).

-spec dir_to_files(Dir :: path(), SkipDirs :: [path()]) -> [path()].
dir_to_files(Dir, SkipDirs) ->
    dir_to_files(Dir, SkipDirs, ?EXTENSIONS).

-spec dir_to_files(Dir :: path(), SkipDirs :: [path()], Extensions :: [binary()]) -> [path()].
dir_to_files(Dir, SkipDirs, Extensions) ->
    dir_to_files(Dir, SkipDirs, Extensions, []).

-spec dir_to_files(Dir :: path(),
                   SkipDirs :: [path()],
                   Extensions :: [binary()],
                   Acc :: [path()]) -> [path()].
dir_to_files(Dir, SkipDirs, Extensions, Acc0) ->
    lists:foldl(fun(Candidate, Acc00) ->
                        case file:read_link_info(Candidate) of
                            {ok, #file_info{type = directory}} ->
                                case lists:member(Candidate, SkipDirs) of
                                    false ->
                                        dir_to_files(Candidate, SkipDirs, Extensions, Acc00);
                                    true ->
                                        Acc00
                                end;
                            {ok, #file_info{type = regular}} ->
                                Extension = filename:extension(Candidate),
                                case lists:member(Extension, Extensions) of
                                    true ->
                                        [Candidate | Acc00];
                                    false ->
                                        Acc00
                                end;
                            _ ->
                                Acc00
                        end
                end, Acc0, wildcard_paths([Dir, "*"])).

-spec otp_path() -> path().
otp_path() ->
    RootDir = code:root_dir(),
    list_to_binary(RootDir).

-spec otp_path(ProjPath :: path()) -> path().
otp_path(ProjPath) ->
    OtpPath = otp_path(),
    absolute_path(ProjPath, OtpPath).

-spec otp_app_paths(ProjPath :: path()) -> [path()].
otp_app_paths(ProjPath) ->
    OtpPath = otp_path(ProjPath),
    lists:filter(fun(AppPath) ->
                         lists:all(fun(Exclude) ->
                                           RegExp = filename:join([OtpPath, "lib", Exclude]),
                                           re:run(AppPath, RegExp) =:= nomatch
                                   end, esrv_config:get_value(otp_apps_exclude, []))
                 end, pattern_to_dirs([OtpPath, <<"lib">>, "*"])).

-spec deps_paths(ProjPath :: path()) -> [path()].
deps_paths(ProjPath) ->
    DepsPaths0 =
        lists:foldr(fun(DepsDir, Acc) ->
                            DepsPath = absolute_path(ProjPath, DepsDir),
                            case filelib:is_dir(DepsPath) of
                                true ->
                                    [esrv_lib:flatten_path(DepsPath) | Acc];
                                false ->
                                    Acc
                            end
                    end, [], esrv_config:get_value(deps_dirs, [])),
    lists:filter(fun filelib:is_dir/1, DepsPaths0).

-spec deps_app_paths(ProjPath :: path()) -> [path()].
deps_app_paths(ProjPath) ->
    DepsPaths = deps_paths(ProjPath),
    ProjName = filename:basename(ProjPath),
    deps_app_paths(DepsPaths, [binary_to_list(ProjName)], [], []).

-spec deps_app_paths(DepsPaths :: [path()],
                     Exclude :: [string()],
                     Acc :: [path()],
                     Uniq :: [binary()]) -> [path()].
deps_app_paths([], _, Acc, _) ->
    Acc;
deps_app_paths([DepsPath | T], Exclude, Acc0, Uniq0) ->
    {ok, Deps} = file:list_dir(DepsPath),
    {Acc1, Uniq1} =
        lists:foldl(fun(Dep, {Acc00, Uniq00}) ->
                            DepPath = filename:join(DepsPath, Dep),
                            case lists:member(Dep, Uniq00) of
                                false ->
                                    {[DepPath | Acc00], [Dep | Uniq00]};
                                true ->
                                    {Acc00, Uniq00}
                            end
                    end, {Acc0, Uniq0}, Deps -- Exclude),
    deps_app_paths(T, Exclude, Acc1, Uniq1).

-spec absolute_path(ProjPath :: path(), Path :: path()) -> path().
absolute_path(ProjPath, Path0) ->
    Path1 =
        case Path0 of
            <<"/", _/binary>> ->
                Path0;
            _ ->
                filename:join(ProjPath, Path0)
        end,
    flatten_path(Path1).

-spec flatten_path(Path :: path()) -> NewPath :: path().
flatten_path(Path) ->
    Elements0 = filename:split(Path),
    Elements1 = flatten_path(Elements0, []),
    filename:join(Elements1).

-spec flatten_path(Elements :: [binary()], Acc :: [binary()]) -> [binary()].
flatten_path([], Acc) ->
    lists:reverse(Acc);
flatten_path([<<".">> | T], Acc) ->
    flatten_path(T, Acc);
flatten_path([<<"..">> | T], Acc) ->
    flatten_path(T, tl(Acc));
flatten_path([Element | T], Acc) ->
    flatten_path(T, [Element | Acc]).

-spec recursive_dirs(Dir :: path()) -> [path()].
recursive_dirs(Dir) ->
    recursive_dirs(Dir, []).

-spec recursive_dirs(Dir :: path(), Acc :: [path()]) -> [path()].
recursive_dirs(Dir, Acc0) ->
    lists:foldl(fun(Candidate, Acc00) ->
                        case file:read_link_info(Candidate) of
                            {ok, #file_info{type = directory}} ->
                                [Candidate | recursive_dirs(Candidate, Acc00)];
                            _ ->
                                Acc00
                        end
                end, Acc0, wildcard_paths([Dir, "*"])).

-spec rm_rf(Target :: file:filename_all()) -> ok.
rm_rf(Target) ->
    case file:read_link_info(Target) of
        {ok, #file_info{type = directory}} ->
            lists:foreach(fun rm_rf/1, wildcard_paths([Target, "*"])),
            ok = file:del_dir(Target);
        {ok, #file_info{type = Type}} when Type =:= regular orelse Type =:= symlink ->
            ok = file:delete(Target);
        _ ->
            ok
    end.

-spec wildcard_paths(Pattern :: pattern()) -> [path()].
wildcard_paths(Pattern) ->
    Wildcard =
        case filename:join(Pattern) of
            Joined when is_binary(Joined) ->
                binary_to_list(Joined);
            Joined ->
                Joined
        end,
    Filenames = filelib:wildcard(Wildcard),
    lists:map(fun file_to_path/1, Filenames).

%%%===================================================================
%%% LSP utils
%%%===================================================================
-spec version() -> binary().
version() ->
    {ok, Vsn} = application:get_key(erlang_srv, vsn),
    list_to_binary(Vsn).

-spec get_client_capability(Target :: [binary()]) -> jsx:json_term() | undefined.
get_client_capability(Target) ->
    {ok, Capabilities} = esrv_config:get_value(client_capabilities),
    get_client_capability(Target, Capabilities).

-spec get_client_capability(Target :: [binary()], JsonTerm :: jsx:json_term()) ->
          jsx:json_term() | undefined.
get_client_capability([], JsonTerm) ->
    JsonTerm;
get_client_capability([H | T], JsonTerm) ->
    if
        is_map(JsonTerm) andalso is_map_key(H, JsonTerm) ->
            get_client_capability(T, maps:get(H, JsonTerm));
        true ->
            undefined
    end.

-spec format_range(LocationRange :: location_range()) -> jsx:json_term().
format_range({StartLocation, EndLocation}) ->
    format_range(StartLocation, EndLocation).

-spec format_range(StartLocation :: location(), EndLocation :: location()) -> jsx:json_term().
format_range({StartLine, StartColumn}, {EndLine, EndColumn}) ->
    [{<<"start">>, [{<<"line">>, StartLine - 1}, {<<"character">>, StartColumn - 1}]},
     {<<"end">>, [{<<"line">>, EndLine - 1}, {<<"character">>, EndColumn - 1}]}].

%%%===================================================================
%%% Parsed data utils
%%%===================================================================
-spec get_module_data(Uri :: uri()) -> {ok, module_data()} | undefined.
get_module_data(Uri) ->
    case esrv_index_mgr:get_current_module_data(Uri) of
        {ok, VolatileModuleData} ->
            {ok, VolatileModuleData};
        undefined ->
            esrv_db:get_module_data(Uri)
    end.

-spec get_current_content(Uri :: uri()) -> {ok, binary()} | undefined.
get_current_content(Uri) ->
    case esrv_index_mgr:get_current_content(Uri) of
        {ok, VolatileContent} ->
            {ok, VolatileContent};
        undefined ->
            Filename = uri_to_file(Uri),
            case file:read_file(Filename) of
                {ok, PersistentContent} ->
                    {ok, PersistentContent};
                _ ->
                    undefined
            end
    end.

-spec fetch_text_document_lines(Uri :: uri(), StartLine :: line(), EndLine :: line()) -> [binary()].
fetch_text_document_lines(Uri, StartLine, EndLine) ->
    {ok, Content0} = get_current_content(Uri),
    Content1 = goto_line(Content0, StartLine),
    take_lines(Content1, EndLine - StartLine + 1).

-spec fetch_text_document_form(Uri :: uri(), Line :: line()) -> binary().
fetch_text_document_form(Uri, Line) ->
    {ok, Content0} = get_current_content(Uri),
    Content1 = goto_line(Content0, Line),
    case erl_scan:tokens([], binary_to_list(Content1), {1, 1}, []) of
        {done, {ok, _, {EndLine, _}}, _} ->
            Lines = take_lines(Content1, EndLine - 1),
            iolist_to_binary(lists:join(<<"\n">>, Lines));
        _ ->
            <<>>
    end.

-spec goto_line(Content :: binary(), Line :: line()) -> binary().
goto_line(Content, 1) ->
    Content;
goto_line(Content0, Line) ->
    [_, Content1] = binary:split(Content0, <<"\n">>),
    goto_line(Content1, Line - 1).

-spec take_lines(Content :: binary(), LinesLeft :: integer()) -> [binary()].
take_lines(_, 0) ->
    [];
take_lines(Content0, LinesLeft) ->
    case binary:split(Content0, <<"\n">>) of
        [Line, Content1] ->
            [Line | take_lines(Content1, LinesLeft - 1)];
        [Line] ->
            [Line]
    end.

-spec find_included_uri(Uri :: uri(), Include :: string()) -> {ok, uri()} | undefined.
find_included_uri(Uri, Include) ->
    {AppPath, ModuleType} = app_path_and_module_type(Uri),
    find_included_uri(Uri, AppPath, ModuleType, Include).

-spec find_included_uri(Uri :: uri(),
                        AppPath :: path(),
                        ModuleType :: module_type(),
                        Include :: string()) -> {ok, uri()} | undefined.
find_included_uri(Uri, ProjPath, proj, Include) ->
    Path = esrv_lib:uri_to_path(Uri),
    IncluderDir = filename:dirname(Path),
    {ok, IncludeDirs} = esrv_config:get_value(include_dirs),
    Candidates0 =
        lists:map(fun(IncludeDir) ->
                          filename:join([ProjPath, IncludeDir, Include])
                  end, IncludeDirs),
    Candidates1 = [filename:join(IncluderDir, Include) | Candidates0],
    do_find_included_uri(Candidates1);
find_included_uri(_, AppPath, _, Include) ->
    Candidates =
        lists:map(fun(IncludeDir) ->
                          filename:join([IncludeDir, Include])
                  end, recursive_dirs(AppPath)),
    do_find_included_uri(Candidates).

-spec do_find_included_uri(Paths :: [path()]) -> {ok, uri()} | undefined.
do_find_included_uri([]) ->
    undefined;
do_find_included_uri([Path0 | T]) ->
    case filelib:is_regular(Path0) of
        true ->
            Path1 = flatten_path(Path0),
            {ok, path_to_uri(Path1)};
        false ->
            do_find_included_uri(T)
    end.

-spec find_included_lib_uri(IncludeLib :: string()) -> {ok, uri()} | undefined.
find_included_lib_uri(IncludeLib) ->
    {ok, ProjPath} = esrv_config:get_value(proj_path),
    case filename:split(IncludeLib) of
        [App | T] ->
            OtpPattern = [otp_path(ProjPath), <<"lib">>, App ++ "-*" | T],
            DepsPatterns = lists:map(fun(DepsPath) ->
                                             [DepsPath, App | T]
                                     end, deps_paths(ProjPath)),
            do_find_included_lib_uri([OtpPattern | DepsPatterns]);
        [] ->
            undefined
    end.

-spec do_find_included_lib_uri(Patterns :: [pattern()]) -> {ok, uri()} | undefined.
do_find_included_lib_uri([]) ->
    undefined;
do_find_included_lib_uri([Pattern | T]) ->
    case wildcard_paths(Pattern) of
        [Path | _] ->
            {ok, path_to_uri(Path)};
        [] ->
            do_find_included_lib_uri(T)
    end.

-spec app_path_and_module_type(TargetUri :: uri()) -> {path(), module_type()}.
app_path_and_module_type(TargetUri) ->
    TargetPath = uri_to_path(TargetUri),
    {ok, ProjPath} = esrv_config:get_value(proj_path),
    case esrv_db:read_module_meta(TargetUri) of
        [#module_meta{module_type = proj}] ->
            {ProjPath, proj};
        _ ->
            AppPathsInfo =
                [ {deps, AppPath} || AppPath <- deps_app_paths(ProjPath) ] ++
                [ {otp, AppPath} || AppPath <- otp_app_paths(ProjPath) ],
            lists:foldl(fun({ModuleType, AppPath}, Acc) when Acc =:= {ProjPath, proj} ->
                                case is_binary_prefix(AppPath, TargetPath) of
                                    true ->
                                        {AppPath, ModuleType};
                                    false ->
                                        Acc
                                end;
                           (_, Acc) ->
                                Acc
                        end, {ProjPath, proj}, AppPathsInfo)
    end.

-spec get_escript_src(Data :: binary()) -> {ok, binary()} | no_escript | no_source.
get_escript_src(<<"#!", _/binary>> = Data0) ->
    Data1 = remove_escript_first_line(Data0),
    Data2 = remove_escript_comment_line(Data1),
    Data3 = remove_escript_emu_args_line(Data2),
    case skip_new_lines(Data3) of
        <<"PK", _/binary>> ->
            %% archive
            no_source;
        <<"FOR1", _/binary>> ->
            %% beam
            no_source;
        _ ->
            {ok, Data3}
    end;
get_escript_src(_) ->
    no_escript.

-spec remove_escript_first_line(Data :: binary()) -> binary().
remove_escript_first_line(<<"#!", _/binary>> = Data0) ->
    [_, Data1] = binary:split(Data0, <<"\n">>),
    <<"\n", Data1/binary>>.

-spec remove_escript_comment_line(Data :: binary()) -> binary().
remove_escript_comment_line(<<"%%!", _/binary>> = Data) ->
    Data;
remove_escript_comment_line(<<"%", _/binary>> = Data0) ->
    [_, Data1] = binary:split(Data0, <<"\n">>),
    <<"\n", Data1/binary>>;
remove_escript_comment_line(Data) ->
    Data.

-spec remove_escript_emu_args_line(Data :: binary()) -> binary().
remove_escript_emu_args_line(<<"%%!", _/binary>> = Data0) ->
    [_, Data1] = binary:split(Data0, <<"\n">>),
    <<"\n", Data1/binary>>;
remove_escript_emu_args_line(Data) ->
    Data.

-spec skip_new_lines(Data :: binary()) -> binary().
skip_new_lines(<<"\n", T/binary>>) ->
    skip_new_lines(T);
skip_new_lines(Data) ->
    Data.

%%%===================================================================
%%% Misc
%%%===================================================================
-spec hash(Content :: binary()) -> hash().
hash(Content) ->
    crypto:hash(md5, Content).

-spec includes(AppPath :: path(), ModuleType :: module_type()) -> [file:filename()].
includes(AppPath, ModuleType) ->
    {ok, ProjPath} = esrv_config:get_value(proj_path),
    Paths =
        case ModuleType of
            proj ->
                lists:foldl(fun(IncludeDir, Acc) ->
                                    wildcard_paths([AppPath, IncludeDir]) ++ Acc
                            end, [], esrv_config:get_value(include_dirs, []));
            _ ->
                recursive_dirs(AppPath)
        end,
    [ path_to_file(Path) || Path <- Paths ++ deps_paths(ProjPath) ].

-spec defines() -> [{atom(), any()}].
defines() ->
    lists:map(fun(#{<<"name">> := Name, <<"value">> := Value}) ->
                      {binary_to_atom(Name, utf8), parse_term(Value, true)};
                 (#{<<"name">> := Name}) ->
                      {binary_to_atom(Name, utf8), true}
              end, esrv_config:get_value(macros, [])).

-spec parse_term(Binary :: binary(), Default :: any()) -> term().
parse_term(Binary, Default) ->
    try
        String = binary_to_list(Binary),
        {ok, Tokens, _} = erl_scan:string(String ++ "."),
        {ok, Parsed} = erl_parse:parse_term(Tokens),
        Parsed
    catch
        _:_ ->
            Default
    end.

-spec is_binary_prefix(Prefix :: binary(), Binary :: binary()) -> boolean().
is_binary_prefix(Prefix, Binary) ->
    case binary:match(Binary, Prefix) of
        {0, _} ->
            true;
        _ ->
            false
    end.

-spec substitute_group_leader(Fun :: fun()) -> any().
substitute_group_leader(Fun) ->
    Caller = self(),
    GroupLeader =
        spawn_link(fun F() ->
                           receive
                               {io_request, From, Ref, _} ->
                                   From ! {io_reply, Ref, []}, F();
                               stop ->
                                   ok
                           end
                   end),
    {Executor, MonRef} =
        spawn_monitor(fun() ->
                              group_leader(GroupLeader, self()),
                              Caller ! {done, Fun()}
                      end),
    receive
        {done, Result} ->
            demonitor(MonRef, [flush]),
            Result;
        {'DOWN', MonRef, process, Executor, Reason} ->
            erlang:error(Reason)
    end.

-spec gentle_exit(Code :: integer()) -> no_return().
gentle_exit(Code) ->
    init:stop(Code),
    receive ok -> ok end.
