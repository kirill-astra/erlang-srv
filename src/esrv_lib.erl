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
         get_target_paths/0,
         get_proj_path/0,
         get_sub_projs/0,
         get_sub_proj_dirs/0,
         get_deps/0,
         get_dep_dirs/0,
         get_otp_path/0,
         get_app_dirs/1,
         get_include_dirs/1,
         get_include_dirs/2,
         scan_files/2,
         scan_files/3,
         scan_deep_files/2,
         scan_deep_files/3,
         scan_dirs/1,
         scan_dirs/2,
         scan_deep_dirs/1,
         scan_deep_dirs/2,
         scan_file_system/2,
         ensure_absolute_path/2,
         rm_rf/1]).

%% LSP utils
-export([version/0,
         get_client_capability/1,
         format_range/1,
         format_range/2]).

%% Parsed data utils
-export([get_app_id/1,
         get_app_type/1,
         get_app_path/1,
         get_app_type_and_path/1,
         find_included_uri/3,
         find_included_lib_uri/1,
         get_module_data/1,
         get_current_content/1,
         fetch_text_document_lines/3,
         fetch_text_document_form/2,
         get_escript_src/1]).

%% Misc
-export([hash/1,
         includes/1,
         defines/0,
         parse_term/2,
         is_binary_prefix/2,
         substitute_group_leader/1,
         get_distr_mode/0,
         append_release_and_distr_mode/1,
         gentle_exit/1]).

-include_lib("kernel/include/file.hrl").

-include("types.hrl").
-include("records.hrl").
-include("parser.hrl").

-define(BDIR_PREFIX, <<"erlang_srv">>).

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

-spec get_target_paths() -> [path()].
get_target_paths() ->
    lists:flatten([get_proj_path(),
                   get_sub_projs(),
                   get_sub_proj_dirs(),
                   get_deps(),
                   get_dep_dirs(),
                   get_otp_path()]).

-spec get_proj_path() -> path().
get_proj_path() ->
    esrv_config:fetch_value(proj_path).

-spec get_sub_projs() -> [path()].
get_sub_projs() ->
    {ok, ProjPath} = esrv_config:get_value(proj_path),
    SubProjs = esrv_config:get_value(sub_projs, []),
    ensure_absolute_paths(ProjPath, SubProjs).

-spec get_sub_proj_dirs() -> [path()].
get_sub_proj_dirs() ->
    {ok, ProjPath} = esrv_config:get_value(proj_path),
    SubProjDirs = esrv_config:get_value(sub_proj_dirs, []),
    ensure_absolute_paths(ProjPath, SubProjDirs).

-spec get_deps() -> [path()].
get_deps() ->
    {ok, ProjPath} = esrv_config:get_value(proj_path),
    Deps = esrv_config:get_value(deps, []),
    ensure_absolute_paths(ProjPath, Deps).

-spec get_dep_dirs() -> [path()].
get_dep_dirs() ->
    {ok, ProjPath} = esrv_config:get_value(proj_path),
    DepDirs = esrv_config:get_value(dep_dirs, []),
    ensure_absolute_paths(ProjPath, DepDirs).

-spec get_otp_path() -> path().
get_otp_path() ->
    RootDir = code:root_dir(),
    list_to_binary(RootDir).

-spec get_app_dirs(Base :: path()) -> [path()].
get_app_dirs(Base) ->
    lists:map(fun(AppsDir) ->
                      ensure_absolute_path(Base, AppsDir)
              end, esrv_config:fetch_value(apps_dirs)).

-spec get_include_dirs(Base :: path()) -> [path()].
get_include_dirs(Base) ->
    lists:map(fun(IncludeDir) ->
                      ensure_absolute_path(Base, IncludeDir)
              end, esrv_config:fetch_value(include_dirs)).

-spec get_include_dirs(AppType :: app_type(), AppPath :: path()) -> [path()].
get_include_dirs(AppType, AppPath) ->
    case AppType of
        otp ->
            scan_deep_dirs(AppPath);
        deps ->
            scan_deep_dirs(AppPath);
        sub_proj ->
            get_include_dirs(AppPath);
        proj ->
            get_include_dirs(AppPath)
    end.

-spec scan_files(Pattern :: scan_fs_pattern(), Extensions :: [binary()]) -> [path()].
scan_files(Pattern, Extensions) ->
    scan_files(Pattern, Extensions, []).

-spec scan_files(Pattern :: scan_fs_pattern(),
                 Extensions :: [binary()],
                 ToSkip :: [path()]) -> [path()].
scan_files(Pattern, Extensions, ToSkip) ->
    scan_file_system(Pattern, #{filter => file,
                                recursive => false,
                                to_skip => ToSkip,
                                extensions => Extensions}).

-spec scan_deep_files(Pattern :: scan_fs_pattern(), Extensions :: [binary()]) -> [path()].
scan_deep_files(Pattern, Extensions) ->
    scan_deep_files(Pattern, Extensions, []).

-spec scan_deep_files(Pattern :: scan_fs_pattern(),
                      Extensions :: [binary()],
                      ToSkip :: [path()]) -> [path()].
scan_deep_files(Pattern, Extensions, ToSkip) ->
    scan_file_system(Pattern, #{filter => file,
                                recursive => true,
                                to_skip => ToSkip,
                                extensions => Extensions}).

-spec scan_dirs(Pattern :: scan_fs_pattern()) -> [path()].
scan_dirs(Pattern) ->
    scan_dirs(Pattern, []).

-spec scan_dirs(Pattern :: scan_fs_pattern(), ToSkip :: [path()]) -> [path()].
scan_dirs(Pattern, ToSkip) ->
    scan_file_system(Pattern, #{filter => dir, recursive => false, to_skip => ToSkip}).

-spec scan_deep_dirs(Pattern :: scan_fs_pattern()) -> [path()].
scan_deep_dirs(Pattern) ->
    scan_deep_dirs(Pattern, []).

-spec scan_deep_dirs(Pattern :: scan_fs_pattern(), ToSkip :: [path()]) -> [path()].
scan_deep_dirs(Pattern, ToSkip) ->
    scan_file_system(Pattern, #{filter => dir, recursive => true, to_skip => ToSkip}).

-spec scan_file_system(Pattern :: scan_fs_pattern(), Options :: scan_fs_options()) ->
          [path()].
scan_file_system(Pattern, Options) ->
    {_, Paths} = scan_file_system(Pattern, Options, [], []),
    lists:reverse(Paths).

-spec scan_file_system(Pattern :: scan_fs_pattern(),
                       Options :: scan_fs_options(),
                       ChainAcc :: [path()],
                       PathsAcc :: [path()]) -> {[path()], [path()]}.
scan_file_system(Pattern, Options, ChainAcc0, PathsAcc0) ->
    lists:foldl(fun(Path, {ChainAcc00, PathsAcc00}) ->
                        scan_path(Path, Options, ChainAcc00, PathsAcc00)
                end, {ChainAcc0, PathsAcc0}, resolve_pattern(Pattern)).

-spec resolve_pattern(Pattern :: scan_fs_pattern()) -> [path()].
resolve_pattern(Pattern0) when is_list(Pattern0) ->
    case lists:all(fun is_integer/1, Pattern0) of
        true ->
            Pattern1 = unicode:characters_to_binary(Pattern0),
            resolve_pattern(Pattern1);
        false ->
            Pattern1 = filename:join(Pattern0),
            resolve_pattern(Pattern1)
    end;
resolve_pattern(Pattern0) when is_binary(Pattern0) ->
    Pattern1 = binary_to_list(Pattern0),
    FileNames = filelib:wildcard(Pattern1),
    lists:map(fun file_to_path/1, FileNames).

-spec scan_path(Path :: path(),
                Options :: scan_fs_options(),
                ChainAcc :: [path()],
                PathsAcc :: [path()]) -> {[path()], [path()]}.
scan_path(Path, Options, ChainAcc0, PathsAcc0) ->
    case resolve_link(Path, [Path]) of
        {ok, _, #file_info{type = regular}} ->
            {ChainAcc0, maybe_add_file(Path, Options, PathsAcc0)};
        {ok, ResolvedPath, #file_info{type = directory}} ->
            maybe_add_dir(Path, ResolvedPath, Options, ChainAcc0, PathsAcc0);
        _ ->
            {ChainAcc0, PathsAcc0}
    end.

-spec resolve_link(Path :: path(), Chain :: [path()]) -> {ok, path(), file:file_info()} | undefined.
resolve_link(Path, Chain0) ->
    case file:read_link_info(Path) of
        {ok, #file_info{type = Type} = FileInfo}
          when Type =:= regular orelse Type =:= directory ->
            {ok, Path, FileInfo};
        {ok, #file_info{type = symlink}} ->
            {ok, LinkFileName} = file:read_link(Path),
            LinkPath0 = file_to_path(LinkFileName),
            Base = filename:dirname(Path),
            LinkPath1 = ensure_absolute_path(Base, LinkPath0),
            case lists:member(LinkPath1, Chain0) of
                false ->
                    Chain1 = [LinkPath1 | Chain0],
                    resolve_link(LinkPath1, Chain1);
                true ->
                    undefined
            end;
        _ ->
            undefined
    end.

-spec maybe_add_file(Path :: path(),
                     Options :: scan_fs_options(),
                     PathsAcc :: [path()]) -> [path()].
maybe_add_file(Path, Options, PathsAcc) ->
    case maps:get(filter, Options, both) of
        Filter when Filter =:= file orelse Filter =:= both ->
            case maps:find(extensions, Options) of
                {ok, Extensions} ->
                    Extension = filename:extension(Path),
                    case lists:member(Extension, Extensions) of
                        true ->
                            maybe_add(Path, Options, PathsAcc);
                        false ->
                            PathsAcc
                    end;
                error ->
                    maybe_add(Path, Options, PathsAcc)
            end;
        _ ->
            PathsAcc
    end.

-spec maybe_add_dir(Path :: path(),
                    ResolvedPath :: path(),
                    Options :: scan_fs_options(),
                    ChainAcc :: [path()],
                    PathsAcc :: [path()]) -> {[path()], [path()]}.
maybe_add_dir(Path, ResolvedPath, Options, ChainAcc0, PathsAcc0) ->
    case lists:member(ResolvedPath, ChainAcc0) of
        false ->
            PathsAcc1 =
                case maps:get(filter, Options, both) of
                    Filter when Filter =:= dir orelse Filter =:= both ->
                        maybe_add(Path, Options, PathsAcc0);
                    _ ->
                        PathsAcc0
                end,
            maybe_recursive(Path, ResolvedPath, Options, ChainAcc0, PathsAcc1);
        true ->
            {ChainAcc0, PathsAcc0}
    end.

-spec maybe_add(Path :: path(),
                Options :: scan_fs_options(),
                PathsAcc :: [path()]) -> [path()].
maybe_add(Path, Options, PathsAcc) ->
    ToSkip = maps:get(to_skip, Options, []),
    case lists:member(Path, ToSkip) of
        false ->
            [Path | PathsAcc];
        true ->
            PathsAcc
    end.

-spec maybe_recursive(Path :: path(),
                      ResolvedPath :: path(),
                      Options :: scan_fs_options(),
                      ChainAcc :: [path()],
                      PathsAcc :: [path()]) -> {[path()], [path()]}.
maybe_recursive(Path, ResolvedPath, Options, ChainAcc0, PathsAcc0) ->
    case maps:get(recursive, Options, false) of
        true ->
            ToSkip = maps:get(to_skip, Options, []),
            case lists:member(Path, ToSkip) of
                false ->
                    ChainAcc1 = [ResolvedPath | ChainAcc0],
                    scan_file_system([Path, <<"*">>], Options, ChainAcc1, PathsAcc0);
                true ->
                    {ChainAcc0, PathsAcc0}
            end;
        false ->
            {ChainAcc0, PathsAcc0}
    end.

-spec ensure_absolute_paths(Base :: path(), Paths :: [path()]) -> [path()].
ensure_absolute_paths(Base, Paths) ->
    [ ensure_absolute_path(Base, Path) || Path <- Paths ].

-spec ensure_absolute_path(Base :: path(), Path :: path()) -> path().
ensure_absolute_path(Base, Path0) ->
    Path1 =
        case re:run(Path0, "^([a-zA-z]:)?/") of
            {match, _} ->
                Path0;
            nomatch ->
                filename:join(Base, Path0)
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

-spec rm_rf(Target :: file:filename_all()) -> ok.
rm_rf(Target) ->
    case file:read_link_info(Target) of
        {ok, #file_info{type = directory}} ->
            lists:foreach(fun rm_rf/1, scan_file_system([Target, <<"*">>], #{recursive => false})),
            ok = file:del_dir(Target);
        {ok, #file_info{type = Type}} when Type =:= regular orelse Type =:= symlink ->
            ok = file:delete(Target);
        _ ->
            ok
    end.

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
-spec get_app_id(Uri :: uri()) -> app_id() | undefined.
get_app_id(Uri) ->
    case esrv_db:read_module_meta(Uri) of
        [#module_meta{app_id = AppId}] ->
            AppId;
        [] ->
            discover_app_id(Uri)
    end.

-spec discover_app_id(TargetUri :: uri()) -> app_id() | undefined.
discover_app_id(TargetUri) ->
    TargetPath = uri_to_path(TargetUri),
    Candidates =
        lists:foldl(fun(AppType, Acc) ->
                            discover_app_id(TargetPath, AppType, Acc)
                    end, [], [otp, deps, sub_proj, proj]),
    case Candidates of
        [_|_] ->
            {AppId, _} =
                lists:foldl(fun(#scanned_app{id = AppId, path = Path}, {_, MaxLength})
                                  when byte_size(Path) > MaxLength ->
                                    {AppId, byte_size(Path)};
                               (_, Acc) ->
                                    Acc
                            end, {undefined, 0}, Candidates),
            AppId;
        [] ->
            undefined
    end.

-spec discover_app_id(TargetPath :: uri(), AppType :: app_type(), Acc :: [scanned_app()]) ->
          [scanned_app()].
discover_app_id(TargetPath, AppType, Acc0) ->
    lists:foldl(fun(#scanned_app{path = Path} = ScannedApp, Acc00) ->
                          case is_binary_prefix(Path, TargetPath) of
                              true ->
                                  [ScannedApp | Acc00];
                              false ->
                                  Acc00
                          end
                  end, Acc0, esrv_db:read_scanned_app_by_type(AppType)).

-spec get_app_type(AppId :: app_id()) -> {ok, app_type()} | undefined.
get_app_type(AppId) ->
    case esrv_db:read_scanned_app(AppId) of
        [#scanned_app{type = Type}] ->
            {ok, Type};
        [] ->
            undefined
    end.

-spec get_app_path(AppId :: app_id()) -> {ok, path()} | undefined.
get_app_path(AppId) ->
    case esrv_db:read_scanned_app(AppId) of
        [#scanned_app{path = Path}] ->
            {ok, Path};
        [] ->
            undefined
    end.

-spec get_app_type_and_path(AppId :: app_id()) -> {ok, app_type(), path()} | undefined.
get_app_type_and_path(AppId) ->
    case esrv_db:read_scanned_app(AppId) of
        [#scanned_app{type = Type, path = Path}] ->
            {ok, Type, Path};
        [] ->
            undefined
    end.

-spec find_included_uri(Uri :: uri(),
                        AppId :: app_id() | undefined,
                        Include :: string()) -> {ok, uri()} | undefined.
find_included_uri(Uri, undefined, Include) ->
    Path = esrv_lib:uri_to_path(Uri),
    IncluderDir = filename:dirname(Path),
    do_find_included_uri([IncluderDir], Include);
find_included_uri(Uri, AppId, Include) ->
    {ok, AppType, AppPath} = get_app_type_and_path(AppId),
    Path = esrv_lib:uri_to_path(Uri),
    IncluderDir = filename:dirname(Path),
    IncludeDirs = get_include_dirs(AppType, AppPath),
    do_find_included_uri([IncluderDir | IncludeDirs], Include).

-spec do_find_included_uri(IncludeDirs :: [path()], Include :: string()) -> {ok, uri()} | undefined.
do_find_included_uri([], _) ->
    undefined;
do_find_included_uri([IncludeDir | T], Include) ->
    Path = filename:join(IncludeDir, Include),
    case filelib:is_regular(Path) of
        true ->
            {ok, path_to_uri(flatten_path(Path))};
        false ->
            do_find_included_uri(T, Include)
    end.

-spec find_included_lib_uri(IncludeLib :: string()) -> {ok, uri()} | undefined.
find_included_lib_uri(IncludeLib) ->
    case filename:split(IncludeLib) of
        [AppName | T] ->
            AppId = list_to_atom(AppName),
            case esrv_db:read_scanned_app(AppId) of
                [#scanned_app{path = AppPath}] ->
                    Path = filename:join([AppPath | T]),
                    case filelib:is_regular(Path) of
                        true ->
                            {ok, path_to_uri(Path)};
                        false ->
                            undefined
                    end;
                [] ->
                    undefined
            end;
        [] ->
            undefined
    end.

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

-spec includes(AppId :: app_id() | undefined) -> [file:filename()].
includes(AppId) ->
    Paths =
        case AppId of
            AppId when AppId =/= undefined ->
                case get_app_type_and_path(AppId) of
                    {ok, AppType, AppPath} ->
                        get_include_dirs(AppType, AppPath);
                    undefined ->
                        []
                end;
            undefined ->
                []
        end,
    [ path_to_file(Path) || Path <- Paths ++ get_sub_proj_dirs() ++ get_dep_dirs() ].

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

-spec get_distr_mode() -> distr_mode().
get_distr_mode() ->
    {ok, DistrMode} = esrv_config:get_value(distr_mode),
    if
        DistrMode =:= <<"shortnames">> orelse DistrMode =:= <<"longnames">> ->
            binary_to_atom(DistrMode, utf8);
        true ->
            shortnames
    end.

-spec append_release_and_distr_mode(Value :: string()) -> string().
append_release_and_distr_mode(Value) ->
    OptRelease = erlang:system_info(otp_release),
    case get_distr_mode() of
        longnames ->
            Value ++ "_" ++ OptRelease ++ "_long";
        shortnames ->
            Value ++ "_" ++ OptRelease ++ "_short"
    end.

-spec gentle_exit(Code :: integer()) -> no_return().
gentle_exit(Code) ->
    init:stop(Code),
    receive ok -> ok end.
