-module(esrv_compiler_diagnostics).

-behaviour(esrv_diagnostics).

%% API
-export([init/1,
         run/4]).

-include("types.hrl").
-include("protocol.hrl").
-include("diagnostics.hrl").
-include("records.hrl").
-include("log.hrl").

-define(SOURCE, <<"compiler">>).

-type compiler_info() :: {erl_anno:line() | none, module(), any()}.
-type compiler_item() :: {file:filename(), [compiler_info()]}.
-type epp_form() :: erl_parse:abstract_form() | {error, compiler_info()} | {eof, line()}.

%%------------------------------------------------------------------------------
%% Init
%%------------------------------------------------------------------------------
-spec init(Options :: map()) -> NewOptions :: map().
init(Options0) ->
    Options1 = init_deploy_node(Options0),
    init_extra_params(Options1).

-spec init_deploy_node(Options :: map()) -> NewOptions :: map().
init_deploy_node(#{<<"deployNode">> := DeployNode} = Options) when is_atom(DeployNode) ->
    ?LOG_INFO("Deployment node: '~ts'", [DeployNode]),
    init_deploy_cookie(Options);
init_deploy_node(#{<<"deployNode">> := DeployNode0} = Options0) ->
    case binary:match(DeployNode0, <<"@">>) of
        Position when Position =/= nomatch ->
            init_deploy_node(Options0#{<<"deployNode">> => binary_to_atom(DeployNode0, utf8)});
        nomatch ->
            {ok, Hostname} = inet:gethostname(),
            DeployNode1 = iolist_to_binary([DeployNode0, <<"@">>, Hostname]),
            init_deploy_node(Options0#{<<"deployNode">> => DeployNode1})
    end;
init_deploy_node(Options) ->
    ?LOG_INFO("No deployment node configured"),
    Options.

-spec init_deploy_cookie(Options :: map()) -> NewOptions :: map().
init_deploy_cookie(#{<<"deployCookie">> := DeployCookie} = Options) when is_atom(DeployCookie) ->
    ?LOG_INFO("Applying cookie: '~ts'", [DeployCookie]),
    true = erlang:set_cookie(node(), DeployCookie),
    Options;
init_deploy_cookie(#{<<"deployCookie">> := DeployCookie0} = Options0) ->
    DeployCookie1 = binary_to_atom(DeployCookie0, utf8),
    init_deploy_cookie(Options0#{<<"deployCookie">> => DeployCookie1});
init_deploy_cookie(Options) ->
    Options.

-spec init_extra_params(Options :: map()) -> NewOptions :: map().
init_extra_params(#{<<"extraOptions">> := ExtraOptions0} = Options0)
  when is_binary(ExtraOptions0) ->
    case esrv_lib:parse_term(ExtraOptions0, undefined) of
        ExtraOptions1 when is_list(ExtraOptions1) ->
            ExtraOptions2 = lists:filter(fun valid_extra_options/1, ExtraOptions1),
            case ExtraOptions2 of
                [_|_] ->
                    ?LOG_INFO("Extra compile options: ~p", [ExtraOptions2]),
                    Options0#{<<"extraOptions">> => ExtraOptions2};
                [] ->
                    ?LOG_WARNING("No valid extra compile options found", []),
                    maps:remove(<<"extraOptions">>, Options0)
            end;
        _ ->
            ?LOG_WARNING("Incorrect extra compile options: ~ts", [ExtraOptions0]),
            maps:remove(<<"extraOptions">>, Options0)
    end;
init_extra_params(Options) ->
    Options.

-spec valid_extra_options(Option :: compile:option()) -> boolean().
valid_extra_options(Option) ->
    case Option of
        bin_opt_info -> true;
        {compile_info, _} -> true;
        compressed -> true;
        debug_info -> true;
        {debug_info, _} -> true;
        {debug_info_key, _} -> true;
        encrypt_debug_info -> true;
        deterministic -> true;
        warnings_as_errors -> true;
        export_all -> true;
        {i, _} -> true;
        {d, _} -> true;
        {d, _, _} -> true;
        {parse_transform, _} -> true;
        {extra_chunks, _} -> true;
        nowarn_export_all -> true;
        warn_export_vars -> true;
        nowarn_shadow_vars -> true;
        nowarn_unused_function -> true;
        {nowarn_unused_function, _} -> true;
        nowarn_deprecated_function -> true;
        {nowarn_deprecated_function, _} -> true;
        nowarn_deprecated_type -> true;
        nowarn_removed -> true;
        {nowarn_removed, _} -> true;
        nowarn_obsolete_guard -> true;
        warn_unused_import -> true;
        nowarn_unused_vars -> true;
        nowarn_unused_record -> true;
        nowarn_nif_inline -> true;
        _ -> false
    end.

%%------------------------------------------------------------------------------
%% Run
%%------------------------------------------------------------------------------
-spec run(Uri :: uri(), AppPath :: path(), ModuleType :: module_type(), Options :: map()) ->
          [diagnostic()].
run(_, _, otp, _) ->
    [];
run(Uri, AppPath, ModuleType, Options) ->
    Filename = esrv_lib:uri_to_file(Uri),
    case filename:extension(Filename) of
        ".erl" ->
            ensure_dependencies(Uri, Options),
            process_erl(Filename, AppPath, ModuleType, Options);
        ".hrl" ->
            process_hrl(Filename, AppPath, ModuleType);
        ".escript" ->
            process_escript(Filename, AppPath, ModuleType)
    end.

-spec process_erl(Filename :: file:filename(),
                  AppPath :: path(),
                  ModuleType :: module_type(),
                  Options :: map()) -> [diagnostic()].
process_erl(Filename, AppPath, ModuleType, Options) ->
    CompileOptions1 = add_deploy_options(Options, [return]),
    CompileOptions2 = add_strong_validation_option(CompileOptions1),
    CompileOptions3 = add_extra_options(Options, CompileOptions2),
    case compile_file(Filename, AppPath, ModuleType, CompileOptions3) of
        {ok, _, Warnings} ->
            diagnostics(Filename, [{Warnings, ?DIAGNOSTIC_WARNING}]);
        {ok, ModuleName, Binary, Warnings} ->
            deploy_module(ModuleName, Binary, Options),
            diagnostics(Filename, [{Warnings, ?DIAGNOSTIC_WARNING}]);
        {error, Errors, Warnings} ->
            diagnostics(Filename, [{Errors, ?DIAGNOSTIC_ERROR},
                                   {Warnings, ?DIAGNOSTIC_WARNING}])
    end.

-spec process_hrl(Filename :: file:filename(),
                  AppPath :: path(),
                  ModuleType :: module_type()) -> [diagnostic()].
process_hrl(Filename, AppPath, ModuleType) ->
    Errors = [ Error || {error, Error} <- epp_forms(Filename, AppPath, ModuleType) ],
    case Errors of
        [_|_] ->
            diagnostics(Filename, [{[{Filename, Errors}], ?DIAGNOSTIC_ERROR}]);
        [] ->
            []
    end.

-spec process_escript(Filename :: file:filename(),
                      AppPath :: path(),
                      ModuleType :: module_type()) -> [diagnostic()].
process_escript(Filename, AppPath, ModuleType) ->
    {ok, FileContent} = file:read_file(Filename),
    case esrv_lib:get_escript_src(FileContent) of
        {ok, SrcContent} ->
            TmpFilename = tmp_filename(Filename),
            ok = filelib:ensure_dir(TmpFilename),
            ok = file:write_file(TmpFilename, SrcContent),
            Forms0 = epp_forms(TmpFilename, AppPath, ModuleType),
            Forms1 = complete_escript_forms(TmpFilename, Forms0),
            ok = file:delete(TmpFilename),
            case compile_forms(Forms1, AppPath, ModuleType, [return, strong_validation]) of
                {ok, _, Warnings} ->
                    diagnostics(TmpFilename, [{Warnings, ?DIAGNOSTIC_WARNING}]);
                {error, Errors, Warnings} ->
                    diagnostics(TmpFilename, [{Errors, ?DIAGNOSTIC_ERROR},
                                              {Warnings, ?DIAGNOSTIC_WARNING}])
            end;
        no_escript ->
            [#diagnostic{position = {line, 1},
                         severity = ?DIAGNOSTIC_ERROR,
                         source = ?SOURCE,
                         message = <<"No escript header found">>}];
        no_source ->
            []
    end.

-spec complete_escript_forms(Filename :: file:filename(), Forms :: [epp_form()]) -> [epp_form()].
complete_escript_forms(Filename, [{attribute, _, file, _} | _] = Forms0) ->
    Forms1 = ensure_module_form(Filename, Forms0),
    Forms2 = ensure_main_exported(Forms1),
    check_mode_forms(Forms2).

-spec ensure_module_form(Filename :: file:filename(), Forms :: [epp_form()]) -> [epp_form()].
ensure_module_form(_, [_, {attribute, _, module, _} | _] = Forms) ->
    Forms;
ensure_module_form(Filename, [FileAttr | T]) ->
    {match, [Name]} = re:run(filename:basename(Filename),
                             "^(.*).escript$",
                             [{capture, all_but_first, binary}]),
    Module = binary_to_atom(Name, utf8),
    [FileAttr, {attribute, erl_anno:new(1), module, Module} | T].

-spec ensure_main_exported(Forms :: [epp_form()]) -> [epp_form()].
ensure_main_exported([FileAttr, ModuleAttr | T] = Forms0) ->
    IsExported =
        lists:any(fun({attribute, _, export, Export}) ->
                          lists:member({main, 1}, Export);
                     (_) ->
                          false
                  end, Forms0),
    if
        not IsExported ->
            [FileAttr, ModuleAttr, {attribute, erl_anno:new(1), export, [{main, 1}]} | T];
        true ->
            Forms0
    end.

-spec check_mode_forms(Forms :: [epp_form()]) -> [epp_form()].
check_mode_forms(Forms0) ->
    lists:map(fun({attribute, Anno, mode, Mode}) when Mode =/= compile andalso
                                                      Mode =/= debug andalso
                                                      Mode =/= interpret andalso
                                                      Mode =/= native ->
                      {error, {Anno, erl_parse, "illegal mode attribute"}};
                 (Form) ->
                      Form
              end, Forms0).

-spec tmp_filename(Filename :: file:filename()) -> file:filename().
tmp_filename(Filename) ->
    TmpPath = filename:join([esrv_lib:bdir_user_cache(),
                             <<"escript_src">>,
                             filename:basename(Filename)]),
    esrv_lib:path_to_file(TmpPath).

-spec epp_forms(Filename :: file:filename(),
                AppPath :: path(),
                ModuleType :: module_type()) -> [epp_form()].
epp_forms(Filename, AppPath, ModuleType) ->
    {ok, Epp} = epp:open(Filename, esrv_lib:includes(AppPath, ModuleType), esrv_lib:defines()),
    EppForms = epp:parse_file(Epp),
    ok = epp:close(Epp),
    EppForms.

%%------------------------------------------------------------------------------
%% Compiling
%%------------------------------------------------------------------------------
-spec compile_file(Filename :: file:filename(),
                   AppPath :: path(),
                   ModuleType :: module_type(),
                   CompileOptions :: [compile:option()]) -> any().
compile_file(Filename, AppPath, ModuleType, CompileOptions0) ->
    CompileOptions1 = add_basic_options(AppPath, ModuleType, CompileOptions0),
    compile:file(Filename, CompileOptions1).

-spec compile_forms(Forms :: [epp_form()],
                    AppPath :: path(),
                    ModuleType :: module_type(),
                    CompileOptions :: [compile:option()]) -> any().
compile_forms(Forms, AppPath, ModuleType, CompileOptions0) ->
    CompileOptions1 = add_basic_options(AppPath, ModuleType, CompileOptions0),
    compile:forms(Forms, CompileOptions1).

-spec add_deploy_options(Options :: map(), CompileOptions :: [compile:option()]) ->
          [compile:option()].
add_deploy_options(Options, CompileOptions0) when is_map_key(<<"deployNode">>, Options) ->
    add_option(binary, CompileOptions0);
add_deploy_options(_, CompileOptions) ->
    CompileOptions.

-spec add_strong_validation_option(CompileOptions :: [compile:option()]) -> [compile:option()].
add_strong_validation_option(CompileOptions0) ->
    case lists:member(binary, CompileOptions0) of
        true ->
            CompileOptions0;
        false ->
            add_option(strong_validation, CompileOptions0)
    end.

-spec add_extra_options(Options :: map(), CompileOptions :: [compile:option()]) ->
          [compile:option()].
add_extra_options(#{<<"extraOptions">> := ExtraOptions}, CompileOptions0) ->
    lists:foldl(fun add_option/2, CompileOptions0, ExtraOptions);
add_extra_options(_, CompileOptions) ->
    CompileOptions.

-spec add_basic_options(AppPath :: path(),
                        ModuleType :: module_type(),
                        CompileOptions :: [compile:option()]) -> [compile:option()].
add_basic_options(AppPath, ModuleType, CompileOptions0) ->
    CompileOptions0 ++
        [ {i, I} || I <- esrv_lib:includes(AppPath, ModuleType) ] ++
        [ {d, K, V} || {K, V} <- esrv_lib:defines() ].

-spec add_option(CompileOption :: compile:option(), CompileOptions :: [compile:option()]) ->
          [compile:option()].
add_option(CompileOption, CompileOptions0) ->
    case lists:member(CompileOption, CompileOptions0) of
        true ->
            CompileOptions0;
        false ->
            [CompileOption | CompileOptions0]
    end.

%%------------------------------------------------------------------------------
%% Dependencies handling
%%------------------------------------------------------------------------------
-record(deps_info, {options :: map(),
                    processed :: sets:set(),
                    chain :: [uri()],
                    target_uri :: uri(),
                    target_module :: module() | undefined}).

-spec ensure_dependencies(Uri :: uri(), Options :: map()) -> #deps_info{}.
ensure_dependencies(Uri, Options) ->
    DepsInfo = #deps_info{options = Options, processed = sets:new(), chain = [], target_uri = Uri},
    do_ensure_dependencies(Uri, DepsInfo).

-spec do_ensure_dependencies(Uri :: uri(), DepsInfo :: #deps_info{}) -> #deps_info{}.
do_ensure_dependencies(Uri, #deps_info{chain = Chain0} = DepsInfo0) ->
    case lists:member(Uri, Chain0) of
        false ->
            case esrv_db:get_module_data(Uri) of
                {ok, ModuleData} ->
                    DepsInfo1 = DepsInfo0#deps_info{chain = [Uri | Chain0]},
                    DepsInfo2 = set_target_module(Uri, ModuleData, DepsInfo1),
                    DepsInfo3 = process_included_uris(ModuleData, DepsInfo2),
                    DepsInfo4 = process_behaviors(ModuleData, DepsInfo3),
                    process_parse_transforms(ModuleData, DepsInfo4);
                undefined ->
                    DepsInfo0
            end;
        true ->
            DepsInfo0
    end.

-spec set_target_module(Uri :: uri(), ModuleData :: module_data(), DepsInfo :: #deps_info{}) ->
          #deps_info{}.
set_target_module(Uri, ModuleData, #deps_info{target_uri = Uri} = DepsInfo0) ->
    case esrv_req_lib:get_module_name(Uri, ModuleData) of
        {ok, {_, {Module, _}}} ->
            DepsInfo0#deps_info{target_module = Module};
        undefined ->
            DepsInfo0
    end;
set_target_module(_, _, DepsInfo) ->
    DepsInfo.

-spec process_included_uris(ModuleData :: module_data(), DepsInfo :: #deps_info{}) ->
          #deps_info{}.
process_included_uris(#module_data{include_data = #include_data{resolved = Resolved}}, DepsInfo0) ->
    lists:foldl(fun do_ensure_dependencies/2, DepsInfo0, maps:keys(Resolved)).

-spec process_behaviors(ModuleData :: module_data(), DepsInfo :: #deps_info{}) -> #deps_info{}.
process_behaviors(#module_data{behaviors = Behaviors}, DepsInfo0) ->
    Modules = maps:keys(Behaviors),
    lists:foldl(fun process_module/2, DepsInfo0, filter_modules(Modules, DepsInfo0)).

-spec process_parse_transforms(ModuleData :: module_data(), DepsInfo :: #deps_info{}) ->
          #deps_info{}.
process_parse_transforms(#module_data{parse_transform = ParseTransform},
                         #deps_info{options = Options} = DepsInfo0) ->
    ExtraOptions = maps:get(<<"extraOptions">>, Options, []),
    Modules = [ PT || {parse_transform, PT} <- ExtraOptions ] ++ ParseTransform,
    lists:foldl(fun(Module, DepsInfo1) ->
                        DepsInfo2 = process_module(Module, DepsInfo1),
                        ok = ensure_parse_transform_module(Module),
                        DepsInfo2
                end, DepsInfo0, filter_modules(Modules, DepsInfo0)).

-spec filter_modules(Modules :: [module()], DepsInfo :: #deps_info{}) -> [module()].
filter_modules(Modules, #deps_info{target_module = TargetModule}) ->
    [ M || M <- Modules, M =/= TargetModule ].

-spec process_module(Module :: module(), DepsInfo :: #deps_info{}) -> #deps_info{}.
process_module(Module, #deps_info{processed = Processed0, target_uri = TargetUri} = DepsInfo0) ->
    case sets:is_element(Module, Processed0) of
        false ->
            DepsInfo1 = DepsInfo0#deps_info{processed = sets:add_element(Module, Processed0)},
            case esrv_db:read_module_meta_by_name(Module) of
                [#module_meta{module_type = otp} | _] ->
                    do_load_module(Module),
                    DepsInfo1;
                [#module_meta{uri = Uri, hash = Hash} | _] ->
                    ok = esrv_diagnostics_srv:add_dependency(Module, TargetUri),
                    do_compile_and_load(Uri, Hash, DepsInfo1);
                [] ->
                    DepsInfo1
            end;
        true ->
            DepsInfo0
    end.

-spec do_load_module(Module :: module()) -> ok.
do_load_module(Module) ->
    case code:is_loaded(Module) of
        false ->
            case code:load_file(Module) of
                {module, Module} ->
                    ok;
                {error, Error} ->
                    ?LOG_WARNING("Unable to load '~s' module: ~p", [Module, Error])
            end;
        _ ->
            ok
    end.

-spec do_compile_and_load(Uri :: uri(), Hash :: hash(),  DepsInfo :: #deps_info{}) -> #deps_info{}.
do_compile_and_load(Uri, Hash, DepsInfo0) ->
    case esrv_db:read_loaded_module(Uri) of
        [#loaded_module{hash = Hash}] ->
            DepsInfo0;
        _ ->
            DepsInfo1 = do_ensure_dependencies(Uri, DepsInfo0),
            Filename = esrv_lib:uri_to_file(Uri),
            {AppPath, ModuleType} = esrv_lib:app_path_and_module_type(Uri),
            case compile_file(Filename, AppPath, ModuleType, [binary]) of
                {ok, Module, Binary} ->
                    case code:load_binary(Module, Filename, Binary) of
                        {module, Module} ->
                            ok = esrv_db:write_loaded_module(Uri, Hash);
                        {error, Error} ->
                            ?LOG_WARNING("Unable to load ~ts: ~p", [Uri, Error])
                    end;
                _ ->
                    ok
            end,
            DepsInfo1
    end.

-spec ensure_parse_transform_module(Module :: module()) -> ok.
ensure_parse_transform_module(Module) ->
    case code:is_loaded(Module) of
        false ->
            Anno = erl_anno:new(1),
            Forms =
                [{attribute, Anno, module, Module},
                 {attribute, Anno, export, [{parse_transform, 2}]},
                 {function, Anno, parse_transform, 2,
                  [{clause, Anno,
                    [{var, Anno, 'Ast'}, {var, Anno, '_'}],
                    [],
                    [{var, Anno, 'Ast'}]}]}],
            {ok, Module, Binary} = compile:forms(Forms),
            case code:load_binary(Module, "", Binary) of
                {module, Module} ->
                    ?LOG_INFO("Dummy module '~s' loaded", [Module]);
                {error, Error} ->
                    ?LOG_WARNING("Unable to load '~s' dummy module: ~p", [Module, Error])
            end;
        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% Deploying
%%------------------------------------------------------------------------------
-spec deploy_module(ModuleName :: module(), Binary :: binary(), Options :: map()) -> ok.
deploy_module(ModuleName, Binary, #{<<"deployNode">> := DeployNode}) ->
    case rpc:call(DeployNode, code, load_binary, [ModuleName, "", Binary]) of
        {module, ModuleName} ->
            ?LOG_DEBUG("Module '~s' deployed at '~s'",
                       [ModuleName, DeployNode]);
        {badrpc, nodedown} ->
            ?LOG_DEBUG("Module '~s' deploying at '~s' skipped: nodedown",
                       [ModuleName, DeployNode]);
        {Tag, Error} ->
            ?LOG_ERROR("Module '~s' deploying at '~s' ~p: ~p",
                       [ModuleName, DeployNode, Tag, Error]),
            Message = io_lib:format("Module '~s' deploying at '~s' ~p: ~p",
                                    [ModuleName, DeployNode, Tag, Error]),
            Notification = #notification{method = <<"window/showMessage">>,
                                         params = [{<<"type">>, ?MESSAGE_TYPE_INFO},
                                                   {<<"message">>, iolist_to_binary(Message)}]},
            ok = esrv_main_fsm:notification(Notification)
    end;
deploy_module(_, _, _) ->
    ok.

%%------------------------------------------------------------------------------
%% Diagnostics
%%------------------------------------------------------------------------------
-spec diagnostics(TargetFilename :: file:filename(), Items :: [{[compiler_item()], integer()}]) ->
          [diagnostic()].
diagnostics(_, []) ->
    [];
diagnostics(TargetFilename, [{Items, Severity} | T]) ->
    lists:foldl(fun(Item, Acc) ->
                        diagnostics(TargetFilename, Item, Severity, Acc)
                end, diagnostics(TargetFilename, T), Items).

-spec diagnostics(TargetFilename :: file:filename(),
                  Item :: {file:filename(), [compiler_info()]},
                  Severity :: integer(),
                  Acc :: [diagnostic()]) -> [diagnostic()].
diagnostics(TargetFilename, {MessageFilename, InfoList}, Severity, Acc0) ->
    lists:foldl(fun({AnnoLine, Module, Data}, Acc00) ->
                        Position =
                            if
                                TargetFilename =:= MessageFilename ->
                                    {line, format_line(AnnoLine)};
                                true ->
                                    case get_resolved_include(TargetFilename, MessageFilename) of
                                        {ok, #resolved_include{line = Line}} ->
                                            {line, Line};
                                        error ->
                                            {line, 1}
                                    end
                            end,
                        Message =
                            if
                                TargetFilename =:= MessageFilename ->
                                    format_message(Module, Data);
                                true ->
                                    MessageData =
                                        io_lib:format("Issue in '~s' at line ~p: ~s",
                                                      [filename:basename(MessageFilename),
                                                       format_line(AnnoLine),
                                                       format_message(Module, Data)]),
                                    iolist_to_binary(MessageData)
                            end,
                        [#diagnostic{position = Position,
                                     severity = Severity,
                                     source = ?SOURCE,
                                     message = Message} | Acc00]
                end, Acc0, InfoList).

-spec get_resolved_include(TargetFilename :: file:filename(),
                           MessageFilename :: file:filename()) -> {ok, resolved_include()} | error.
get_resolved_include(TargetFilename, MessageFilename) ->
    TargetUri = esrv_lib:file_to_uri(TargetFilename),
    case esrv_index_mgr:get_current_module_data(TargetUri) of
        {ok, #module_data{include_data = #include_data{resolved = Resolved}}} ->
            MessageUri = esrv_lib:file_to_uri(MessageFilename),
            maps:find(MessageUri, Resolved);
        undefined ->
            error
    end.

-spec format_message(Module :: module(), Data :: any()) -> binary().
format_message(Module, Data) ->
    Message0 = Module:format_error(Data),
    Message1 = lists:flatten(Message0),
    list_to_binary(Message1).

-spec format_line(AnnoLine :: erl_anno:line() | none) -> diagnostic_line().
format_line(AnnoLine) when is_integer(AnnoLine) ->
    AnnoLine;
format_line(_) ->
    1.
