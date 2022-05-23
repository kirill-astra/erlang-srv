-module(esrv_req_initialize).

-behaviour(esrv_request_processor).

%% API
-export([process/1]).

-include("types.hrl").
-include("protocol.hrl").
-include("records.hrl").
-include("log.hrl").

-type init_options() :: jsx:json_term().
-type config_options() :: #{binary() => any()}.
-type diagnostic() :: jsx:json_term().

-spec process(Request :: request()) -> {async, pid()}.
process(Request) ->
    {async, spawn_link(fun() -> do_process(Request) end)}.

-spec do_process(Request :: request()) -> ok.
do_process(#request{id = Id, params = Params}) ->
    ok = process_client_info(Params),
    ok = process_trace(Params),
    ok = process_capabilities(Params),
    ProjPath = get_proj_path(Params),
    ok = esrv_config:set_value(proj_path, ProjPath),
    InitOptions = get_init_options(Params),
    ConfigOptions = get_config_options(ProjPath, InitOptions),
    OptionsToApply =
        [{sub_proj_dirs, <<"subProjDirs">>, [<<"_checkouts">>]},
         {sub_projs, <<"subProjs">>, undefined},
         {dep_dirs, <<"depDirs">>, [<<"deps">>, <<"_build/default/lib">>]},
         {deps, <<"deps">>, undefined},
         {apps_ignore, <<"appsIgnore">>, undefined},
         {apps_dirs, <<"appsDirs">>, [<<"src">>, <<"test">>, <<"include">>]},
         {include_dirs, <<"includeDirs">>, [<<"include">>, <<"src/hrl">>]},
         {extra_paths, <<"extraPaths">>, undefined},
         {distr_mode, <<"distrMode">>, <<"shortnames">>},
         {macros, <<"macros">>, undefined},
         {sync_otp_man, <<"syncOtpMan">>, true},
         {dedicated_otp_node, <<"dedicatedOtpNode">>, true}],
    ok = apply_options(OptionsToApply, ConfigOptions, InitOptions),
    ok = esrv_distributed:initialize_ls(),
    ok = init_extra_paths(),
    DefaultDiagnostics =
        [#{<<"name">> => <<"compiler">>, <<"enabled">> => true},
         #{<<"name">> => <<"xref">>, <<"enabled">> => true},
         #{<<"name">> => <<"elvis">>, <<"enabled">> => true},
         #{<<"name">> => <<"unused_macros">>, <<"enabled">> => true},
         #{<<"name">> => <<"dialyzer">>, <<"enabled">> => false}],
    ok = apply_diagnostics(ConfigOptions, InitOptions, DefaultDiagnostics),
    ok = init_predefined_macros(),
    ok = esrv_man_mgr:initialize(),
    ok = esrv_db:initialize(),
    ok = esrv_otp_node_controller:initialize(),
    ok = scan_applications(),
    ProgressToken = maps:get(<<"workDoneToken">>, Params, undefined),
    ok = index_applications(ProgressToken),
    Response =
        #response{id = Id,
                  result = #{<<"capabilities">> => server_capabilities(),
                             <<"serverInfo">> => #{<<"name">> => <<"Erlang SRV">>,
                                                   <<"version">> => esrv_lib:version()}}},
    ok = esrv_main_fsm:initialized(Response).

-spec server_capabilities() -> jsx:json_term().
server_capabilities() ->
    #{<<"completionProvider">> =>
          #{<<"triggerCharacters">> => [<<":">>,
                                        <<"#">>,
                                        <<"?">>,
                                        <<".">>,
                                        <<"-">>,
                                        <<"{">>,
                                        <<"(">>],
            <<"allCommitCharacters">> => [<<"\n">>],
            <<"resolveProvider">> => true},
      <<"definitionProvider">> =>
          true,
      <<"documentSymbolProvider">> =>
          true,
      <<"documentHighlightProvider">> =>
          true,
      <<"hoverProvider">> =>
          true,
      <<"implementationProvider">> =>
          true,
      <<"referencesProvider">> =>
          true,
      <<"renameProvider">> =>
          #{<<"prepareProvider">> => true},
      <<"foldingRangeProvider">> =>
          true,
      <<"textDocumentSync">> =>
          #{<<"openClose">> => true,
            <<"change">> => ?TEXT_DOCUMENT_SYNC_KIND_FULL,
            <<"save">> => #{<<"includeText">> => true}},
      <<"workspaceSymbolProvider">> =>
          #{<<"workDoneProgress">> => true}}.

-spec process_client_info(Params :: jsx:json_term()) -> ok.
process_client_info(#{<<"clientInfo">> := ClientInfo}) ->
    ?LOG_INFO("Client name: ~s; version: ~s",
              [maps:get(<<"name">>, ClientInfo),
               maps:get(<<"version">>, ClientInfo, <<"unspecified">>)]);
process_client_info(_) ->
    ok.

-spec process_trace(Params :: jsx:json_term()) -> ok.
process_trace(#{<<"trace">> := Trace}) ->
    ?LOG_INFO("Client requested server trace ('~s' level), ignoring... "
              "use 'log-level' start parameter instead", [Trace]);
process_trace(_) ->
    ok.

-spec process_capabilities(Params :: jsx:json_term()) -> ok.
process_capabilities(#{<<"capabilities">> := Capabilities}) ->
    ok = esrv_config:set_value(client_capabilities, Capabilities).

-spec get_proj_path(Params :: jsx:json_term()) -> path().
get_proj_path(#{<<"rootUri">> := RootUri}) ->
    esrv_lib:uri_to_path(RootUri);
get_proj_path(#{<<"rootPath">> := RootPath}) ->
    RootPath.

-spec get_init_options(Params :: jsx:json_term()) -> init_options().
get_init_options(#{<<"initializationOptions">> := InitOptions}) when is_map(InitOptions) ->
    InitOptions;
get_init_options(_) ->
    #{}.

-spec get_config_options(ProjPath :: path(), InitOptions :: init_options()) -> config_options().
get_config_options(ProjPath, InitOptions) ->
    TargetPaths = [ProjPath,
                   maps:get(<<"configPath">>, InitOptions, undefined),
                   esrv_lib:bdir_user_config()],
    ConfigFiles = add_config_files(TargetPaths, []),
    parse_config_file(ConfigFiles).

-spec add_config_files(TargetPaths :: [path()], ConfigFilesAcc :: [path()]) -> [path()].
add_config_files([], ConfigFilesAcc) ->
    lists:reverse(ConfigFilesAcc);
add_config_files([TargetPath | T], ConfigFilesAcc0) ->
    TargetNames = [<<"">>, <<"erlang_srv.config">>],
    ConfigFilesAcc1 = add_config_file(TargetNames, TargetPath, ConfigFilesAcc0),
    add_config_files(T, ConfigFilesAcc1).

-spec add_config_file(TargetNames :: [binary()],
                      TargetPath :: path(),
                      ConfigFilesAcc :: [path()]) -> [path()].
add_config_file([], _, ConfigFilesAcc) ->
    ConfigFilesAcc;
add_config_file([FileName | T], TargetPath, ConfigFilesAcc) ->
    ConfigFile = filename:join(TargetPath, FileName),
    case filelib:is_regular(ConfigFile) of
        true ->
            [ConfigFile | ConfigFilesAcc];
        false ->
            add_config_file(T, TargetPath, ConfigFilesAcc)
    end.

-spec parse_config_file(CandidatePaths :: [file:filename_all()]) -> config_options().
parse_config_file([]) ->
    ?LOG_INFO("No configuration file found"),
    #{};
parse_config_file([ConfigFile | Left]) when is_binary(ConfigFile) ->
    parse_config_file([binary_to_list(ConfigFile) | Left]);
parse_config_file([ConfigFile | Left]) ->
    ?LOG_INFO("Reading configuration: ~s", [ConfigFile]),
    try yamerl:decode_file(ConfigFile) of
        [DecodedData] ->
            normalize_decoded_data(DecodedData, #{});
        [] ->
            #{}
    catch
        T:E:S ->
            ?LOG_WARNING("Reading configuration exception; "
                         "type: ~p; error: ~p; stacktrace: ~p", [T, E, S]),
            parse_config_file(Left)
    end.

-spec normalize_decoded_data(Elements :: [any()], Acc :: config_options()) -> config_options().
normalize_decoded_data([], Acc) ->
    Acc;
normalize_decoded_data([{_, null} | T], Acc) ->
    normalize_decoded_data(T, Acc);
normalize_decoded_data([{Name, Value} | T], Acc0) ->
    Acc1 = Acc0#{list_to_binary(Name) => normalize_decoded_value(Value)},
    normalize_decoded_data(T, Acc1).

-spec normalize_decoded_value(Value :: any()) -> any().
normalize_decoded_value(Value) when is_list(Value) ->
    IsString = lists:all(fun is_integer/1, Value),
    IsKeyValue = lists:all(fun is_tuple/1, Value),
    if
        IsString ->
            unicode:characters_to_binary(Value);
        IsKeyValue ->
            normalize_decoded_data(Value, #{});
        true ->
            lists:map(fun normalize_decoded_value/1, Value)
    end;
normalize_decoded_value(Value) ->
    Value.

-spec apply_options([{Key :: atom(), Name :: binary(), Default :: any() | undefined}],
                    ConfigOptions :: config_options(),
                    InitOptions :: init_options()) -> ok.
apply_options([], _, _) ->
    ok;
apply_options([{Key, Name, Default} | T], ConfigOptions, InitOptions) ->
    if
        is_map_key(Name, ConfigOptions) ->
            Value = maps:get(Name, ConfigOptions),
            ?LOG_INFO("Applying '~s' = ~p (source: config file)", [Name, Value]),
            ok = set_value(Key, Value);
        is_map_key(Name, InitOptions) ->
            Value = maps:get(Name, InitOptions),
            ?LOG_INFO("Applying '~s' = ~p (source: init options)", [Name, Value]),
            ok = set_value(Key, Value);
        Default =/= undefined ->
            ?LOG_INFO("Applying '~s' = ~p (source: default)", [Name, Default]),
            ok = set_value(Key, Default);
        true ->
            ok
    end,
    apply_options(T, ConfigOptions, InitOptions).

-spec set_value(Key :: atom(), Value :: any()) -> ok.
set_value(Key, Value) when Value =:= <<"true">> orelse Value =:= <<"false">> ->
    set_value(Key, binary_to_atom(Value, utf8));
set_value(Key, Value) ->
    ok = esrv_config:set_value(Key, Value).

-spec init_extra_paths() -> ok.
init_extra_paths() ->
    case esrv_config:get_value(extra_paths, []) of
        ExtraPaths when is_list(ExtraPaths) ->
            lists:foreach(fun(Path) when is_binary(Path) ->
                                  code:add_path(binary_to_list(Path))
                          end, ExtraPaths);
        _ ->
            ok
    end.

-spec apply_diagnostics(ConfigOptions :: config_options(),
                        InitOptions :: init_options(),
                        DefaultDiagnostics :: [diagnostic()]) -> ok.
apply_diagnostics(ConfigOptions, InitOptions, DefaultDiagnostics) ->
    ConfigDiagnostics = maps:get(<<"diagnostics">>, ConfigOptions, []),
    InitDiagnostics = maps:get(<<"diagnostics">>, InitOptions, []),
    NamedDiagnostics =
        lists:foldl(fun({Source, Diagnostics}, Acc0) ->
                            lists:foldl(fun(Diagnostic, Acc00) ->
                                                merge_diagnostic(Source, Diagnostic, Acc00)
                                        end, Acc0, Diagnostics)
                    end, #{}, [{<<"config file">>, ConfigDiagnostics},
                               {<<"init options">>, InitDiagnostics},
                               {<<"default">>, DefaultDiagnostics}]),
    ToRegister =
        lists:filtermap(fun({Source, Diagnostic}) ->
                                Name = maps:get(<<"name">>, Diagnostic),
                                Enabled = maps:get(<<"enabled">>, Diagnostic, true),
                                Options = maps:get(<<"options">>, Diagnostic, #{}),
                                if
                                    Enabled andalso is_map(Options) ->
                                        case discover_diagnostic_module(Name) of
                                            {ok, Module} ->
                                                ?LOG_INFO("Using '~s' diagnostic (source: ~s)",
                                                          [Name, Source]),
                                                {true, {Module, Options}};
                                            undefined ->
                                                ?LOG_INFO("Skipping '~s' diagnostic: "
                                                          "module not found (source: ~s)",
                                                          [Name, Source]),
                                                false
                                        end;
                                    not Enabled ->
                                        ?LOG_INFO("Skipping '~s' diagnostic: "
                                                  "disabled (source: ~s)",
                                                  [Name, Source]),
                                        false;
                                    not is_map(Options) ->
                                        ?LOG_INFO("Skipping '~s' diagnostic: "
                                                  "bad options (source: ~s)",
                                                  [Name, Source]),
                                        false
                                end
                        end, maps:values(NamedDiagnostics)),
    esrv_diagnostics_srv:register_diagnostic_modules(ToRegister).

-spec merge_diagnostic(Source :: binary(), Diagnostic :: diagnostic(), Acc :: [diagnostic()]) ->
          [diagnostic()].
merge_diagnostic(Source, Diagnostic, Acc) ->
    case maps:get(<<"name">>, Diagnostic, undefined) of
        Name when is_binary(Name) andalso not is_map_key(Name, Acc) ->
            Acc#{Name => {Source, Diagnostic}};
        _ ->
            Acc
    end.

-spec discover_diagnostic_module(Name :: binary()) -> {ok, module()} | undefined.
discover_diagnostic_module(Name) ->
    Candidates = [<<"esrv_", Name/binary, "_diagnostics">>, Name],
    do_discover_diagnostic_module(Candidates).

-spec do_discover_diagnostic_module(Candidates :: [binary()]) -> {ok, module()} | undefined.
do_discover_diagnostic_module([]) ->
    undefined;
do_discover_diagnostic_module([Candidate | T]) ->
    Module = binary_to_atom(Candidate, utf8),
    case code:load_file(Module) of
        {module, _} ->
            {ok, Module};
        _ ->
            do_discover_diagnostic_module(T)
    end.

-spec init_predefined_macros() -> ok.
init_predefined_macros() ->
    OtpRelease = erlang:system_info(otp_release),
    PredefinedMacros0 =
        #{'MODULE' => [{atom, {1, 1}, '?MODULE?'}],
          'MODULE_STRING' => [{string, {1, 1}, "?MODULE_STRING?"}],
          'FILE' => [{atom, {1, 1}, '?FILE?'}],
          'LINE' => [{atom, {1, 1}, '?LINE?'}],
          'MACHINE' => [{atom, {1, 1}, '?MACHINE?'}],
          'FUNCTION_NAME' => [{atom, {1, 1}, '?FUNCTION_NAME?'}],
          'FUNCTION_ARITY' => [{atom, {1, 1}, '?FUNCTION_ARITY?'}],
          'OTP_RELEASE' => [{integer, {1, 1}, list_to_integer(OtpRelease)}]},
    PredefinedMacros1 =
        lists:foldl(fun(Macro, PredefinedMacros00) when is_map_key(<<"name">>, Macro) ->
                            Name = maps:get(<<"name">>, Macro),
                            MacroName = binary_to_atom(Name, utf8),
                            case maps:get(<<"value">>, Macro, true) of
                                Value when is_binary(Value) ->
                                    Text = binary_to_list(Value),
                                    case erl_scan:string(Text, {1, 1}, [text]) of
                                        {ok, Tokens, _} ->
                                            PredefinedMacros00#{MacroName => Tokens};
                                        _ ->
                                            PredefinedMacros00
                                    end;
                                Value when is_atom(Value) ->
                                    PredefinedMacros00#{MacroName => [{atom, {1, 1}, Value}]};
                                Value when is_integer(Value) ->
                                    PredefinedMacros00#{MacroName => [{integer, {1, 1}, Value}]};
                                _ ->
                                    PredefinedMacros00
                            end;
                       (_, PredefinedMacros00) ->
                            PredefinedMacros00
                    end, PredefinedMacros0, esrv_config:get_value(macros, [])),
    esrv_config:set_value(predefined_macros, PredefinedMacros1).

-spec scan_applications() -> ok.
scan_applications() ->
    ok = scan_otp_applications(),
    ok = scan_deps_applications(),
    ok = scan_sub_proj_applications(),
    ok = scan_proj_application().

-spec scan_otp_applications() -> ok.
scan_otp_applications() ->
    AppPaths = esrv_lib:scan_dirs([esrv_lib:get_otp_path(), <<"lib">>, <<"*">>]),
    lists:foreach(fun scan_otp_application/1, AppPaths).

-spec scan_otp_application(AppPath :: path()) -> ok.
scan_otp_application(AppPath) ->
    case re:run(AppPath, "^.*/(.*)-[0-9\\.]*$", [{capture, all_but_first, binary}]) of
        {match, [OtpApp]} ->
            ok = apply_scanned_application(OtpApp, otp, AppPath);
        nomatch ->
            ok
    end.

-spec scan_deps_applications() -> ok.
scan_deps_applications() ->
    Deps0 = esrv_lib:get_deps(),
    Deps1 = lists:foldl(fun(DepDir, Deps00) ->
                                ToSkip = [filename:join(DepDir, <<".rebar3">>)],
                                esrv_lib:scan_dirs([DepDir, <<"*">>], ToSkip) ++ Deps00
                        end, Deps0, esrv_lib:get_dep_dirs()),
    lists:foreach(fun(Dep) ->
                          DepApp = filename:basename(Dep),
                          apply_scanned_application(DepApp, deps, Dep)
                  end, lists:usort(Deps1)).

-spec scan_sub_proj_applications() -> ok.
scan_sub_proj_applications() ->
    SubProjs0 = esrv_lib:get_sub_projs(),
    SubProjs1 = lists:foldl(fun(SubProj, SubProjs00) ->
                                    ToSkip = [filename:join(SubProj, <<".rebar3">>)],
                                    esrv_lib:scan_dirs([SubProj, <<"*">>], ToSkip) ++ SubProjs00
                            end, SubProjs0, esrv_lib:get_sub_proj_dirs()),
    lists:foreach(fun(SubProj) ->
                          SubProjApp = filename:basename(SubProj),
                          apply_scanned_application(SubProjApp, sub_proj, SubProj)
                  end, lists:usort(SubProjs1)).

-spec scan_proj_application() -> ok.
scan_proj_application() ->
    {ok, ProjPath} = esrv_config:get_value(proj_path),
    ProjApp = filename:basename(ProjPath),
    store_scanned_application(ProjApp, proj, ProjPath).

-spec apply_scanned_application(Name :: binary(), Type :: app_type(), Path :: path()) -> ok.
apply_scanned_application(Name, Type, Path) ->
    AppsIgnore = esrv_config:get_value(apps_ignore, []),
    case lists:member(Name, AppsIgnore) of
        false ->
            store_scanned_application(Name, Type, Path);
        true ->
            ?LOG_DEBUG("Application '~s' (~p) skipped", [Name])
    end.

-spec store_scanned_application(AppName :: binary(),
                                AppType :: app_type(),
                                AppPath :: path()) -> ok.
store_scanned_application(AppName, AppType, AppPath) ->
    AppId = binary_to_atom(AppName, utf8),
    case esrv_db:read_scanned_app(AppId) of
        [#scanned_app{type = PrevType}] ->
            ?LOG_WARNING("Application '~s' (~p overwrites ~p) scanned: ~s",
                         [AppName, AppType, PrevType, AppPath]);
        [] ->
            ?LOG_DEBUG("Application '~s' (~p) scanned: ~s", [AppName, AppType, AppPath])
    end,
    esrv_db:write_scanned_app(AppId, AppType, AppPath).

-spec index_applications(Token :: lp_token() | undefined) -> ok.
index_applications(Token) ->
    TargetPaths = esrv_lib:get_target_paths(),
    ok = notify_start(Token),
    lists:foreach(fun({AppType, Percentage}) ->
                          ok = notify_indexing(Token, Percentage, AppType),
                          ok = index_applications(AppType, TargetPaths)
                  end, [{otp, 20}, {deps, 40}, {sub_proj, 60}, {proj, 80}]),
    ok = notify_end(Token).

-spec index_applications(AppType :: app_type(), TargetPaths :: [path()]) -> ok.
index_applications(otp, _) ->
    ScannedApps = esrv_db:read_scanned_app_by_type(otp),
    case esrv_otp_node_controller:get_otp_node() of
        {ok, OtpNode} ->
            ok = rpc:call(OtpNode,
                          esrv_otp_node_request_srv,
                          ensure_indexed,
                          [ScannedApps]);
        undefined ->
            esrv_index_mgr:process_applications(ScannedApps, [])
    end;
index_applications(AppType, TargetPaths) ->
    ScannedApps = esrv_db:read_scanned_app_by_type(AppType),
    esrv_index_mgr:process_applications(ScannedApps, TargetPaths).

-spec notify_start(Token :: lp_token() | undefined) -> ok.
notify_start(undefined) ->
    ok;
notify_start(Token) ->
    Notification =
        #notification{method = <<"$/progress">>,
                      params = [{<<"token">>, Token},
                                {<<"value">>, [{<<"kind">>, 'begin'},
                                               {<<"title">>, <<"Indexing project">>},
                                               {<<"cancellable">>, false},
                                               {<<"percentage">>, 0}]}]},
    ok = esrv_main_fsm:notification(Notification).

-spec notify_indexing(Token :: lp_token() | undefined,
                      Percentage :: integer(),
                      Type :: app_type()) -> ok.
notify_indexing(undefined, _, _) ->
    ok;
notify_indexing(Token, Percentage, Type) ->
    Message = io_lib:format("Indexing '~s' modules", [Type]),
    Notification =
        #notification{method = <<"$/progress">>,
                      params = [{<<"token">>, Token},
                                {<<"value">>, [{<<"kind">>, report},
                                               {<<"message">>, iolist_to_binary(Message)},
                                               {<<"percentage">>, Percentage}]}]},
    ok = esrv_main_fsm:notification(Notification).

-spec notify_end(Token :: lp_token() | undefined) -> ok.
notify_end(undefined) ->
    ok;
notify_end(Token) ->
    Notification =
        #notification{method = <<"$/progress">>,
                      params = [{<<"token">>, Token},
                                {<<"value">>, [{<<"kind">>, 'end'},
                                               {<<"message">>, <<"Project indexed">>}]}]},
    ok = esrv_main_fsm:notification(Notification).
