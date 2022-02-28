-module(esrv_db).

-include("types.hrl").
-include("parser.hrl").
-include("records.hrl").
-include("log.hrl").

%% API
-export([read_parsed_module/1,
         write_parsed_module/3,
         read_module_meta/1,
         read_module_meta_by_name/1,
         write_module_meta/4,
         read_proj_module/1,
         write_proj_module/3,
         check_cache/2,
         read_cache/1,
         write_cache/2,
         delete_cache/1,
         get_module_data/1,
         get_module_data_by_name/1,
         get_all_module_name/0,
         get_all_proj_module_data/0,
         get_all_behaviors/0,
         read_text_document/1,
         text_document_transaction/2,
         read_loaded_module/1,
         write_loaded_module/2]).

-export([initialize/0]).

-define(CURRENT_VERSION, <<"1">>).
-define(VERSION_FILE_NAME, <<"version">>).
-define(MAX_WAIT_FOR_TABLES, 60000).
-define(MODULE_DATA_EXPIRED_AFTER, 15*60*1000).

%%==============================================================================
%% API
%%==============================================================================
-spec read_parsed_module(Uri :: uri()) -> [parsed_module()].
read_parsed_module(Uri) ->
    mnesia:dirty_read(parsed_module, Uri).

-spec write_parsed_module(Uri :: uri(), Hash :: hash(), ModuleData :: module_data()) -> ok.
write_parsed_module(Uri, Hash, ModuleData) ->
    ParsedModule = #parsed_module{uri = Uri,
                                  hash = Hash,
                                  parser_version = ?PARSER_VERSION,
                                  module_data = ModuleData},
    ok = mnesia:dirty_write(ParsedModule).

-spec read_module_meta(Uri :: uri()) -> [module_meta()].
read_module_meta(Uri) ->
    mnesia:dirty_read(module_meta, Uri).

-spec read_module_meta_by_name(ModuleName :: name()) -> [module_meta()].
read_module_meta_by_name(ModuleName) ->
    mnesia:dirty_index_read(module_meta, ModuleName, #module_meta.module).

-spec write_module_meta(Uri :: uri(),
                        Hash :: hash(),
                        ModuleType :: module_type(),
                        ModuleData :: module_data()) -> ok.
write_module_meta(Uri, Hash, ModuleType, ModuleData) ->
    Module = case ModuleData of #module_data{module_name = {M, _}} -> M; _ -> undefined end,
    case ModuleType of
        proj ->
            write_proj_module(Uri, Module, ModuleData);
        _ ->
            cache_otp_deps_behaviors(ModuleData)
    end,
    ActiveModule = #module_meta{uri = Uri, hash = Hash, module = Module, module_type = ModuleType},
    ok = mnesia:dirty_write(ActiveModule).

-spec read_proj_module(Uri :: uri()) -> [proj_module()].
read_proj_module(Uri) ->
    mnesia:dirty_read(proj_module, Uri).

-spec write_proj_module(Uri :: uri(),
                        Module :: module() | undefined,
                        ModuleData :: module_data()) -> ok.
write_proj_module(Uri, Module, ModuleData) ->
    ProjModule = #proj_module{uri = Uri, module = Module, module_data = ModuleData},
    mnesia:dirty_write(ProjModule).

-spec check_cache(Key :: any(), Fun :: fun(() -> {cache, any()} |
                                                 {cache, any(), integer()} |
                                                 {no_cache, any()})) -> any().
check_cache(Key, Fun) ->
    case read_cache(Key) of
        [#cache{value = Value}] ->
            Value;
        [] ->
            case Fun() of
                {cache, Value} ->
                    write_cache(Key, Value),
                    Value;
                {cache, Value, ExpiredAfter} ->
                    write_cache(Key, Value, ExpiredAfter),
                    Value;
                {no_cache, Value} ->
                    Value
            end
    end.

-spec read_cache(Key :: any()) -> [cache()].
read_cache(Key) ->
    mnesia:dirty_read(cache, Key).

-spec write_cache(Key :: any(), Value :: any()) -> ok.
write_cache(Key, Value) ->
    write_cache(Key, Value, infinity).

-spec write_cache(Key :: any(), Value :: any(), ExpiredAfter :: integer() | infinity) -> ok.
write_cache(Key, Value, ExpiredAfter) ->
    Cache = #cache{key = Key,
                   value = Value,
                   start_timestamp = erlang:system_time(millisecond),
                   expired_after = ExpiredAfter},
    ok = mnesia:dirty_write(Cache).

-spec delete_cache(Key :: any()) -> ok.
delete_cache(Key) ->
    ok = mnesia:dirty_delete(cache, Key).

-spec get_module_data(Uri :: uri()) -> {ok, module_data()} | undefined.
get_module_data(Uri) ->
    Meta = read_module_meta(Uri),
    get_module_data_by_meta(Meta).

-spec get_module_data_by_name(ModuleName :: name()) -> {ok, uri(), module_data()} | undefined.
get_module_data_by_name(ModuleName) ->
    Meta = read_module_meta_by_name(ModuleName),
    case get_module_data_by_meta(Meta) of
        {ok, ModuleData} ->
            [#module_meta{uri = Uri} | _] = Meta,
            {ok, Uri, ModuleData};
        undefined ->
            undefined
    end.

-spec get_module_data_by_meta(Meta :: [module_meta()]) -> {ok, module_data()} | undefined.
get_module_data_by_meta([#module_meta{uri = Uri, module_type = proj} | _]) ->
    case read_proj_module(Uri) of
        [#proj_module{module_data = ModuleData}] ->
            {ok, ModuleData};
        [] ->
            undefined
    end;
get_module_data_by_meta([#module_meta{uri = Uri} | _]) ->
    check_cache({module_data, Uri},
                fun() ->
                        case read_parsed_module(Uri) of
                            [#parsed_module{module_data = ModuleData}] ->
                                {cache, {ok, ModuleData}, ?MODULE_DATA_EXPIRED_AFTER};
                            [] ->
                                {no_cache, undefined}
                        end
                end);
get_module_data_by_meta([]) ->
    undefined.

-spec get_all_module_name() -> [module()].
get_all_module_name() ->
    OtpDepsModules = check_cache(otp_deps_modules, fun() -> {cache, get_otp_deps_modules()} end),
    lists:foldl(fun(Key, Acc) ->
                        case mnesia:dirty_read(proj_module, Key) of
                            [#proj_module{module = Module}] when Module =/= undefined ->
                                [Module | Acc];
                            _ ->
                                Acc
                        end
                end, OtpDepsModules, mnesia:dirty_all_keys(proj_module)).

-spec get_otp_deps_modules() -> [module()].
get_otp_deps_modules() ->
    lists:foldl(fun(Key, Acc) ->
                        case mnesia:dirty_read(module_meta, Key) of
                            [#module_meta{module = Module, module_type = ModuleType}]
                              when Module =/= undefined andalso ModuleType =/= proj ->
                                [Module | Acc];
                            _ ->
                                Acc
                        end
                end, [], mnesia:dirty_all_keys(module_meta)).

-spec get_all_proj_module_data() -> [{uri(), module_data()}].
get_all_proj_module_data() ->
    lists:foldl(fun(Key, Acc) ->
                        case mnesia:dirty_read(proj_module, Key) of
                            [#proj_module{uri = Uri, module_data = ModuleData}] ->
                                [{Uri, ModuleData} | Acc];
                            _ ->
                                Acc
                        end
                end, [], mnesia:dirty_all_keys(proj_module)).

-spec get_all_behaviors() -> [name()].
get_all_behaviors() ->
    OtpDepsBehaviors = check_cache(otp_deps_behaviors, fun() -> {no_cache, sets:new()} end),
    lists:foldl(fun(Key, Acc) ->
                        case mnesia:dirty_read(proj_module, Key) of
                            [#proj_module{module_data = #module_data{behaviors = Behaviors}}]
                              when map_size(Behaviors) > 0 ->
                                maps:keys(Behaviors) ++ Acc;
                            _ ->
                                Acc
                        end
                end, sets:to_list(OtpDepsBehaviors), mnesia:dirty_all_keys(proj_module)).

-spec cache_otp_deps_behaviors(ModuleData :: module_data()) -> ok.
cache_otp_deps_behaviors(#module_data{behaviors = Behaviors}) when map_size(Behaviors) > 0 ->
    OtpDepsBehaviors0 = check_cache(otp_deps_behaviors, fun() -> {no_cache, sets:new()} end),
    OtpDepsBehaviors1 =
        lists:foldl(fun(Behavior, OtpDepsBehaviors00) ->
                            sets:add_element(Behavior, OtpDepsBehaviors00)
                    end, OtpDepsBehaviors0, maps:keys(Behaviors)),
    ok = write_cache(otp_deps_behaviors, OtpDepsBehaviors1);
cache_otp_deps_behaviors(_) ->
    ok.

-spec read_text_document(Uri :: uri()) -> [text_document()].
read_text_document(Uri) ->
    mnesia:dirty_read(text_document, Uri).

-spec text_document_transaction(Uri :: uri(), Fun :: fun(([text_document()]) -> any())) -> any().
text_document_transaction(Uri, Fun) ->
    {atomic, R} = mnesia:transaction(fun() -> Fun(mnesia:read(text_document, Uri, write)) end),
    R.

-spec read_loaded_module(Uri :: uri()) -> [loaded_module()].
read_loaded_module(Uri) ->
    mnesia:dirty_read(loaded_module, Uri).

-spec write_loaded_module(Uri :: uri(), Hash :: hash()) -> ok.
write_loaded_module(Uri, Hash) ->
    LoadedModule = #loaded_module{uri = Uri, hash = Hash},
    ok = mnesia:dirty_write(LoadedModule).

%%------------------------------------------------------------------------------
%% Initialization
%%------------------------------------------------------------------------------
-spec initialize() -> ok.
initialize() ->
    MnesiaDir = mnesia_dir(),
    ok = application:set_env(mnesia, dir, MnesiaDir),
    case read_version(MnesiaDir) of
        ?CURRENT_VERSION ->
            ?LOG_INFO("No need to prepare mnesia schema: ~p", [MnesiaDir]),
            ok = mnesia:start(),
            ok = wait_for_tables();
        _ ->
            ?LOG_INFO("Preparing mnesia schema: ~p", [MnesiaDir]),
            ok = esrv_lib:rm_rf(MnesiaDir),
            ok = filelib:ensure_dir(MnesiaDir),
            ok = mnesia:create_schema([node()]),
            ok = mnesia:start(),
            ok = create_tables(),
            ok = write_version(MnesiaDir, ?CURRENT_VERSION)
    end.

-spec read_version(MnesiaDir :: file:filename()) -> binary().
read_version(MnesiaDir) ->
    VersionFile = filename:join(MnesiaDir, ?VERSION_FILE_NAME),
    case file:read_file(VersionFile) of
        {ok, Version} ->
            Version;
        _ ->
            <<>>
    end.

-spec write_version(MnesiaDir :: file:filename(), Version :: binary()) -> ok.
write_version(MnesiaDir, Version) ->
    VersionFile = filename:join(MnesiaDir, ?VERSION_FILE_NAME),
    ok = file:write_file(VersionFile, Version).

-spec create_tables() -> ok.
create_tables() ->
    lists:foreach(fun({Table, Options}) ->
                          {atomic, ok} = mnesia:create_table(Table, Options)
                  end, tables_info()).

-spec wait_for_tables() -> ok.
wait_for_tables() ->
    Tables = lists:map(fun({Table, _}) -> Table end, tables_info()),
    ok = mnesia:wait_for_tables(Tables, ?MAX_WAIT_FOR_TABLES).

-spec tables_info() -> [{table(), proplists:proplist()}].
tables_info() ->
    [
     {parsed_module, [{attributes, record_info(fields, parsed_module)},
                      {disc_only_copies, [node()]}]},
     {module_meta, [{attributes, record_info(fields, module_meta)},
                    {index, [module]}]},
     {proj_module, [{attributes, record_info(fields, proj_module)},
                    {index, [module]}]},
     {text_document, [{attributes, record_info(fields, text_document)}]},
     {loaded_module, [{attributes, record_info(fields, loaded_module)}]},
     {cache, [{attributes, record_info(fields, cache)}]}
    ].

-spec mnesia_dir() -> file:filename().
mnesia_dir() ->
    MnesiaPath = filename:join([esrv_lib:bdir_user_cache(), "mnesia", node()]),
    binary_to_list(MnesiaPath).
