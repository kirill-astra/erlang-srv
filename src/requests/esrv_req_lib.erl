-module(esrv_req_lib).

-export([get_module_name/2,
         get_module_name/3,
         get_local_function/2,
         get_local_function/3,
         get_remote_function/2,
         get_remote_function/3,
         get_exported/2,
         get_exported/3,
         get_exported/4,
         get_local_type/2,
         get_local_type/3,
         get_remote_type/2,
         get_remote_type/3,
         get_exported_type/2,
         get_exported_type/3,
         get_exported_type/4,
         get_spec_data/2,
         get_spec_data/3,
         get_record_data/2,
         get_record_data/3,
         get_deeply/2,
         get_deeply/3,
         get_deeply/4]).

-export([collect_local_function/1,
         collect_local_function/2,
         collect_remote_function/1,
         collect_remote_function/2,
         collect_local_function_name/3,
         collect_remote_function_name/2,
         collect_remote_function_name/3,
         collect_exported/1,
         collect_exported/2,
         collect_imported/1,
         collect_imported/2,
         collect_local_type/1,
         collect_local_type/2,
         collect_remote_type/1,
         collect_remote_type/2,
         collect_local_type_name/3,
         collect_remote_type_name/2,
         collect_remote_type_name/3,
         collect_exported_type/1,
         collect_exported_type/2,
         collect_macros/1,
         collect_macros/2,
         collect_records/1,
         collect_records/2,
         collect_deeply/3,
         collect_deeply/4,
         collect_deeply/5]).

-export([find_module_data/1,
         traverse_by_definition/3,
         traverse_by_definition/4,
         traverse_modules/3,
         traverse_proj_modules/2,
         traverse_active_modules/2,
         get_poi/3,
         get_poi/4,
         collect_pois/2,
         collect_pois/3,
         find_end_location/2,
         form_signature/2,
         get_zone/2,
         get_bits/0,
         check_location_range/4,
         check_line_range/4,
         format_name_arity/2,
         format_atom/1,
         format_macro_name/1,
         format_macro_id/1,
         format_macro_name_arity/2,
         format_location/2,
         format_location/3]).

-include("types.hrl").
-include("records.hrl").

%%%-------------------------------------------------------------------
%%% Deep getters
%%%-------------------------------------------------------------------
-type get_result() :: {ok, any()} | undefined.

-spec get_module_name(Uri :: uri(), ModuleData :: module_data()) ->
          {ok, {uri(), {module(), location()}}} | undefined.
get_module_name(Uri, ModuleData) ->
    {_, Result} = get_module_name(Uri, ModuleData, []),
    Result.

-spec get_module_name(Uri :: uri() | undefined,
                      ModuleData :: module_data(),
                      ToSkip :: [uri()]) ->
          {[uri()], {ok, {uri() | undefined, {module(), location()}}} | undefined}.
get_module_name(Uri, ModuleData, ToSkip) ->
    get_deeply(Uri, fun(U, MD) -> do_module_name(U, MD) end, ModuleData, ToSkip).

-spec do_module_name(Uri :: uri() | undefined, ModuleData :: module_data()) ->
          {ok, {uri() | undefined, {module(), location()}}} | undefined.
do_module_name(_, #module_data{module_name = undefined}) ->
    undefined;
do_module_name(Uri, #module_data{module_name = ModuleName}) ->
    {ok, {Uri, ModuleName}}.

-spec get_local_function(ModuleName :: module(), NameArity :: name_arity()) ->
          {ok, uri(), function_data()} | undefined.
get_local_function(ModuleName, NameArity) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            get_local_function(Uri, ModuleData, NameArity);
        undefined ->
            undefined
    end.

-spec get_local_function(Uri :: uri(), ModuleData :: module_data(), NameArity :: name_arity()) ->
          {ok, {uri(), function_data()}} | undefined.
get_local_function(Uri, ModuleData, NameArity) ->
    Imports = collect_imported(Uri, ModuleData),
    get_local_function(Uri, ModuleData, NameArity, Imports).

-spec get_local_function(Uri :: uri(),
                         ModuleData :: module_data(),
                         NameArity :: name_arity(),
                         Imports :: [import()]) -> {ok, {uri(), function_data()}} | undefined.
get_local_function(Uri, ModuleData, NameArity, Import) ->
    get_deeply(Uri, fun(U, MD) -> do_function(U, MD, NameArity, Import) end, ModuleData).

-spec get_remote_function(ModuleName :: module(), NameArity :: name_arity()) ->
          {ok, {uri(), function_data()}} | undefined.
get_remote_function(ModuleName, NameArity) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            get_remote_function(Uri, ModuleData, NameArity);
        undefined ->
            undefined
    end.

-spec get_remote_function(Uri :: uri(), ModuleData :: module_data(), NameArity :: name_arity()) ->
          {ok, {uri(), function_data()}} | undefined.
get_remote_function(Uri, ModuleData, NameArity) ->
    case get_exported(Uri, ModuleData, NameArity) of
        true ->
            get_deeply(Uri, fun(U, MD) -> do_function(U, MD, NameArity, []) end, ModuleData);
        false ->
            undefined
    end.

-spec do_function(Uri :: uri(),
                  ModuleData :: module_data(),
                  NameArity :: name_arity(),
                  Imports :: [import()]) -> {ok, {uri(), function_data()}} | undefined.
do_function(Uri, ModuleData, NameArity, Imports) ->
    case ModuleData of
        #module_data{functions = #{NameArity := FunctionData}} ->
            {ok, {Uri, FunctionData}};
        _ ->
            case find_imported(NameArity, Imports) of
                {ok, _, UriFunctionData} ->
                    {ok, UriFunctionData};
                undefined ->
                    undefined
            end
    end.

-spec get_exported(ModuleName :: module(), NameArity :: name_arity()) -> boolean().
get_exported(ModuleName, NameArity) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            get_exported(Uri, ModuleData, NameArity);
        undefined ->
            false
    end.

-spec get_exported(Uri :: uri(), ModuleData :: module_data(), NameArity :: name_arity()) ->
          boolean().
get_exported(Uri, ModuleData, NameArity) ->
    {_, Result} = get_exported(Uri, ModuleData, NameArity, []),
    Result.

-spec get_exported(Uri :: uri(),
                   ModuleData :: module_data(),
                   NameArity :: name_arity(),
                   ToSkip :: [uri()]) -> {[uri()], boolean()}.
get_exported(Uri, ModuleData, NameArity, ToSkip0) ->
    case get_deeply(Uri, fun(_, MD) -> do_exported(MD, NameArity) end, ModuleData, ToSkip0) of
        {ToSkip1, {ok, true}} ->
            {ToSkip1, true};
        {ToSkip1, undefined} ->
            {ToSkip1, false}
    end.

-spec do_exported(ModuleData :: module_data(), NameArity :: name_arity()) ->
          {ok, true} | undefined.
do_exported(#module_data{export_all = true}, _) ->
    {ok, true};
do_exported(#module_data{export = Export}, NameArity) ->
    case lists:member(NameArity, Export) of
        true ->
            {ok, true};
        false ->
            undefined
    end.

-spec get_local_type(ModuleName :: module(), NameArity :: name_arity()) ->
          {ok, {uri(), location()}} | undefined.
get_local_type(ModuleName, NameArity) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            get_local_type(Uri, ModuleData, NameArity);
        undefined ->
            undefined
    end.

-spec get_local_type(Uri :: uri(), ModuleData :: module_data(), NameArity :: name_arity()) ->
          {ok, {uri(), location()}} | undefined.
get_local_type(Uri, ModuleData, NameArity) ->
    get_deeply(Uri, fun(U, MD) -> do_type(U, MD, NameArity) end, ModuleData).

-spec get_remote_type(ModuleName :: module(), NameArity :: name_arity()) ->
          {ok, {uri(), location()}} | undefined.
get_remote_type(ModuleName, NameArity) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            get_remote_type(Uri, ModuleData, NameArity);
        undefined ->
            undefined
    end.

-spec get_remote_type(Uri :: uri(), ModuleData :: module_data(), NameArity :: name_arity()) ->
          {ok, {uri(), location()}} | undefined.
get_remote_type(Uri, ModuleData, NameArity) ->
    case get_exported_type(Uri, ModuleData, NameArity) of
        true ->
            get_deeply(Uri, fun(U, MD) -> do_type(U, MD, NameArity) end, ModuleData);
        false ->
            undefined
    end.

-spec do_type(Uri :: uri(),
              ModuleData :: module_data(),
              NameArity :: name_arity()) -> {ok, {uri(), location()}} | undefined.
do_type(Uri, ModuleData, NameArity) ->
    case ModuleData of
        #module_data{types = #{NameArity := Location}} ->
            {ok, {Uri, Location}};
        _ ->
            undefined
    end.

-spec get_exported_type(ModuleName :: module(), NameArity :: name_arity()) -> boolean().
get_exported_type(ModuleName, NameArity) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            get_exported_type(Uri, ModuleData, NameArity);
        undefined ->
            false
    end.

-spec get_exported_type(Uri :: uri(), ModuleData :: module_data(), NameArity :: name_arity()) ->
          boolean().
get_exported_type(Uri, ModuleData, NameArity) ->
    {_, Result} = get_exported_type(Uri, ModuleData, NameArity, []),
    Result.

-spec get_exported_type(Uri :: uri(),
                        ModuleData :: module_data(),
                        NameArity :: name_arity(),
                        ToSkip :: [uri()]) -> {[uri()], boolean()}.
get_exported_type(Uri, ModuleData, NameArity, ToSkip0) ->
    case get_deeply(Uri, fun(_, MD) -> do_exported_type(MD, NameArity) end, ModuleData, ToSkip0) of
        {ToSkip1, {ok, true}} ->
            {ToSkip1, true};
        {ToSkip1, undefined} ->
            {ToSkip1, false}
    end.

-spec do_exported_type(ModuleData :: module_data(), NameArity :: name_arity()) ->
          {ok, true} | undefined.
do_exported_type(#module_data{export_all = true}, _) ->
    {ok, true};
do_exported_type(#module_data{export_type = ExportType}, NameArity) ->
    case lists:member(NameArity, ExportType) of
        true ->
            {ok, true};
        false ->
            undefined
    end.

-spec get_spec_data(ModuleName :: module(), NameArity :: name_arity()) ->
          {ok, {uri(), spec_data()}} | undefined.
get_spec_data(ModuleName, NameArity) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            get_spec_data(Uri, ModuleData, NameArity);
        undefined ->
            undefined
    end.

-spec get_spec_data(Uri :: uri() | undefined,
                    ModuleData :: module_data(),
                    NameArity :: name_arity()) ->
          {ok, {uri() | undefined, spec_data()}} | undefined.
get_spec_data(Uri, ModuleData, NameArity) ->
    get_deeply(Uri, fun(U, MD) -> do_spec_data(U, MD, NameArity) end, ModuleData).

-spec do_spec_data(Uri :: uri() | undefined,
                   ModuleData :: module_data(),
                   NameArity :: name_arity()) ->
          {ok, {uri() | undefined, spec_data()}} | undefined.
do_spec_data(Uri, ModuleData, NameArity) ->
    case find_spec_data(NameArity, ModuleData) of
        {ok, SpecData} ->
            {ok, {Uri, SpecData}};
        undefined ->
            undefined
    end.

-spec get_record_data(ModuleName :: module(), RecordName :: name()) ->
          {ok, {uri(), record_data()}} | undefined.
get_record_data(ModuleName, RecordName) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            get_record_data(Uri, ModuleData, RecordName);
        undefined ->
            undefined
    end.

-spec get_record_data(Uri :: uri() | undefined,
                      ModuleData :: module_data(),
                      RecordName :: name()) ->
          {ok, {uri() | undefined, record_data()}} | undefined.
get_record_data(Uri, ModuleData, RecordName) ->
    get_deeply(Uri, fun(U, MD) -> do_record_data(U, MD, RecordName) end, ModuleData).

-spec do_record_data(Uri :: uri() | undefined,
                     ModuleData :: module_data(),
                     RecordName :: name()) ->
          {ok, {uri() | undefined, record_data()}} | undefined.
do_record_data(Uri, #module_data{records = Records}, RecordName) ->
    case maps:find(RecordName, Records) of
        {ok, RecordData} ->
            {ok, {Uri, RecordData}};
        error ->
            undefined
    end.

-spec get_deeply(Finder :: fun((uri() | undefined, module_data()) -> get_result()),
                 ModuleData :: module_data()) -> get_result().
get_deeply(Finder, ModuleData) ->
    get_deeply(undefined, Finder, ModuleData).

-spec get_deeply(Uri :: uri() | undefined,
                 Finder :: fun((uri() | undefined, module_data()) -> get_result()),
                 ModuleData :: module_data()) -> get_result().
get_deeply(Uri, Finder, ModuleData) ->
    {_, Result} = get_deeply(Uri, Finder, ModuleData, []),
    Result.

-spec get_deeply(Uri :: uri() | undefined,
                 Finder :: fun((uri() | undefined, module_data()) -> get_result()),
                 ModuleData :: module_data(),
                 ToSkip :: [uri()]) -> {[uri()], get_result()}.
get_deeply(Uri, Finder, ModuleData, ToSkip) ->
    get_deeply2(Uri, Finder, ModuleData, ToSkip, []).

-spec get_deeply2(Uri :: uri() | undefined,
                  Finder :: fun((uri() | undefined, module_data()) -> get_result()),
                  ModuleData :: module_data(),
                  ToSkip :: [uri()],
                  Chain :: [uri()]) -> {[uri()], get_result()}.
get_deeply2(Uri, Finder, ModuleData, ToSkip, Chain) ->
    case lists:member(Uri, ToSkip) of
        false ->
            case lists:member(Uri, Chain) of
                false ->
                    get_deeply3(Uri, Finder, ModuleData, ToSkip, Chain);
                true ->
                    {ToSkip, undefined}
            end;
        true ->
            {ToSkip, undefined}
    end.

-spec get_deeply3(Uri :: uri() | undefined,
                  Finder :: fun((uri() | undefined, module_data()) -> get_result()),
                  ModuleData :: module_data(),
                  ToSkip :: [uri()],
                  Chain :: [uri()]) -> {[uri()], get_result()}.
get_deeply3(Uri, Finder, ModuleData, ToSkip0, Chain) ->
    case Finder(Uri, ModuleData) of
        {ok, Value} ->
            {ToSkip0, {ok, Value}};
        undefined ->
            #module_data{include_data = #include_data{resolved = Resolved}} = ModuleData,
            {ToSkip1, Result} =
                lists:foldl(fun(_, {ToSkip00, {ok, Data}}) ->
                                    {ToSkip00, {ok, Data}};
                               (IncludedUri, {ToSkip00, undefined}) ->
                                    case esrv_lib:get_module_data(IncludedUri) of
                                        {ok, IncludedModuleData} ->
                                            get_deeply2(IncludedUri,
                                                        Finder,
                                                        IncludedModuleData,
                                                        ToSkip00,
                                                        [Uri | Chain]);
                                        undefined ->
                                            {ToSkip00, undefined}
                                    end
                            end, {ToSkip0, undefined}, maps:keys(Resolved)),
            case Result of
                {ok, _} ->
                    {ToSkip1, Result};
                undefined ->
                    {[Uri | ToSkip1], undefined}
            end
    end.

%%%-------------------------------------------------------------------
%%% Deep collectors
%%%-------------------------------------------------------------------
-type collect_acc() :: any().

-spec collect_local_function(ModuleName :: module()) -> [{name_arity(), signature()}].
collect_local_function(ModuleName) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            collect_local_function(Uri, ModuleData);
        undefined ->
            []
    end.

-spec collect_local_function(Uri :: uri() | undefined, ModuleData :: module_data()) ->
          [{name_arity(), signature()}].
collect_local_function(Uri, ModuleData) ->
    Collector = fun(_, MD, A) -> do_collect_function_and_signature(MD, A, all) end,
    collect_deeply(Uri, Collector, ModuleData, []).

-spec collect_remote_function(ModuleName :: module()) -> [{name_arity(), signature()}].
collect_remote_function(ModuleName) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            collect_remote_function(Uri, ModuleData);
        undefined ->
            []
    end.

-spec collect_remote_function(Uri :: uri() | undefined, ModuleData :: module_data()) ->
          [{name_arity(), signature()}].
collect_remote_function(Uri, ModuleData) ->
    Exported = collect_exported(Uri, ModuleData),
    Collector = fun(_, MD, A) -> do_collect_function_and_signature(MD, A, Exported) end,
    collect_deeply(Uri, Collector, ModuleData, []).

-spec do_collect_function_and_signature(ModuleData :: module_data(),
                                        Acc :: [{name_arity(), signature()}],
                                        Filter :: [name_arity()] | all) ->
          [{name_arity(), signature()}].
do_collect_function_and_signature(ModuleData, Acc0, Filter) ->
    lists:foldl(fun(NameArity, Acc00) ->
                        [{NameArity, form_signature(NameArity, ModuleData)} | Acc00]
                end, Acc0, do_collect_function(ModuleData, [], Filter)).

-spec do_collect_function(ModuleData :: module_data(),
                          Acc :: [name_arity()],
                          Filter :: [name_arity()] | all) -> [name_arity()].
do_collect_function(#module_data{functions = Functions}, Acc, all) ->
    maps:keys(Functions) ++ Acc;
do_collect_function(#module_data{functions = Functions}, Acc0, Filter) ->
    lists:foldl(fun(NameArity, Acc00) ->
                        case lists:member(NameArity, Filter) of
                            true ->
                                [NameArity | Acc00];
                            false ->
                                Acc00
                        end
                end, Acc0, maps:keys(Functions)).

-spec collect_local_function_name(Uri :: uri(),
                                  ModuleData :: module_data(),
                                  FunctionName :: name()) ->
          [{arity(), {uri(), function_data()}}].
collect_local_function_name(Uri, ModuleData, FunctionName) ->
    Collector = fun(U, MD, A) -> do_collect_function_name(U, MD, A, all, FunctionName) end,
    Elements = collect_deeply(Uri, Collector, ModuleData, []),
    lists:keysort(1, Elements).

-spec collect_remote_function_name(ModuleName :: module(), FunctionName :: name()) ->
          [{arity(), {uri(), function_data()}}].
collect_remote_function_name(ModuleName, FunctionName) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            collect_remote_function_name(Uri, ModuleData, FunctionName);
        undefined ->
            []
    end.

-spec collect_remote_function_name(Uri :: uri(),
                                   ModuleData :: module_data(),
                                   FunctionName :: name()) ->
          [{arity(), {uri(), function_data()}}].
collect_remote_function_name(Uri, ModuleData, FunctionName) ->
    Exported = collect_exported(Uri, ModuleData),
    Collector = fun(U, MD, A) -> do_collect_function_name(U, MD, A, Exported, FunctionName) end,
    Elements = collect_deeply(Uri, Collector, ModuleData, []),
    lists:keysort(1, Elements).

-spec do_collect_function_name(Uri :: uri(),
                               ModuleData :: module_data(),
                               Acc :: [{arity(), {uri(), location()}}],
                               Filter :: [name_arity()] | all,
                               TypeName :: name()) -> [{arity(), {uri(), function_data()}}].
do_collect_function_name(Uri, ModuleData, Acc0, Filter, TypeName) ->
    #module_data{functions = Functions} = ModuleData,
    lists:foldl(fun({Name, Arity}, Acc00) when Name =:= TypeName ->
                        FunctionData = maps:get({Name, Arity}, Functions),
                        [{Arity, {Uri, FunctionData}} | Acc00];
                   (_, Acc00) ->
                        Acc00
                end, Acc0, do_collect_function(ModuleData, [], Filter)).

-spec collect_exported(ModuleName :: module()) -> [name_arity()] | all.
collect_exported(ModuleName) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            collect_exported(Uri, ModuleData);
        undefined ->
            []
    end.

-spec collect_exported(Uri :: uri(), ModuleData :: module_data()) -> [name_arity()] | all.
collect_exported(Uri, ModuleData) ->
    Collector = fun(_, MD, A) -> do_collect_exported(MD, A) end,
    collect_deeply(Uri, Collector, ModuleData, []).

-spec do_collect_exported(ModuleData :: module_data(), Acc :: [name_arity()] | all) ->
          [name_arity()] | all.
do_collect_exported(_, all) ->
    all;
do_collect_exported(#module_data{export_all = true}, _) ->
    all;
do_collect_exported(#module_data{export = Export}, Acc) ->
    Export ++ Acc.

-spec collect_imported(ModuleName :: module()) -> [import()].
collect_imported(ModuleName) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            collect_imported(Uri, ModuleData);
        undefined ->
            false
    end.

-spec collect_imported(Uri :: uri() | undefined, ModuleData :: module_data()) -> [import()].
collect_imported(Uri, ModuleData) ->
    collect_deeply(Uri, fun(_, MD, A) -> do_collect_imported(MD, A) end, ModuleData, []).

-spec do_collect_imported(ModuleData :: module_data(), Acc :: [import()]) -> [import()].
do_collect_imported(#module_data{import = Import}, Acc) ->
    Import ++ Acc.

-spec collect_local_type(ModuleName :: module()) -> [name_arity()].
collect_local_type(ModuleName) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            collect_local_type(Uri, ModuleData);
        undefined ->
            []
    end.

-spec collect_local_type(Uri :: uri() | undefined, ModuleData :: module_data()) -> [name_arity()].
collect_local_type(Uri, ModuleData) ->
    Collector = fun(_, MD, A) -> do_collect_type(MD, A, all) end,
    collect_deeply(Uri, Collector, ModuleData, []).

-spec collect_remote_type(ModuleName :: module()) -> [name_arity()].
collect_remote_type(ModuleName) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            collect_remote_type(Uri, ModuleData);
        undefined ->
            []
    end.

-spec collect_remote_type(Uri :: uri(), ModuleData :: module_data()) -> [name_arity()].
collect_remote_type(Uri, ModuleData) ->
    ExportedType = collect_exported_type(Uri, ModuleData),
    Collector = fun(_, MD, A) -> do_collect_type(MD, A, ExportedType) end,
    collect_deeply(Uri, Collector, ModuleData, []).

-spec do_collect_type(ModuleData :: module_data(),
                      Acc :: [name_arity()],
                      Filter :: [name_arity()] | all) -> [name_arity()].
do_collect_type(#module_data{types = Types}, Acc, all) ->
    maps:keys(Types) ++ Acc;
do_collect_type(#module_data{types = Types}, Acc0, Filter) ->
    lists:foldl(fun(NameArity, Acc00) ->
                        case lists:member(NameArity, Filter) of
                            true ->
                                [NameArity | Acc00];
                            false ->
                                Acc00
                        end
                end, Acc0, maps:keys(Types)).

-spec collect_local_type_name(Uri :: uri(), ModuleData :: module_data(), TypeName :: name()) ->
          [{arity(), {uri(), location()}}].
collect_local_type_name(Uri, ModuleData, TypeName) ->
    Collector = fun(U, MD, A) -> do_collect_type_name(U, MD, A, all, TypeName) end,
    Elements = collect_deeply(Uri, Collector, ModuleData, []),
    lists:keysort(1, Elements).

-spec collect_remote_type_name(ModuleName :: module(), TypeName :: name()) ->
          [{arity(), {uri(), location()}}].
collect_remote_type_name(ModuleName, TypeName) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            collect_remote_type_name(Uri, ModuleData, TypeName);
        undefined ->
            []
    end.

-spec collect_remote_type_name(Uri :: uri(),
                               ModuleData :: module_data(),
                               TypeName :: name()) -> [{arity(), {uri(), location()}}].
collect_remote_type_name(Uri, ModuleData, TypeName) ->
    ExportedType = collect_exported_type(Uri, ModuleData),
    Collector = fun(U, MD, A) -> do_collect_type_name(U, MD, A, ExportedType, TypeName) end,
    Elements = collect_deeply(Uri, Collector, ModuleData, []),
    lists:keysort(1, Elements).

-spec do_collect_type_name(Uri :: uri(),
                           ModuleData :: module_data(),
                           Acc :: [{arity(), {uri(), location()}}],
                           Filter :: [name_arity()] | all,
                           TypeName :: name()) -> [{arity(), {uri(), location()}}].
do_collect_type_name(Uri, ModuleData, Acc0, Filter, TypeName) ->
    #module_data{types = Types} = ModuleData,
    lists:foldl(fun({Name, Arity}, Acc00) when Name =:= TypeName ->
                        Location = maps:get({Name, Arity}, Types),
                        [{Arity, {Uri, Location}} | Acc00];
                   (_, Acc00) ->
                        Acc00
                end, Acc0, do_collect_type(ModuleData, [], Filter)).

-spec collect_exported_type(ModuleName :: module()) -> [name_arity()] | all.
collect_exported_type(ModuleName) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            collect_exported_type(Uri, ModuleData);
        undefined ->
            []
    end.

-spec collect_exported_type(Uri :: uri(), ModuleData :: module_data()) -> [name_arity()] | all.
collect_exported_type(Uri, ModuleData) ->
    Collector = fun(_, MD, A) -> do_collect_exported_type(MD, A) end,
    collect_deeply(Uri, Collector, ModuleData, []).

-spec do_collect_exported_type(ModuleData :: module_data(), Acc :: [name_arity()] | all) ->
          [name_arity()] | all.
do_collect_exported_type(_, all) ->
    all;
do_collect_exported_type(#module_data{export_all = true}, _) ->
    all;
do_collect_exported_type(#module_data{export_type = ExportType}, Acc) ->
    ExportType ++ Acc.

-spec collect_macros(ModuleName :: module()) -> #{uri() => macros()}.
collect_macros(ModuleName) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            collect_macros(Uri, ModuleData);
        undefined ->
            []
    end.

-spec collect_macros(Uri :: uri(), ModuleData :: module_data()) -> #{uri() => macros()}.
collect_macros(Uri, ModuleData) ->
    Collector = fun(U, MD, A) -> do_collect_macros(U, MD, A) end,
    collect_deeply(Uri, Collector, ModuleData, #{}).

-spec do_collect_macros(Uri :: uri(), ModuleData :: module_data(), Acc :: #{uri() => macros()}) ->
          #{uri() => macros()}.
do_collect_macros(Uri, #module_data{macros = Macros}, Acc) ->
    maps:update_with(Uri,
                     fun(MacrosAcc) ->
                             maps:merge(Macros, MacrosAcc)
                     end, Macros, Acc).

-spec collect_records(ModuleName :: module()) -> #{name() => record_data()}.
collect_records(ModuleName) ->
    case find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            collect_records(Uri, ModuleData);
        undefined ->
            []
    end.

-spec collect_records(Uri :: uri() | undefined, ModuleData :: module_data()) ->
          #{name() => record_data()}.
collect_records(Uri, ModuleData) ->
    Collector = fun(_, MD, A) -> do_collect_records(MD, A) end,
    collect_deeply(Uri, Collector, ModuleData, #{}).

-spec do_collect_records(ModuleData :: module_data(), Acc :: #{name() => record_data()}) ->
          #{name() => record_data()}.
do_collect_records(#module_data{records = Records}, Acc) ->
    maps:merge(Records, Acc).

-spec collect_deeply(Collector :: fun((uri() | undefined,
                                       module_data(),
                                       collect_acc()) -> collect_acc()),
                     ModuleData :: module_data(),
                     Acc :: collect_acc()) -> collect_acc().
collect_deeply(Collector, ModuleData, Acc) ->
    collect_deeply(undefined, Collector, ModuleData, Acc).

-spec collect_deeply(Uri :: uri() | undefined,
                     Collector :: fun((uri() | undefined,
                                       module_data(),
                                       collect_acc()) -> collect_acc()),
                     ModuleData :: module_data(),
                     Acc :: collect_acc()) -> collect_acc().
collect_deeply(Uri, Collector, ModuleData, Acc) ->
    {_, Result} = collect_deeply(Uri, Collector, ModuleData, [], Acc),
    Result.

-spec collect_deeply(Uri :: uri() | undefined,
                     Collector :: fun((uri() | undefined,
                                       module_data(),
                                       collect_acc()) -> collect_acc()),
                     ModuleData :: module_data(),
                     UniqUris :: [uri()],
                     Acc :: collect_acc()) -> {[uri()], collect_acc()}.
collect_deeply(Uri, Collector, ModuleData, UniqAcc0, Acc0) ->
    #module_data{include_data = #include_data{resolved = Resolved}} = ModuleData,
    lists:foldl(fun(IncludedUri, {UniqAcc00, Acc00}) ->
                        case lists:member(IncludedUri, UniqAcc00) of
                            false ->
                                case esrv_lib:get_module_data(IncludedUri) of
                                    {ok, IncludedModuleData} ->
                                        collect_deeply(IncludedUri,
                                                       Collector,
                                                       IncludedModuleData,
                                                       UniqAcc00,
                                                       Acc00);
                                    undefined ->
                                        {UniqAcc00, Acc00}
                                end;
                            true ->
                                {UniqAcc00, Acc00}
                        end
                end, {[Uri | UniqAcc0], Collector(Uri, ModuleData, Acc0)}, maps:keys(Resolved)).

%%%-------------------------------------------------------------------
%%% Other
%%%-------------------------------------------------------------------
-spec find_module_data(ModuleName :: name()) -> {ok, uri(), module_data()} | undefined.
find_module_data(ModuleName) ->
    case esrv_db:get_module_data_by_name(ModuleName) of
        {ok, Uri, PersistentModuleData} ->
            case esrv_index_mgr:get_current_module_data(Uri) of
                {ok, VolatileModuleData} ->
                    {ok, Uri, VolatileModuleData};
                undefined ->
                    {ok, Uri, PersistentModuleData}
            end;
        undefined ->
            undefined
    end.

-type traverse_processor() :: fun(({uri(), module_data()}, any()) -> any()).
-type traverse_fetcher() :: fun(() -> [{uri(), module_data()}]).

-spec traverse_by_definition(Processor :: traverse_processor(),
                             DefinitionUri :: uri(),
                             InitAcc :: any()) -> any().
traverse_by_definition(Processor, DefinitionUri, Acc) ->
    traverse_by_definition(false, Processor, DefinitionUri, Acc).

-spec traverse_by_definition(OnlyProjModules :: boolean(),
                             Processor :: traverse_processor(),
                             DefinitionUri :: uri(),
                             InitAcc :: any()) -> any().
traverse_by_definition(OnlyProjModules, Processor, DefinitionUri, Acc0) ->
    {Acc1, _} =
        do_traverse_modules(fun({U, MD}, {Acc00, IncludedAcc00}) ->
                                    {DefinitionIncluded, IncludedAcc01} =
                                        check_included(DefinitionUri, U, MD, IncludedAcc00),
                                    {case DefinitionIncluded of
                                         true ->
                                             Processor({U, MD}, Acc00);
                                         false ->
                                             Acc00
                                     end, IncludedAcc01}
                            end,
                            case OnlyProjModules of
                                true ->
                                    fun esrv_db:get_all_proj_module_data/0;
                                false ->
                                    fun esrv_db:get_all_active_module_data/0
                            end,
                            {Acc0, #{}}),
    Acc1.

-spec check_included(DefinitionUri :: uri(),
                     Uri :: uri(),
                     ModuleData :: module_data(),
                     IncludedAcc :: #{uri() => boolean()}) -> {boolean(), #{uri() => boolean()}}.
check_included(Uri, Uri, _, IncludedAcc) ->
    {true, IncludedAcc};
check_included(DefinitionUri, _, ModuleData, IncludedAcc0) ->
    #module_data{include_data = #include_data{resolved = Resolved}} = ModuleData,
    lists:foldl(fun(_, {true, IncludedAcc00}) ->
                        {true, IncludedAcc00};
                   (IncludedUri, {false, IncludedAcc00}) ->
                        case maps:find(IncludedUri, IncludedAcc00) of
                            {ok, DefinitionIncluded} ->
                                {DefinitionIncluded, IncludedAcc00};
                            error ->
                                case esrv_lib:get_module_data(IncludedUri) of
                                    {ok, IncludedModuleData} ->
                                        {DefinitionIncluded, IncludedAcc01} =
                                            check_included(DefinitionUri,
                                                           IncludedUri,
                                                           IncludedModuleData,
                                                           IncludedAcc00),
                                        {DefinitionIncluded,
                                         IncludedAcc01#{IncludedUri => DefinitionIncluded}};
                                    undefined ->
                                        {false, IncludedAcc00}
                                end
                        end
                end, {false, IncludedAcc0}, maps:keys(Resolved)).

-spec traverse_modules(OnlyProjModules :: boolean(),
                       Processor :: traverse_processor(),
                       Acc :: any()) -> any().
traverse_modules(true, Processor, Acc) ->
    traverse_proj_modules(Processor, Acc);
traverse_modules(false, Processor, Acc) ->
    traverse_active_modules(Processor, Acc).

-spec traverse_proj_modules(Processor :: traverse_processor(), Acc :: any()) -> any().
traverse_proj_modules(Processor, Acc) ->
    do_traverse_modules(Processor, fun esrv_db:get_all_proj_module_data/0, Acc).

-spec traverse_active_modules(Processor :: traverse_processor(), Acc :: any()) -> any().
traverse_active_modules(Processor, Acc) ->
    do_traverse_modules(Processor, fun esrv_db:get_all_active_module_data/0, Acc).

-spec do_traverse_modules(Processor :: traverse_processor(),
                          Fetcher :: traverse_fetcher(),
                          Acc :: any()) -> any().
do_traverse_modules(Processor, Fetcher, Acc0) ->
    lists:foldl(fun({Uri, PersistentModuleData}, Acc00) ->
                        case esrv_index_mgr:get_current_module_data(Uri) of
                            {ok, VolatileModuleData} ->
                                Processor({Uri, VolatileModuleData}, Acc00);
                            undefined ->
                                Processor({Uri, PersistentModuleData}, Acc00)
                        end
                end, Acc0, Fetcher()).

-spec get_poi(Location :: location(), Uri :: uri(), ModuleData :: module_data()) ->
          {ok, poi()} | undefined.
get_poi(Location, Uri, ModuleData) ->
    get_poi(true, Location, Uri, ModuleData).

-spec get_poi(ResolveModuleMacro :: boolean(),
              Location :: location(),
              Uri :: uri(),
              ModuleData :: module_data()) -> {ok, poi()} | undefined.
get_poi(ResolveModuleMacro, Location, Uri, #module_data{pois = {GbTree, _}} = ModuleData) ->
    Iterator = gb_trees:iterator_from(Location, GbTree),
    case gb_trees:next(Iterator) of
        {EndLocation, #poi{data = PoiData, start_location = StartLocation} = Poi, _} ->
            case check_location_range(Location, {StartLocation, EndLocation}, ge, lt) of
                true ->
                    case PoiData of
                        {macro, {'MODULE', undefined}} when not ResolveModuleMacro ->
                            {ok, Poi};
                        {local_function, _} ->
                            Imports = collect_imported(Uri, ModuleData),
                            {ok, resolve_poi(Poi, Imports, Uri, ModuleData)};
                        _ ->
                            {ok, resolve_poi(Poi, [], Uri, ModuleData)}
                    end;
                false ->
                    undefined
            end;
        none ->
            undefined
    end.

-spec collect_pois(Uri :: uri(), ModuleData :: module_data()) -> [{location(), poi()}].
collect_pois(Uri, ModuleData) ->
    collect_pois(true, Uri, ModuleData).

-spec collect_pois(ResolveModuleMacro :: boolean(),
                   Uri :: uri(),
                   ModuleData :: module_data()) -> [{location(), poi()}].
collect_pois(ResolveModuleMacro, Uri, #module_data{pois = {GbTree, _}} = ModuleData) ->
    Imports = collect_imported(Uri, ModuleData),
    lists:map(fun({EndLocation, #poi{data = {macro, {'MODULE', undefined}}} = Poi})
                    when not ResolveModuleMacro ->
                      {EndLocation, Poi};
                 ({EndLocation, Poi}) ->
                      {EndLocation, resolve_poi(Poi, Imports, Uri, ModuleData)}
              end, gb_trees:to_list(GbTree)).

-spec resolve_poi(Poi :: poi(),
                  Imports :: [import()],
                  Uri :: uri(),
                  ModuleData :: module_data()) -> poi().
resolve_poi(#poi{data = {local_function, {Name, Arity}}} = Poi0, Imports, Uri, ModuleData) ->
    IsBif = erl_internal:bif(Name, Arity),
    case find_imported({Name, Arity}, Imports) of
        {ok, ModuleName, _} ->
            PoiData = {remote_function, ModuleName, {Name, Arity}},
            maybe_change_poi({Name, Arity}, Uri, ModuleData, PoiData, Poi0);
        undefined when IsBif =:= true ->
            PoiData = {remote_function, erlang, {Name, Arity}},
            maybe_change_poi({Name, Arity}, Uri, ModuleData, PoiData, Poi0);
        undefined ->
            Poi0
    end;

resolve_poi(#poi{data = {macro, {'MODULE', undefined}}} = Poi0, _, Uri, ModuleData) ->
    case get_module_name(Uri, ModuleData) of
        {ok, {_, {ModuleName, _}}} ->
            Poi0#poi{data = {module, ModuleName}};
        undefined ->
            Poi0
    end;

resolve_poi(#poi{data = {Tag, '?MODULE?', NameArity}} = Poi0, _, Uri, ModuleData)
  when Tag =:= remote_type orelse Tag =:= remote_function ->
    case get_module_name(Uri, ModuleData) of
        {ok, {_, {ModuleName, _}}} ->
            Poi0#poi{data = {Tag, ModuleName, NameArity}};
        undefined ->
            Poi0
    end;

resolve_poi(Poi, _, _, _) ->
    Poi.

-spec maybe_change_poi(NameArity :: name_arity(),
                       Uri :: uri(),
                       ModuleData :: module_data(),
                       PoiData :: poi_data(),
                       Poi :: poi()) -> poi().
maybe_change_poi(NameArity, Uri, ModuleData, PoiData, Poi0) ->
    case get_local_function(Uri, ModuleData, NameArity, []) of
        undefined ->
            Poi0#poi{data = PoiData};
        _ ->
            Poi0
    end.

-spec find_end_location(StartLocation :: location(), Moduledata :: module_data()) -> location().
find_end_location(StartLocation, #module_data{pois = {GbTree, _}}) ->
    Iterator = gb_trees:iterator_from(StartLocation, GbTree),
    {EndLocation, _, _} = gb_trees:next(Iterator),
    EndLocation.

-spec find_imported(NameArity :: name_arity(), Imports :: [import()]) ->
          {ok, module(), {uri(), function_data()}} | undefined.
find_imported(_, []) ->
    undefined;
find_imported(NameArity, [{ModuleName, NameArity} | T]) ->
    find_imported(NameArity, [ModuleName | T]);
find_imported(NameArity, [ModuleName | T]) ->
    case get_remote_function(ModuleName, NameArity) of
        {ok, UriFunctionData} ->
            {ok, ModuleName, UriFunctionData};
        undefined ->
            find_imported(NameArity, T)
    end.

-spec form_signature(NameArity :: name_arity(), ModuleData :: module_data()) -> signature().
form_signature({Name, Arity}, #module_data{functions = Functions} = ModuleData) ->
    Signature0 = lists:duplicate(Arity, none),
    Signature1 =
        case find_spec_data({Name, Arity}, ModuleData) of
            {ok, #spec_data{signatures = SpecSignatures}} ->
                merge_signatures(SpecSignatures, Signature0);
            undefined ->
                Signature0
        end,
    Signature2 =
        case maps:find({Name, Arity}, Functions) of
            {ok, #function_data{signatures = FunctionSignatures}} ->
                merge_signatures(FunctionSignatures, Signature1);
            error ->
                Signature1
        end,
    finalize_signature(Signature2).

-spec merge_signatures(ToMerge :: [signature()], Signature :: signature()) -> signature().
merge_signatures([], Signature) ->
    Signature;
merge_signatures([Signature | T], Signature0) ->
    Signature1 = merge_signature(Signature, Signature0),
    merge_signatures(T, Signature1).

-spec merge_signature(ToMerge :: signature(), Signature :: signature()) -> signature().
merge_signature(ToMerge, Signature) ->
    lists:map(fun({ToMergeName, SignatureName})
                    when SignatureName =:= none andalso ToMergeName =/= none ->
                      ToMergeName;
                 ({_, SignatureName}) ->
                      SignatureName
              end, lists:zip(ToMerge, Signature)).

-spec finalize_signature(Signature :: signature()) -> signature().
finalize_signature(Signature) ->
    finalize_signature(Signature, 1).

-spec finalize_signature(Names :: [name() | none], N :: pos_integer()) -> signature().
finalize_signature([], _) ->
    [];
finalize_signature([none | T], N) ->
    [list_to_atom("Arg" ++ integer_to_list(N)) | finalize_signature(T, N + 1)];
finalize_signature([Name | T], N) ->
    [Name | finalize_signature(T, N + 1)].

-spec find_spec_data(NameArity :: name_arity(), ModuleData :: module_data()) ->
          {ok, spec_data()} | undefined.
find_spec_data(NameArity, #module_data{local_specs = LocalSpecs} = ModuleData) ->
    case maps:find(NameArity, LocalSpecs) of
        {ok, SpecData} ->
            {ok, SpecData};
        error ->
            find_remote_spec_data(NameArity, ModuleData)
    end.

-spec find_remote_spec_data(NameArity :: name_arity(), ModuleData :: module_data()) ->
          {ok, spec_data()} | undefined.
find_remote_spec_data(NameArity, #module_data{module_name = {Module, _},
                                              remote_specs = RemoteSpecs})
  when is_map_key(Module, RemoteSpecs) ->
    ModuleSpecs = maps:get(Module, RemoteSpecs),
    case maps:find(NameArity, ModuleSpecs) of
        {ok, SpecData} ->
            {ok, SpecData};
        error ->
            undefined
    end;
find_remote_spec_data(_, _) ->
    undefined.

-spec get_zone(Location :: location(), Zones :: [zone()]) -> {ok, #zone{}} | undefined.
get_zone(_, []) ->
    undefined;
get_zone(Location, [#zone{start_location = StartLocation,
                          end_location = EndLocation} = Zone | T]) ->
    case check_location_range(Location, {StartLocation, EndLocation}, gt, le) of
        true ->
            {ok, Zone};
        false ->
            get_zone(Location, T)
    end.

-spec get_bits() -> proplists:proplist().
get_bits() ->
    [{any, 0},
     {arity, 0},
     {atom, 0},
     {binary, 0},
     {bitstring, 0},
     {boolean, 0},
     {byte, 0},
     {char, 0},
     {float, 0},
     {'fun', 0},
     {'fun', 1},
     {function, 0},
     {identifier, 0},
     {integer, 0},
     {iodata, 0},
     {iolist, 0},
     {list, 0},
     {list, 1},
     {map, 0},
     {maybe_improper_list, 0},
     {maybe_improper_list, 2},
     {mfa, 0},
     {module, 0},
     {neg_integer, 0},
     {nil, 0},
     {no_return, 0},
     {node, 0},
     {non_neg_integer, 0},
     {nonempty_improper_list, 2},
     {nonempty_list, 0},
     {nonempty_list, 1},
     {nonempty_string, 0},
     {none, 0},
     {number, 0},
     {pid, 0},
     {port, 0},
     {pos_integer, 0},
     {reference, 0},
     {string, 0},
     {term, 0},
     {timeout, 0},
     {tuple, 0}].

-type operation() :: lt | le | gt | ge.

-spec check_location_range(Location :: location(),
                           LocationRange :: location_range(),
                           LowerBoundOperation :: operation(),
                           UpperBoundOperation :: operation()) -> boolean().
check_location_range(Location, {StartLocation, EndLocation},
                     LowerBoundOperation, UpperBoundOperation) ->
    Condition1 = compare_location(Location, StartLocation, LowerBoundOperation),
    Condition2 = compare_location(Location, EndLocation, UpperBoundOperation),
    Condition1 andalso Condition2.

-spec check_line_range(Location :: location(),
                       LineRange :: line_range(),
                       LowerBoundOperation :: operation(),
                       UpperBoundOperation :: operation()) -> boolean().
check_line_range(Location, {StartLine, EndLine}, LowerBoundOperation, UpperBoundOperation) ->
    check_location_range(Location, {{StartLine, 1}, {EndLine + 1, 1}},
                         LowerBoundOperation, UpperBoundOperation).

-spec compare_location(LocationA :: location(),
                       LocationB :: location(),
                       Operation :: operation()) -> boolean().
compare_location({Line, Column1}, {Line, Column2}, Operation) ->
    apply_operation(Column1, Column2, Operation);
compare_location({Line1, _}, {Line2, _}, Operation) ->
    apply_operation(Line1, Line2, Operation).

-spec apply_operation(Element1 :: any(), Element2 :: any(), Operation :: operation()) -> boolean().
apply_operation(Element1, Element2, lt) ->
    Element1 < Element2;
apply_operation(Element1, Element2, le) ->
    Element1 =< Element2;
apply_operation(Element1, Element2, gt) ->
    Element1 > Element2;
apply_operation(Element1, Element2, ge) ->
    Element1 >= Element2.

-spec format_name_arity(Name :: name(), Arity :: arity()) -> binary().
format_name_arity(Name, Arity) ->
    iolist_to_binary([format_atom(Name), <<"/">>, integer_to_binary(Arity)]).

-spec format_atom(Atom :: atom()) -> binary().
format_atom(Atom) ->
    iolist_to_binary(io_lib:format("~p", [Atom])).

-spec format_macro_name(Name :: name()) -> binary().
format_macro_name(Name) ->
    LowercasedName = string:lowercase(atom_to_binary(Name, utf8)),
    NormalizedName = binary_to_atom(binary:replace(LowercasedName, <<"_">>, <<>>, [global]), utf8),
    case atom_to_binary(NormalizedName, utf8) =:= format_atom(NormalizedName) of
        true ->
            atom_to_binary(Name, utf8);
        false ->
            format_atom(Name)
    end.

-spec format_macro_id(MacroId :: macro_id()) -> binary().
format_macro_id({Name, Arity}) ->
    format_macro_name_arity(Name, Arity).

-spec format_macro_name_arity(Name :: name(), Arity :: macro_arity()) -> binary().
format_macro_name_arity(Name, undefined) ->
    format_macro_name(Name);
format_macro_name_arity(Name, Arity) ->
    iolist_to_binary([format_macro_name(Name), <<"/">>, integer_to_binary(Arity)]).

-spec format_location(Uri :: uri(), Location :: location()) -> jsx:json_term().
format_location(Uri, Location) ->
    format_location(Uri, Location, Location).

-spec format_location(Uri :: uri(),
                      StartLocation :: location(),
                      EndLocation :: location()) -> jsx:json_term().
format_location(Uri, StartLocation, EndLocation) ->
    [{<<"uri">>, Uri},
     {<<"range">>, esrv_lib:format_range(StartLocation, EndLocation)}].
