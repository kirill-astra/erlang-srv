-module(esrv_req_td_references).

-behaviour(esrv_request_processor).

%% API
-export([process/1]).

-include("types.hrl").
-include("parser.hrl").
-include("records.hrl").
-include("log.hrl").

-type item() :: [{uri(), location(), location()}].

-spec process(Request :: request()) -> {response, response()}.
process(#request{id = Id, params = #{<<"position">> := #{<<"line">> := Line,
                                                         <<"character">> := Character},
                                     <<"textDocument">> := #{<<"uri">> := Uri}}}) ->
    {ok, ModuleData} = esrv_index_mgr:get_current_module_data(Uri),
    OnlyProjModules = check_only_proj_modules(Uri),
    Items =
        case esrv_req_lib:get_poi({Line + 1, Character + 1}, Uri, ModuleData) of
            {ok, #poi{data = {macro, _}} = Poi} ->
                process_macro(OnlyProjModules, Uri, ModuleData, Poi);
            {ok, #poi{data = {include, _} = PoiData}} ->
                process_plain(OnlyProjModules, PoiData);
            {ok, #poi{data = {include_lib, _} = PoiData}} ->
                process_plain(OnlyProjModules, PoiData);
            {ok, #poi{data = {module, _} = PoiData}} ->
                process_plain(OnlyProjModules, PoiData);
            {ok, #poi{data = {behavior, _} = PoiData}} ->
                process_plain(OnlyProjModules, PoiData);
            {ok, #poi{data = {callback, NameArity}}} ->
                process_callback(OnlyProjModules, NameArity, ModuleData);
            {ok, #poi{data = {record, RecordName}}} ->
                process_record(OnlyProjModules, Uri, ModuleData, RecordName);
            {ok, #poi{data = {field, RecordName, FieldName}}} ->
                process_field(OnlyProjModules, Uri, ModuleData, RecordName, FieldName);
            {ok, #poi{data = {local_type, NameArity}}} ->
                process_local_type(OnlyProjModules, Uri, ModuleData, NameArity);
            {ok, #poi{data = {remote_type, ModuleName, NameArity}}} ->
                process_remote_type(OnlyProjModules, ModuleName, NameArity);
            {ok, #poi{data = {local_spec, NameArity}}} ->
                process_local_function(OnlyProjModules, Uri, ModuleData, NameArity);
            {ok, #poi{data = {remote_spec, ModuleName, NameArity}}} ->
                process_remote_function(OnlyProjModules, ModuleName, NameArity);
            {ok, #poi{data = {local_function, NameArity}}} ->
                process_local_function(OnlyProjModules, Uri, ModuleData, NameArity);
            {ok, #poi{data = {function_clause, NameArity}}} ->
                process_local_function(OnlyProjModules, Uri, ModuleData, NameArity);
            {ok, #poi{data = {remote_function, ModuleName, NameArity}}} ->
                process_remote_function(OnlyProjModules, ModuleName, NameArity);
            _ ->
                []
        end,
    Result =
        lists:map(fun({RefUri, StartLocation, EndLocation}) ->
                          esrv_req_lib:format_location(RefUri, StartLocation, EndLocation)
                  end, lists:usort(Items)),
    {response, #response{id = Id, result = Result}}.


-spec check_only_proj_modules(Uri :: uri()) -> boolean().
check_only_proj_modules(Uri) ->
    case esrv_db:read_module_meta(Uri) of
        [#module_meta{app_type = proj}] ->
            true;
        _ ->
            false
    end.

-spec process_macro(OnlyProjModules :: boolean(),
                    Uri :: uri(),
                    ModuleData :: module_data(),
                    Poi :: poi()) -> [item()].
process_macro(OnlyProjModules, _, _, #poi{data = PoiData, definition = {remote, Uri, _}}) ->
    process_definition(OnlyProjModules, PoiData, Uri);
process_macro(OnlyProjModules, Uri, _, #poi{data = PoiData, definition = {local, _}}) ->
    process_definition(OnlyProjModules, PoiData, Uri);
process_macro(_, Uri, ModuleData, #poi{data = PoiData, definition = undefined}) ->
    collect_pois(Uri, ModuleData, [PoiData], []).

-spec process_callback(OnlyProjModules :: boolean(),
                       NameArity :: name_arity(),
                       ModuleData :: module_data()) -> [item()].
process_callback(OnlyProjModules, NameArity, #module_data{module_name = {ModuleName, _}}) ->
    Processor = fun({Uri, #module_data{behaviors = Behaviors} = ModuleData}, Acc) ->
                        case maps:find(ModuleName, Behaviors) of
                            {ok, _} ->
                                add_callback_definition(Uri, ModuleData, NameArity, Acc);
                            error ->
                                Acc
                        end
                end,
    esrv_req_lib:traverse_modules(OnlyProjModules, Processor, []);
process_callback(_, _, _) ->
    [].

-spec add_callback_definition(Uri :: uri(),
                              ModuleData :: module_data(),
                              NameArity :: name_arity(),
                              Acc :: [item()]) -> [item()].
add_callback_definition(Uri, ModuleData, NameArity, Acc0) ->
    case esrv_req_lib:get_remote_function(Uri, ModuleData, NameArity) of
        {ok, {DefinitionUri, _}} ->
            add_callback_definition2(DefinitionUri, NameArity, Acc0);
        undefined ->
            Acc0
    end.

-spec add_callback_definition2(Uri :: uri(),
                               NameArity :: name_arity(),
                               Acc :: [item()]) -> [item()].
add_callback_definition2(Uri, NameArity, Acc0) ->
    case esrv_lib:get_module_data(Uri) of
        {ok, #module_data{pois = {GbTree, _}}} ->
            lists:foldl(fun({EndLocation, Poi}, Acc00) ->
                                case Poi of
                                    #poi{data = {local_function, NameArity},
                                         start_location = StartLocation,
                                         definition = {local, StartLocation}} ->
                                        [{Uri, StartLocation, EndLocation} | Acc00];
                                    _ ->
                                        Acc00
                                end
                        end, Acc0, gb_trees:to_list(GbTree));
        undefined ->
            Acc0
    end.

-spec process_record(OnlyProjModules :: boolean(),
                     Uri :: uri(),
                     ModuleData :: module_data(),
                     RecordName :: name()) -> [item()].
process_record(OnlyProjModules, Uri, ModuleData, RecordName) ->
    case esrv_req_lib:get_record_data(Uri, ModuleData, RecordName) of
        {ok, {DefinitionUri, _}} ->
            process_definition(OnlyProjModules, {record, RecordName}, DefinitionUri);
        undefined ->
            []
    end.

-spec process_field(OnlyProjModules :: boolean(),
                    Uri :: uri(),
                    ModuleData :: module_data(),
                    RecordName :: name(),
                    FieldName :: name()) -> [item()].
process_field(OnlyProjModules, Uri, ModuleData, RecordName, FieldName) ->
    case esrv_req_lib:get_record_data(Uri, ModuleData, RecordName) of
        {ok, {DefinitionUri, _}} ->
            process_definition(OnlyProjModules, {field, RecordName, FieldName}, DefinitionUri);
        undefined ->
            []
    end.

-spec process_local_type(OnlyProjModules :: boolean(),
                         Uri :: uri(),
                         ModuleData :: module_data(),
                         NameArity :: name_arity()) -> [item()].
process_local_type(OnlyProjModules, Uri, ModuleData, NameArity) ->
    IsBit =
        case ModuleData of
            #module_data{types = Types} when is_map_key(NameArity, Types) ->
                false;
            _ ->
                Bits = esrv_req_lib:get_bits(),
                lists:member(NameArity, Bits)
        end,
    case IsBit of
        true ->
            process_bit_calls(OnlyProjModules, NameArity);
        false ->
            case esrv_req_lib:get_local_type(Uri, ModuleData, NameArity) of
                {ok, {DefinitionUri, _}} ->
                    add_type_items(OnlyProjModules, DefinitionUri, NameArity, sets:new());
                undefined ->
                    collect_type_pois(Uri, ModuleData, [{local_type, NameArity}], [])
            end
    end.

-spec process_remote_type(OnlyProjModules :: boolean(),
                          ModuleName :: module(),
                          NameArity :: name_arity()) -> [item()].
process_remote_type(OnlyProjModules, ModuleName, NameArity) ->
    case esrv_req_lib:get_remote_type(ModuleName, NameArity) of
        {ok, {DefinitionUri, _}} ->
            ModulesSet = sets:from_list([ModuleName]),
            add_type_items(OnlyProjModules, DefinitionUri, NameArity, ModulesSet);
        undefined ->
            esrv_req_lib:traverse_modules(OnlyProjModules,
                                          fun({U, MD}, Acc) ->
                                                  ToCollect = [{remote_type,
                                                                ModuleName,
                                                                NameArity}],
                                                  collect_type_pois(U, MD, ToCollect, Acc)
                                          end,
                                          [])
    end.

-spec process_bit_calls(OnlyProjModules :: boolean(), NameArity :: name_arity()) -> [item()].
process_bit_calls(OnlyProjModules, NameArity) ->
    Processor = fun({U, MD}, Acc) ->
                        ToCollect = [{local_type, NameArity}],
                        collect_type_pois(U, MD, ToCollect, Acc)
                end,
    esrv_req_lib:traverse_modules(OnlyProjModules, Processor, []).

-spec add_type_items(OnlyProjModules :: boolean(),
                     DefinitionUri :: uri(),
                     NameArity :: name_arity(),
                     ModulesSet :: sets:set(module())) -> [item()].
add_type_items(OnlyProjModules, DefinitionUri, NameArity, ModulesSet0) ->
    Processor =
        fun({U, MD}, {Items00, ModulesSet00, ExportedToSkip00, ModuleToSkip00}) ->
                Items01 = collect_type_pois(U, MD, [{local_type, NameArity}], Items00),
                {ExportedToSkip01, ExportedType} =
                    esrv_req_lib:get_exported_type(U, MD, NameArity, ExportedToSkip00),
                case ExportedType of
                    true ->
                        {ModuleToSkip01, ModuleNameData} =
                            esrv_req_lib:get_module_name(U, MD, ModuleToSkip00),
                        case ModuleNameData of
                            {ok, {_, {ModuleName, _}}} ->
                                {Items01,
                                 sets:add_element(ModuleName, ModulesSet00),
                                 ExportedToSkip01,
                                 ModuleToSkip01};
                            undefined ->
                                {Items01, ModulesSet00, ExportedToSkip01, ModuleToSkip01}
                        end;
                    false ->
                        {Items01, ModulesSet00, ExportedToSkip01, ModuleToSkip00}
                end
        end,
    {Items1, ModulesSet1, _, _} =
        esrv_req_lib:traverse_by_definition(OnlyProjModules,
                                            Processor,
                                            DefinitionUri,
                                            {[], ModulesSet0, [], []}),
    case sets:is_empty(ModulesSet1) of
        false ->
            ToCollect =
                sets:fold(fun(ModuleName, Acc) ->
                                  [{remote_type, ModuleName, NameArity} | Acc]
                          end, [], ModulesSet1),
            esrv_req_lib:traverse_modules(OnlyProjModules,
                                          fun({U, MD}, Items10) ->
                                                  collect_type_pois(U, MD, ToCollect, Items10)
                                          end,
                                          Items1);
        true ->
            Items1
    end.

-spec collect_type_pois(Uri :: uri(),
                        ModuleData :: module_data(),
                        ToCollect :: [poi_data()],
                        Acc :: [item()]) -> [item()].
collect_type_pois(Uri, ModuleData, ToCollect, Acc) ->
    #module_data{zones = Zones} = ModuleData,
    lists:filter(fun({_, StartLocation, _}) ->
                         case esrv_req_lib:get_zone(StartLocation, Zones) of
                             {ok, #zone{type = Type}} when Type =:= type orelse
                                                           Type =:= record_body orelse
                                                           Type =:= spec_body ->
                                 true;
                             _ ->
                                 false
                         end
                 end, collect_pois(Uri, ModuleData, ToCollect, [])) ++ Acc.

-spec process_local_function(OnlyProjModules :: boolean(),
                             Uri :: uri(),
                             ModuleData :: module_data(),
                             NameArity :: name_arity()) -> [item()].
process_local_function(OnlyProjModules, Uri, ModuleData, NameArity) ->
    IsBif =
        case ModuleData of
            #module_data{functions = Functions} when is_map_key(NameArity, Functions) ->
                false;
            _ ->
                {Name, Arity} = NameArity,
                erl_internal:bif(Name, Arity)
        end,
    case IsBif of
        true ->
            process_bif_calls(OnlyProjModules, NameArity);
        false ->
            case esrv_req_lib:get_local_function(Uri, ModuleData, NameArity) of
                {ok, {DefinitionUri, _}} ->
                    add_function_items(OnlyProjModules, DefinitionUri, NameArity, sets:new());
                undefined ->
                    collect_function_pois(Uri, ModuleData, [{local_function, NameArity}], [])
            end
    end.

-spec process_remote_function(OnlyProjModules :: boolean(),
                              ModuleName :: module(),
                              NameArity :: name_arity()) -> [item()].
process_remote_function(OnlyProjModules, ModuleName, NameArity) ->
    case esrv_req_lib:get_remote_function(ModuleName, NameArity) of
        {ok, {DefinitionUri, _}} ->
            ModulesSet = sets:from_list([ModuleName]),
            add_function_items(OnlyProjModules, DefinitionUri, NameArity, ModulesSet);
        undefined ->
            esrv_req_lib:traverse_modules(OnlyProjModules,
                                          fun({U, MD}, Acc) ->
                                                  ToCollect = [{remote_function,
                                                                ModuleName,
                                                                NameArity}],
                                                  collect_function_pois(U, MD, ToCollect, Acc)
                                          end,
                                          [])
    end.

-spec process_bif_calls(OnlyProjModules :: boolean(), NameArity :: name_arity()) -> [item()].
process_bif_calls(OnlyProjModules, NameArity) ->
    esrv_req_lib:traverse_modules(OnlyProjModules,
                                  fun({U, MD}, Acc) ->
                                          ToCollect = [{remote_function, erlang, NameArity}],
                                          collect_function_pois(U, MD, ToCollect, Acc)
                                  end,
                                  []).

-spec add_function_items(OnlyProjModules :: boolean(),
                         DefinitionUri :: uri(),
                         NameArity :: name_arity(),
                         ModulesSet :: sets:set(module())) -> [item()].
add_function_items(OnlyProjModules, DefinitionUri, NameArity, ModulesSet0) ->
    Processor =
        fun({U, MD}, {Items00, ModulesSet00, ExportedToSkip00, ModuleToSkip00}) ->
                Items01 = collect_function_pois(U, MD, [{local_function, NameArity}], Items00),
                {ExportedToSkip01, Exported} =
                    esrv_req_lib:get_exported(U, MD, NameArity, ExportedToSkip00),
                case Exported of
                    true ->
                        {ModuleToSkip01, ModuleNameData} =
                            esrv_req_lib:get_module_name(U, MD, ModuleToSkip00),
                        case ModuleNameData of
                            {ok, {_, {ModuleName, _}}} ->
                                {Items01,
                                 sets:add_element(ModuleName, ModulesSet00),
                                 ExportedToSkip01,
                                 ModuleToSkip01};
                            undefined ->
                                {Items01, ModulesSet00, ExportedToSkip01, ModuleToSkip01}
                        end;
                    false ->
                        {Items01, ModulesSet00, ExportedToSkip01, ModuleToSkip00}
                end
        end,
    {Items1, ModulesSet1, _, _} =
        esrv_req_lib:traverse_by_definition(OnlyProjModules,
                                            Processor,
                                            DefinitionUri,
                                            {[], ModulesSet0, [], []}),
    case sets:is_empty(ModulesSet1) of
        false ->
            ToCollect =
                sets:fold(fun(ModuleName, Acc) ->
                                  [{remote_function, ModuleName, NameArity} | Acc]
                          end, [], ModulesSet1),
            esrv_req_lib:traverse_modules(OnlyProjModules,
                                          fun({U, MD}, Items10) ->
                                                  collect_function_pois(U, MD, ToCollect, Items10)
                                          end,
                                          Items1);
        true ->
            Items1
    end.

-spec collect_function_pois(Uri :: uri(),
                            ModuleData :: module_data(),
                            ToCollect :: [poi_data()],
                            Acc :: [item()]) -> [item()].
collect_function_pois(Uri, ModuleData, ToCollect, Acc) ->
    #module_data{zones = Zones} = ModuleData,
    lists:filter(fun({_, StartLocation, _}) ->
                         case esrv_req_lib:get_zone(StartLocation, Zones) of
                             {ok, #zone{type = {function, _}}} ->
                                 true;
                             {ok, #zone{type = record_body}} ->
                                 true;
                             _ ->
                                 false
                         end
                 end, collect_pois(Uri, ModuleData, ToCollect, [])) ++ Acc.

-spec process_plain(OnlyProjModules :: boolean(), PoiData :: poi_data()) -> [item()].
process_plain(OnlyProjModules, PoiData) ->
    Processor = fun({U, MD}, Acc) -> collect_pois(U, MD, [PoiData], Acc) end,
    esrv_req_lib:traverse_modules(OnlyProjModules, Processor, []).

-spec process_definition(OnlyProjModules :: boolean(),
                         PoiData :: poi_data(),
                         DefinitionUri :: uri()) -> [item()].
process_definition(OnlyProjModules, PoiData, DefinitionUri) ->
    Processor = fun({U, MD}, A) -> collect_pois(U, MD, [PoiData], A) end,
    esrv_req_lib:traverse_by_definition(OnlyProjModules, Processor, DefinitionUri, []).

-spec collect_pois(Uri :: uri(),
                   ModuleData :: module_data(),
                   ToCollect :: [poi_data()],
                   Acc :: [item()]) -> [item()].
collect_pois(Uri, ModuleData, ToCollect, Acc0) ->
    lists:foldl(fun({_, #poi{start_location = Location, definition = {local, Location}}}, Acc00) ->
                        Acc00;
                   ({EndLocation, #poi{data = PoiData, start_location = StartLocation}}, Acc00) ->
                        case lists:member(PoiData, ToCollect) of
                            true ->
                                [{Uri, StartLocation, EndLocation} | Acc00];
                            false ->
                                Acc00
                        end
                end, Acc0, esrv_req_lib:collect_pois(Uri, ModuleData)).
