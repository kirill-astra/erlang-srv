-module(esrv_req_td_rename).

-behaviour(esrv_request_processor).

%% API
-export([process/1]).

-include("types.hrl").
-include("parser.hrl").
-include("records.hrl").
-include("log.hrl").

-type edit_item() :: {uri(), location(), location()}.
-type rename_item() :: {uri(), uri()}.

-type document_change_item() :: ok.

-define(TRAVERSE_PROJ_MODULES, esrv_req_lib:traverse_proj_modules).

-spec process(Request :: request()) -> {response, response()}.
process(#request{id = Id, params = #{<<"position">> := #{<<"line">> := Line,
                                                         <<"character">> := Character},
                                     <<"textDocument">> := #{<<"uri">> := Uri},
                                     <<"newName">> := NewName}}) ->
    {ok, ModuleData} = esrv_index_mgr:get_current_module_data(Uri),
    {EditItems, RenameItems, TargetName} =
        case esrv_req_lib:get_poi(false, {Line + 1, Character + 1}, Uri, ModuleData) of
            {ok, Poi} ->
                {process_edits(Poi, Uri, ModuleData),
                 process_renames(Poi, NewName),
                 process_target_name(Poi, NewName)};
            undefined ->
                {[], [], NewName}
        end,
    Result = format_result(TargetName, EditItems, RenameItems),
    {response, #response{id = Id, result = Result}}.

-spec process_edits(Poi :: poi(), Uri :: uri(), ModuleData :: module_data()) -> [edit_item()].
process_edits(#poi{data = {macro, _}} = Poi, Uri, ModuleData) ->
    process_macro(Uri, ModuleData, Poi);
process_edits(#poi{data = {Tag, _} = PoiData}, _, _)
  when Tag =:= include orelse Tag =:= include_lib ->
    process_plain([PoiData]);
process_edits(#poi{data = {Tag, Name}}, _, _)
  when Tag =:= module orelse Tag =:= behavior ->
    process_plain([{module, Name}, {behavior, Name}]);
process_edits(#poi{data = {callback, NameArity}, start_location = StartLocation},
              Uri, ModuleData) ->
    EndLocation = esrv_req_lib:find_end_location(StartLocation, ModuleData),
    [{Uri, StartLocation, EndLocation} | process_callback(NameArity, ModuleData)];
process_edits(#poi{data = {variable, _}} = Poi, Uri, ModuleData) ->
    process_variable(Uri, ModuleData, Poi);
process_edits(#poi{data = {record, RecordName}}, Uri, ModuleData) ->
    process_record(Uri, ModuleData, RecordName);
process_edits(#poi{data = {field, RecordName, FieldName}}, Uri, ModuleData) ->
    process_field(Uri, ModuleData, RecordName, FieldName);
process_edits(#poi{data = {local_type, NameArity}}, Uri, ModuleData) ->
    process_local_type(Uri, ModuleData, NameArity);
process_edits(#poi{data = {remote_type, ModuleName, NameArity}}, _, _) ->
    process_remote_type(ModuleName, NameArity);
process_edits(#poi{data = {Tag, NameArity}}, Uri, ModuleData)
  when Tag =:= local_spec orelse Tag =:= local_function orelse Tag =:= function_clause ->
    process_local_function(Uri, ModuleData, NameArity);
process_edits(#poi{data = {Tag, ModuleName, NameArity}}, _, _)
  when Tag =:= remote_spec orelse Tag =:= remote_function ->
    process_remote_function(ModuleName, NameArity);
process_edits(_, _, _) ->
    [].

-spec process_macro(Uri :: uri(), ModuleData :: module_data(), Poi :: poi()) -> [edit_item()].
process_macro(_, _, #poi{data = PoiData, definition = {remote, Uri, _}}) ->
    process_definition(PoiData, Uri);
process_macro(Uri, _, #poi{data = PoiData, definition = {local, _}}) ->
    process_definition(PoiData, Uri);
process_macro(Uri, ModuleData, #poi{data = PoiData, definition = undefined}) ->
    process_pois(Uri, ModuleData, [PoiData], []).

-spec process_callback(NameArity :: name_arity(), ModuleData :: module_data()) -> [edit_item()].
process_callback(NameArity, #module_data{module_name = {ModuleName, _}}) ->
    Processor = fun({Uri, #module_data{behaviors = Behaviors} = ModuleData}, Acc) ->
                        case maps:is_key(ModuleName, Behaviors) of
                            true ->
                                Targets = [{local_spec, NameArity},
                                           {local_function, NameArity},
                                           {function_clause, NameArity}],
                                process_pois(Uri, ModuleData, Targets, Acc);
                            false ->
                                Acc
                        end
                end,
    ?TRAVERSE_PROJ_MODULES(Processor, []);
process_callback(_, _) ->
    [].

-spec process_variable(Uri :: uri(), ModuleData :: module_data(), Poi :: poi()) -> [edit_item()].
process_variable(Uri, ModuleData, #poi{data = PoiData, definition = Definition}) ->
    lists:foldl(fun({EndLocation, Poi}, Acc) ->
                        case Poi of
                            #poi{data = PoiData,
                                 start_location = StartLocation,
                                 definition = Definition} ->
                                [{Uri, StartLocation, EndLocation} | Acc];
                            _ ->
                                Acc
                        end
                end, [], esrv_req_lib:collect_pois(false, Uri, ModuleData)).

-spec process_record(Uri :: uri(), ModuleData :: module_data(), RecordName :: name()) ->
          [edit_item()].
process_record(Uri, ModuleData, RecordName) ->
    case esrv_req_lib:get_record_data(Uri, ModuleData, RecordName) of
        {ok, {DefinitionUri, _}} ->
            process_definition({record, RecordName}, DefinitionUri);
        undefined ->
            []
    end.

-spec process_field(Uri :: uri(),
                    ModuleData :: module_data(),
                    RecordName :: name(),
                    FieldName :: name()) -> [edit_item()].
process_field(Uri, ModuleData, RecordName, FieldName) ->
    case esrv_req_lib:get_record_data(Uri, ModuleData, RecordName) of
        {ok, {DefinitionUri, _}} ->
            process_definition({field, RecordName, FieldName}, DefinitionUri);
        undefined ->
            []
    end.

-spec process_local_type(Uri :: uri(),
                         ModuleData :: module_data(),
                         NameArity :: name_arity()) -> [edit_item()].
process_local_type(Uri, ModuleData, NameArity) ->
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
            process_bit_calls(NameArity);
        false ->
            case esrv_req_lib:get_local_type(Uri, ModuleData, NameArity) of
                {ok, {DefinitionUri, _}} ->
                    add_type_items(DefinitionUri, NameArity, sets:new());
                undefined ->
                    process_pois(Uri, ModuleData, [{local_type, NameArity}], [])
            end
    end.

-spec process_remote_type(ModuleName :: module(), NameArity :: name_arity()) -> [edit_item()].
process_remote_type(ModuleName, NameArity) ->
    case esrv_req_lib:get_remote_type(ModuleName, NameArity) of
        {ok, {DefinitionUri, _}} ->
            add_type_items(DefinitionUri, NameArity, sets:from_list([ModuleName]));
        undefined ->
            ?TRAVERSE_PROJ_MODULES(fun({U, MD}, Acc) ->
                                           ToCollect = [{remote_type,
                                                         ModuleName,
                                                         NameArity}],
                                           process_pois(U, MD, ToCollect, Acc)
                                   end, [])
    end.

-spec process_bit_calls(NameArity :: name_arity()) -> [edit_item()].
process_bit_calls(NameArity) ->
    ?TRAVERSE_PROJ_MODULES(fun({U, MD}, Acc) ->
                                   ToCollect = [{local_type, NameArity}],
                                   process_pois(U, MD, ToCollect, Acc)
                           end, []).

-spec add_type_items(DefinitionUri :: uri(),
                     NameArity :: name_arity(),
                     ModulesSet :: sets:set(module())) -> [edit_item()].
add_type_items(DefinitionUri, NameArity, ModulesSet0) ->
    Processor =
        fun({U, MD}, {Items00, ModulesSet00, ExportedToSkip00, ModuleToSkip00}) ->
                Items01 = process_pois(U, MD, [{local_type, NameArity}], Items00),
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
        esrv_req_lib:traverse_by_definition(Processor, DefinitionUri, {[], ModulesSet0, [], []}),
    case sets:is_empty(ModulesSet1) of
        false ->
            ToCollect =
                sets:fold(fun(ModuleName, Acc) ->
                                  [{remote_type, ModuleName, NameArity} | Acc]
                          end, [], ModulesSet1),
            ?TRAVERSE_PROJ_MODULES(fun({U, MD}, Items10) ->
                                           process_pois(U, MD, ToCollect, Items10)
                                   end, Items1);
        true ->
            Items1
    end.

-spec process_local_function(Uri :: uri(),
                             ModuleData :: module_data(),
                             NameArity :: name_arity()) -> [edit_item()].
process_local_function(Uri, ModuleData, NameArity) ->
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
            process_bif_calls(NameArity);
        false ->
            case esrv_req_lib:get_local_function(Uri, ModuleData, NameArity) of
                {ok, {DefinitionUri, _}} ->
                    add_function_items(DefinitionUri, NameArity, sets:new());
                undefined ->
                    process_pois(Uri, ModuleData, [{local_function, NameArity}], [])
            end
    end.

-spec process_remote_function(ModuleName :: module(), NameArity :: name_arity()) -> [edit_item()].
process_remote_function(ModuleName, NameArity) ->
    case esrv_req_lib:get_remote_function(ModuleName, NameArity) of
        {ok, {DefinitionUri, _}} ->
            add_function_items(DefinitionUri, NameArity, sets:from_list([ModuleName]));
        undefined ->
            ?TRAVERSE_PROJ_MODULES(fun({U, MD}, Acc) ->
                                           ToCollect = [{remote_function,
                                                         ModuleName,
                                                         NameArity}],
                                           process_pois(U, MD, ToCollect, Acc)
                                   end, [])
    end.

-spec process_bif_calls(NameArity :: name_arity()) -> [edit_item()].
process_bif_calls(NameArity) ->
    ?TRAVERSE_PROJ_MODULES(fun({U, MD}, Acc) ->
                                   ToCollect = [{remote_function, erlang, NameArity}],
                                   process_pois(U, MD, ToCollect, Acc)
                           end, []).

-spec add_function_items(DefinitionUri :: uri(),
                         NameArity :: name_arity(),
                         ModulesSet :: sets:set(module())) -> [edit_item()].
add_function_items(DefinitionUri, NameArity, ModulesSet0) ->
    Processor =
        fun({U, MD}, {Items00, ModulesSet00, ExportedToSkip00, ModuleToSkip00}) ->
                Items01 = process_pois(U, MD, [{local_spec, NameArity},
                                               {local_function, NameArity},
                                               {function_clause, NameArity}], Items00),
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
        esrv_req_lib:traverse_by_definition(Processor, DefinitionUri, {[], ModulesSet0, [], []}),
    case sets:is_empty(ModulesSet1) of
        false ->
            ToCollect =
                sets:fold(fun(ModuleName, Acc) ->
                                  [{remote_spec, ModuleName, NameArity},
                                   {remote_function, ModuleName, NameArity} | Acc]
                          end, [], ModulesSet1),
            ?TRAVERSE_PROJ_MODULES(fun({U, MD}, Items10) ->
                                           process_pois(U, MD, ToCollect, Items10)
                                   end, Items1);
        true ->
            Items1
    end.

-spec process_plain(Targets :: [poi_data()]) -> [edit_item()].
process_plain(Targets) ->
    Processor = fun({U, MD}, A) -> process_pois(U, MD, Targets, A) end,
    ?TRAVERSE_PROJ_MODULES(Processor, []).

-spec process_definition(PoiData :: poi_data(), DefinitionUri :: uri()) -> [edit_item()].
process_definition(PoiData, DefinitionUri) ->
    Processor = fun({U, MD}, A) -> process_pois(U, MD, [PoiData], A) end,
    esrv_req_lib:traverse_by_definition(Processor, DefinitionUri, []).

-spec process_pois(Uri :: uri(),
                   ModuleData :: module_data(),
                   Targets :: [poi_data()],
                   Acc :: [edit_item()]) -> [edit_item()].
process_pois(Uri, ModuleData, Targets, Acc0) ->
    lists:foldl(fun({EndLocation, #poi{data = PoiData, start_location = StartLocation}}, Acc00) ->
                        case lists:member(PoiData, Targets) of
                            true ->
                                [{Uri, StartLocation, EndLocation} | Acc00];
                            false ->
                                Acc00
                        end
                end, Acc0, esrv_req_lib:collect_pois(false, Uri, ModuleData)).

-spec process_renames(Poi :: poi(), NewName :: binary()) -> [rename_item()].
process_renames(#poi{data = {Tag, Name}}, NewName)
  when Tag =:= module orelse Tag =:= behavior ->
    case esrv_db:read_module_meta_by_name(Name) of
        [#module_meta{app_type = AppType, uri = OldUri}]
          when AppType =:= proj orelse AppType =:= sub_proj ->
            RegExp = [<<"^(.*/)">>, atom_to_binary(Name, utf8), <<"(\\.erl)$">>],
            Replacement = [<<"\\1">>, strip_name(NewName), <<"\\2">>],
            case re:replace(OldUri, RegExp, Replacement, [{return, binary}]) of
                NewUri when NewUri =/= OldUri ->
                    [{OldUri, NewUri}];
                _ ->
                    []
            end;
        _ ->
            []
    end;
process_renames(_, _) ->
    [].

-spec process_target_name(Poi :: poi(), NewName :: binary()) -> binary().
process_target_name(#poi{data = {macro, {MacroName, _}}}, NewName) ->
    format_new_name(MacroName, NewName, fun esrv_req_lib:format_macro_name/1);
process_target_name(#poi{data = {module, ModuleName}}, NewName) ->
    format_new_name(ModuleName, NewName, fun esrv_req_lib:format_atom/1);
process_target_name(#poi{data = {behavior, BehaviorName}}, NewName) ->
    format_new_name(BehaviorName, NewName, fun esrv_req_lib:format_atom/1);
process_target_name(#poi{data = {callback, {CallbackName, _}}}, NewName) ->
    format_new_name(CallbackName, NewName, fun esrv_req_lib:format_atom/1);
process_target_name(#poi{data = {record, RecordName}}, NewName) ->
    format_new_name(RecordName, NewName, fun esrv_req_lib:format_atom/1);
process_target_name(#poi{data = {field, _, FieldName}}, NewName) ->
    format_new_name(FieldName, NewName, fun esrv_req_lib:format_atom/1);
process_target_name(#poi{data = {local_type, {TypeName, _}}}, NewName) ->
    format_new_name(TypeName, NewName, fun esrv_req_lib:format_atom/1);
process_target_name(#poi{data = {local_type_name, TypeName}}, NewName) ->
    format_new_name(TypeName, NewName, fun esrv_req_lib:format_atom/1);
process_target_name(#poi{data = {remote_type, _, {TypeName, _}}}, NewName) ->
    format_new_name(TypeName, NewName, fun esrv_req_lib:format_atom/1);
process_target_name(#poi{data = {remote_type_name, _, TypeName}}, NewName) ->
    format_new_name(TypeName, NewName, fun esrv_req_lib:format_atom/1);
process_target_name(#poi{data = {Tag, {FunctionName, _}}}, NewName)
  when Tag =:= local_spec orelse Tag =:= function_clause orelse Tag =:= local_function ->
    format_new_name(FunctionName, NewName, fun esrv_req_lib:format_atom/1);
process_target_name(#poi{data = {local_function_name, FunctionName}}, NewName) ->
    format_new_name(FunctionName, NewName, fun esrv_req_lib:format_atom/1);
process_target_name(#poi{data = {Tag, _, {FunctionName, _}}}, NewName)
  when Tag =:= remote_spec orelse Tag =:= remote_function ->
    format_new_name(FunctionName, NewName, fun esrv_req_lib:format_atom/1);
process_target_name(#poi{data = {remote_function_name, _, FunctionName}}, NewName) ->
    format_new_name(FunctionName, NewName, fun esrv_req_lib:format_atom/1);
process_target_name(_, NewName) ->
    NewName.

-spec format_new_name(OldName :: name(),
                      NewName :: binary(),
                      Formatter :: fun((name()) -> binary())) -> binary().
format_new_name(OldName, NewName, Formatter) ->
    FormattedOldName = Formatter(OldName),
    StrippedNewName = strip_name(NewName),
    FormattedNewName = Formatter(binary_to_atom(StrippedNewName, utf8)),
    if
        FormattedOldName =:= FormattedNewName ->
            case {is_name_quoted(FormattedOldName), is_name_quoted(NewName)} of
                {false, true} ->
                    <<$', FormattedNewName/binary, $'>>;
                _ ->
                    FormattedNewName
            end;
        true ->
            FormattedNewName
    end.

-spec strip_name(Name :: binary()) -> binary().
strip_name(Name) when byte_size(Name) > 0 ->
    case is_name_quoted(Name) of
        true ->
            StrippedName = binary:part(Name, 1, byte_size(Name) - 2),
            strip_name(StrippedName);
        false ->
            Name
    end;
strip_name(Name) ->
    Name.

-spec is_name_quoted(Name :: binary()) -> boolean().
is_name_quoted(Name) ->
    binary:first(Name) =:= $' andalso binary:last(Name) =:= $'.

-spec format_result(NewName :: binary(),
                    EditItems :: [edit_item()],
                    RenameItems :: [document_change_item()]) -> jsx:json_term().
format_result(_, [], []) ->
    null;
format_result(NewName, EditItems, RenameItems) ->
    lists:foldl(fun({Key, Values}, Acc) ->
                        maps:update_with(Key,
                                         fun(ValuesAcc) ->
                                                 Values ++ ValuesAcc
                                         end, Values, Acc);
                   (undefined, Acc) ->
                        Acc
                end, #{}, [format_rename_items(RenameItems),
                           format_edit_items(NewName, EditItems)]).

-spec format_edit_items(NewName :: binary(), EditItems :: [edit_item()]) ->
          {binary(), jsx:json_term()} | undefined.
format_edit_items(_, []) ->
    undefined;
format_edit_items(NewName, EditItems) ->
    Grouped =
        lists:foldl(fun({Uri, LocationStart, LocationEnd}, Acc) ->
                            maps:update_with(Uri,
                                             fun(RangesAcc) ->
                                                     [{LocationStart, LocationEnd} | RangesAcc]
                                             end, [{LocationStart, LocationEnd}], Acc)
                    end, #{}, EditItems),
    Path = [<<"workspace">>, <<"workspaceEdit">>, <<"documentChanges">>],
    case esrv_lib:get_client_capability(Path) of
        true ->
            {<<"documentChanges">>,
             maps:fold(fun(Uri, Ranges, Acc) ->
                               [#{<<"textDocument">> => #{<<"uri">> => Uri, <<"version">> => null},
                                  <<"edits">> => format_edits(NewName, Ranges)} | Acc]
                       end, [], Grouped)};
        false ->
            {<<"changes">>,
             maps:map(fun(_, Ranges) -> format_edits(NewName, Ranges) end, Grouped)}
    end.

-spec format_edits(NewName :: binary(), Ranges :: [location_range()]) -> jsx:json_term().
format_edits(NewName, Ranges) ->
    lists:map(fun(Range) ->
                      #{<<"range">> => esrv_lib:format_range(Range),
                        <<"newText">> => NewName}
              end, Ranges).

-spec format_rename_items(RenameItems :: [document_change_item()]) ->
          {binary(), jsx:json_term()} | undefined.
format_rename_items([]) ->
    undefined;
format_rename_items(RenameItems) ->
    Path = [<<"workspace">>, <<"workspaceEdit">>, <<"resourceOperations">>],
    case esrv_lib:get_client_capability(Path) of
        Operations when is_list(Operations) ->
            case lists:member(<<"rename">>, Operations) of
                true ->
                    {<<"documentChanges">>,
                     lists:map(fun({OldUri, NewUri}) ->
                                       #{<<"kind">> => <<"rename">>,
                                         <<"oldUri">> => OldUri,
                                         <<"newUri">> => NewUri,
                                         <<"options">> => #{<<"overwrite">> => false}}
                               end, RenameItems)};
                false ->
                    undefined
            end;
        _ ->
            undefined
    end.
