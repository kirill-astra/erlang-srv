-module(esrv_req_td_definition).

-behaviour(esrv_request_processor).

%% API
-export([process/1]).

-include("types.hrl").
-include("parser.hrl").
-include("records.hrl").

-spec process(Request :: request()) -> {response, response()}.
process(#request{id = Id, params = #{<<"position">> := #{<<"line">> := Line,
                                                         <<"character">> := Character},
                                     <<"textDocument">> := #{<<"uri">> := Uri}}}) ->
    Result =
        case process_target(Uri, {Line + 1, Character + 1}) of
            {ok, DefinitionUri, DefinitionLocation} ->
                esrv_req_lib:format_location(DefinitionUri, DefinitionLocation);
            undefined ->
                null
        end,
    {response, #response{id = Id, result = Result}}.

-spec process_target(TargetUri :: uri(), TargetLocation :: location()) ->
          {ok, uri(), location()} | undefined.
process_target(TargetUri, TargetLocation) ->
    {ok, ModuleData} = esrv_index_mgr:get_current_module_data(TargetUri),
    case esrv_req_lib:get_poi(TargetLocation, TargetUri, ModuleData) of
        {ok, #poi{start_location = Location, definition = {local, Location}}} ->
            undefined;
        {ok, #poi{definition = {local, DefinitionLocation}}} ->
            {ok, TargetUri, DefinitionLocation};
        {ok, #poi{definition = {remote, DefinitionUri, DefinitionLocation}}} ->
            {ok, DefinitionUri, DefinitionLocation};
        {ok, Poi} ->
            process_poi(TargetUri, ModuleData, Poi);
        undefined ->
            undefined
    end.

-spec process_poi(TargetUri :: uri(), ModuleData :: module_data(), Poi :: poi()) ->
          {ok, uri(), location()} | undefined.
process_poi(TargetUri, _, #poi{data = {include, Include}}) ->
    AppId = esrv_lib:get_app_id(TargetUri),
    case esrv_lib:find_included_uri(TargetUri, AppId, Include) of
        {ok, IncludedUri} ->
            {ok, IncludedUri, {1, 1}};
        undefined ->
            undefined
    end;
process_poi(_, _, #poi{data = {include_lib, IncludeLib}}) ->
    case esrv_lib:find_included_lib_uri(IncludeLib) of
        {ok, IncludedUri} ->
            {ok, IncludedUri, {1, 1}};
        undefined ->
            undefined
    end;
process_poi(_, _, #poi{data = {module, ModuleName}}) ->
    case esrv_req_lib:find_module_data(ModuleName) of
        {ok, ModuleUri, #module_data{module_name = {_, Location}}} ->
            {ok, ModuleUri, Location};
        _ ->
            undefined
    end;
process_poi(TargetUri, ModuleData, #poi{data = {behavior, ModuleName}} = Poi0) ->
    process_poi(TargetUri, ModuleData, Poi0#poi{data = {module, ModuleName}});
process_poi(TargetUri, ModuleData, #poi{data = {record, RecordName}}) ->
    case esrv_req_lib:get_record_data(TargetUri, ModuleData, RecordName) of
        {ok, {RecordUri, #record_data{location = RecordLocation}}} ->
            {ok, RecordUri, RecordLocation};
        undefined ->
            undefined
    end;
process_poi(TargetUri, ModuleData, #poi{data = {field, RecordName, FieldName}}) ->
    case esrv_req_lib:get_record_data(TargetUri, ModuleData, RecordName) of
        {ok, {RecordUri, #record_data{record_fields = RecordFields}}}
          when is_map_key(FieldName, RecordFields) ->
            FieldLocation = maps:get(FieldName, RecordFields),
            {ok, RecordUri, FieldLocation};
        _ ->
            undefined
    end;
process_poi(TargetUri, ModuleData, #poi{data = {local_type, NameArity}}) ->
    case esrv_req_lib:get_local_type(TargetUri, ModuleData, NameArity) of
        {ok, {TypeUri, TypeLocation}} ->
            {ok, TypeUri, TypeLocation};
        undefined ->
            undefined
    end;
process_poi(TargetUri, ModuleData, #poi{data = {local_type_name, Name}}) ->
    case esrv_req_lib:collect_local_type_name(TargetUri, ModuleData, Name) of
        [{_, {TypeUri, TypeLocation}} | _] ->
            {ok, TypeUri, TypeLocation};
        [] ->
            undefined
    end;
process_poi(_, _, #poi{data = {remote_type, ModuleName, NameArity}}) ->
    case esrv_req_lib:get_remote_type(ModuleName, NameArity) of
        {ok, {TypeUri, TypeLocation}} ->
            {ok, TypeUri, TypeLocation};
        undefined ->
            undefined
    end;
process_poi(_, _, #poi{data = {remote_type_name, ModuleName, Name}}) ->
    case esrv_req_lib:collect_remote_type_name(ModuleName, Name) of
        [{_, {TypeUri, TypeLocation}} | _] ->
            {ok, TypeUri, TypeLocation};
        [] ->
            undefined
    end;
process_poi(TargetUri, ModuleData, #poi{data = {local_spec, NameArity}} = Poi0) ->
    process_poi(TargetUri, ModuleData, Poi0#poi{data = {local_function, NameArity}});
process_poi(TargetUri, ModuleData, #poi{data = {function_clause, NameArity}} = Poi0) ->
    process_poi(TargetUri, ModuleData, Poi0#poi{data = {local_function, NameArity}});
process_poi(TargetUri, ModuleData, #poi{data = {local_function, {Name, Arity}}}) ->
    case esrv_req_lib:get_local_function(TargetUri, ModuleData, {Name, Arity}) of
        {ok, {FunctionUri, #function_data{location = FunctionLocation}}} ->
            {ok, FunctionUri, FunctionLocation};
        undefined ->
            undefined
    end;
process_poi(TargetUri, ModuleData, #poi{data = {local_function_name, Name}}) ->
    case esrv_req_lib:collect_local_function_name(TargetUri, ModuleData, Name) of
        [{_, {FunctionUri, #function_data{location = FunctionLocation}}} | _] ->
            {ok, FunctionUri, FunctionLocation};
        [] ->
            undefined
    end;
process_poi(TargetUri, ModuleData, #poi{data = {remote_spec, ModuleName, NameArity}} = Poi0) ->
    process_poi(TargetUri, ModuleData, Poi0#poi{data = {remote_function, ModuleName, NameArity}});
process_poi(_, _, #poi{data = {remote_function, ModuleName, NameArity}}) ->
    case esrv_req_lib:get_remote_function(ModuleName, NameArity) of
        {ok, {FunctionUri, #function_data{location = FunctionLocation}}} ->
            {ok, FunctionUri, FunctionLocation};
        undefined ->
            undefined
    end;
process_poi(_, _, #poi{data = {remote_function_name, ModuleName, Name}}) ->
    case esrv_req_lib:collect_remote_function_name(ModuleName, Name) of
        [{_, {FunctionUri, #function_data{location = FunctionLocation}}} | _] ->
            {ok, FunctionUri, FunctionLocation};
        [] ->
            undefined
    end;
process_poi(TargetUri, ModuleData, #poi{data = {local_type_or_function, NameArity}} = Poi0) ->
    lists:foldl(fun(PoiData, undefined) ->
                        Poi1 = Poi0#poi{data = PoiData},
                        process_poi(TargetUri, ModuleData, Poi1);
                   (_, Result) ->
                        Result
                end, undefined, [{local_type, NameArity},
                                 {local_function, NameArity}]);
process_poi(TargetUri, ModuleData, #poi{data = {local_type_or_function_name, Name}} = Poi0) ->
    lists:foldl(fun(PoiData, undefined) ->
                        Poi1 = Poi0#poi{data = PoiData},
                        process_poi(TargetUri, ModuleData, Poi1);
                   (_, Result) ->
                        Result
                end, undefined, [{local_type_name, Name},
                                 {local_function_name, Name}]);
process_poi(TargetUri, ModuleData,
            #poi{data = {remote_type_or_function, ModuleName, NameArity}} = Poi0) ->
    lists:foldl(fun(PoiData, undefined) ->
                        Poi1 = Poi0#poi{data = PoiData},
                        process_poi(TargetUri, ModuleData, Poi1);
                   (_, Result) ->
                        Result
                end, undefined, [{remote_type, ModuleName, NameArity},
                                 {remote_function, ModuleName, NameArity}]);
process_poi(TargetUri, ModuleData,
            #poi{data = {remote_type_or_function_name, ModuleName, Name}} = Poi0) ->
    lists:foldl(fun(PoiData, undefined) ->
                        Poi1 = Poi0#poi{data = PoiData},
                        process_poi(TargetUri, ModuleData, Poi1);
                   (_, Result) ->
                        Result
                end, undefined, [{remote_type_name, ModuleName, Name},
                                 {remote_function_name, ModuleName, Name}]);
process_poi(_, _, _) ->
    undefined.
