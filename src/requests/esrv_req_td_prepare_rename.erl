-module(esrv_req_td_prepare_rename).

-behaviour(esrv_request_processor).

%% API
-export([process/1]).

-include("types.hrl").
-include("parser.hrl").
-include("records.hrl").
-include("log.hrl").

-type result() :: {location(), location(), binary()} | undefined.

-spec process(Request :: request()) -> {response, #response{}}.
process(#request{id = Id, params = #{<<"position">> := #{<<"line">> := Line,
                                                         <<"character">> := Character},
                                     <<"textDocument">> := #{<<"uri">> := Uri}}}) ->
    {ok, ModuleData} = esrv_index_mgr:get_current_module_data(Uri),
    Result =
        case esrv_req_lib:get_poi(false, {Line + 1, Character + 1}, Uri, ModuleData) of
            {ok, #poi{data = Data, start_location = StartLocation}} ->
                case process_poi_data(Data) of
                    Placeholder when is_binary(Placeholder) ->
                        EndLocation = esrv_req_lib:find_end_location(StartLocation, ModuleData),
                        {StartLocation, EndLocation, Placeholder};
                    undefined ->
                        undefined
                end;
            undefined ->
                undefined
        end,
    {response, #response{id = Id, result = format_result(Result)}}.

-spec process_poi_data(PoiData :: poi_data()) -> binary() | undefined.
process_poi_data({macro, {MacroName, _}}) ->
    atom_to_binary(MacroName, utf8);
process_poi_data({Tag, Include}) when Tag =:= include orelse Tag =:= include_lib ->
    iolist_to_binary(["\"", Include, "\""]);
process_poi_data({module, ModuleName}) ->
    atom_to_binary(ModuleName, utf8);
process_poi_data({behavior, BehaviorName}) ->
    atom_to_binary(BehaviorName, utf8);
process_poi_data({callback, {CallbackName, _}}) ->
    atom_to_binary(CallbackName, utf8);
process_poi_data({variable, Variable}) ->
    atom_to_binary(Variable, utf8);
process_poi_data({record, RecordName}) ->
    atom_to_binary(RecordName, utf8);
process_poi_data({field, _, FieldName}) ->
    atom_to_binary(FieldName, utf8);
process_poi_data({local_type, {TypeName, _}}) ->
    atom_to_binary(TypeName, utf8);
process_poi_data({local_type_name, TypeName}) ->
    atom_to_binary(TypeName, utf8);
process_poi_data({remote_type, _, {TypeName, _}}) ->
    atom_to_binary(TypeName, utf8);
process_poi_data({remote_type_name, _, TypeName}) ->
    atom_to_binary(TypeName, utf8);
process_poi_data({Tag, {FunctionName, _}})
  when Tag =:= local_spec orelse Tag =:= function_clause orelse Tag =:= local_function ->
    atom_to_binary(FunctionName, utf8);
process_poi_data({local_function_name, FunctionName}) ->
    atom_to_binary(FunctionName, utf8);
process_poi_data({Tag, _, {FunctionName, _}})
  when Tag =:= remote_spec orelse Tag =:= remote_function ->
    atom_to_binary(FunctionName, utf8);
process_poi_data({remote_function_name, _, FunctionName}) ->
    atom_to_binary(FunctionName, utf8);
process_poi_data(_) ->
    undefined.

-spec format_result(Result :: result()) -> jsx:json_term().
format_result({StartLocation, EndLocation, Placeholder}) ->
    #{<<"range">> => esrv_lib:format_range(StartLocation, EndLocation),
      <<"placeholder">> => Placeholder};
format_result(undefined) ->
    null.
