-module(esrv_req_td_hover).

-behaviour(esrv_request_processor).

%% API
-export([process/1]).

-include("types.hrl").
-include("records.hrl").

-spec process(Request :: request()) -> {response, response()}.
process(#request{id = Id, params = #{<<"position">> := #{<<"line">> := Line,
                                                         <<"character">> := Character},
                                     <<"textDocument">> := #{<<"uri">> := Uri}}}) ->
    MarkupKind = markup_kind(),
    Result =
        case process_target(MarkupKind, Uri, {Line + 1, Character + 1}) of
            {ok, Description} ->
                #{contents => #{kind => MarkupKind, value => Description}};
            undefined ->
                null
        end,
    {response, #response{id = Id, result = Result}}.

-spec markup_kind() -> markup_kind().
markup_kind() ->
    Path = [<<"textDocument">>, <<"hover">>, <<"contentFormat">>],
    case esrv_lib:get_client_capability(Path) of
        Formats when is_list(Formats) ->
            markup_kind(Formats);
        _ ->
            plaintext
    end.

-spec markup_kind(Formats :: [binary()]) -> markup_kind().
markup_kind([]) ->
    plaintext;
markup_kind([Format | _]) when Format =:= <<"plaintext">> orelse Format =:= <<"markdown">> ->
    binary_to_atom(Format, utf8);
markup_kind([_ | T]) ->
    markup_kind(T).

-spec process_target(MarkupKind :: markup_kind(),
                     TargetUri :: uri(),
                     TargetLocation :: location()) -> {ok, binary()} | undefined.
process_target(MarkupKind, TargetUri, TargetLocation) ->
    {ok, ModuleData} = esrv_index_mgr:get_current_module_data(TargetUri),
    case esrv_req_lib:get_poi(TargetLocation, TargetUri, ModuleData) of
        {ok, Poi} ->
            process_poi(MarkupKind, TargetUri, ModuleData, Poi);
        undefined ->
            undefined
    end.

-spec process_poi(MarkupKind :: markup_kind(),
                  TargetUri :: uri(),
                  ModuleData :: module_data(),
                  Poi :: poi()) -> {ok, binary()} | undefined.
process_poi(MarkupKind, _, _, #poi{data = {module, ModuleName}}) ->
    {ok, esrv_documentation:module_doc(MarkupKind, ModuleName)};
process_poi(MarkupKind, TargetUri, ModuleData, #poi{data = {local_spec, NameArity}} = Poi0) ->
    process_poi(MarkupKind, TargetUri, ModuleData, Poi0#poi{data = {local_function, NameArity}});
process_poi(MarkupKind, TargetUri, ModuleData, #poi{data = {function_clause, NameArity}} = Poi0) ->
    process_poi(MarkupKind, TargetUri, ModuleData, Poi0#poi{data = {local_function, NameArity}});
process_poi(MarkupKind, TargetUri, ModuleData, #poi{data = {local_function, NameArity}}) ->
    {ok, esrv_documentation:function_doc(MarkupKind, TargetUri, ModuleData, NameArity)};
process_poi(MarkupKind, TargetUri, ModuleData, #poi{data = {local_function_name, Name}}) ->
    {ok, esrv_documentation:function_name_doc(MarkupKind, TargetUri, ModuleData, Name)};
process_poi(MarkupKind, TargetUri, ModuleData,
            #poi{data = {remote_spec, ModuleName, NameArity}} = Poi0) ->
    process_poi(MarkupKind, TargetUri, ModuleData,
                Poi0#poi{data = {remote_function, ModuleName, NameArity}});
process_poi(MarkupKind, _, _, #poi{data = {remote_function, ModuleName, NameArity}}) ->
    {ok, esrv_documentation:function_doc(MarkupKind, ModuleName, NameArity)};
process_poi(MarkupKind, _, _, #poi{data = {remote_function_name, ModuleName, Name}}) ->
    {ok, esrv_documentation:function_name_doc(MarkupKind, ModuleName, Name)};
process_poi(MarkupKind, TargetUri, ModuleData, #poi{data = {local_type, NameArity}}) ->
    {ok, esrv_documentation:type_doc(MarkupKind, TargetUri, ModuleData, NameArity)};
process_poi(MarkupKind, TargetUri, ModuleData, #poi{data = {local_type_name, Name}}) ->
    {ok, esrv_documentation:type_name_doc(MarkupKind, TargetUri, ModuleData, Name)};
process_poi(MarkupKind, _, _, #poi{data = {remote_type, ModuleName, NameArity}}) ->
    {ok, esrv_documentation:type_doc(MarkupKind, ModuleName, NameArity)};
process_poi(MarkupKind, _, _, #poi{data = {remote_type_name, ModuleName, Name}}) ->
    {ok, esrv_documentation:type_name_doc(MarkupKind, ModuleName, Name)};
process_poi(MarkupKind, TargetUri, ModuleData, #poi{data = {local_type_or_function, NameArity}}) ->
    {ok, esrv_documentation:type_or_function_doc(MarkupKind, TargetUri, ModuleData, NameArity)};
process_poi(MarkupKind, TargetUri, ModuleData, #poi{data = {local_type_or_function_name, Name}}) ->
    {ok, esrv_documentation:type_or_function_name_doc(MarkupKind, TargetUri, ModuleData, Name)};
process_poi(MarkupKind, _, _, #poi{data = {remote_type_or_function, ModuleName, NameArity}}) ->
    {ok, esrv_documentation:type_or_function_doc(MarkupKind, ModuleName, NameArity)};
process_poi(MarkupKind, _, _, #poi{data = {remote_type_or_function_name, ModuleName, Name}}) ->
    {ok, esrv_documentation:type_or_function_name_doc(MarkupKind, ModuleName, Name)};
process_poi(MarkupKind, TargetUri, _, #poi{data = {record, RecordName}}) ->
    {ok, esrv_documentation:record_doc(MarkupKind, TargetUri, RecordName)};
process_poi(MarkupKind, TargetUri, _, #poi{data = {macro, {MacroName, MacroArity}}}) ->
    {ok, esrv_documentation:macros_doc(MarkupKind, TargetUri, MacroName, MacroArity)};
process_poi(_, _, _, _) ->
    undefined.
