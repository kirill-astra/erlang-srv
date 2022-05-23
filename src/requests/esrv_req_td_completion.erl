-module(esrv_req_td_completion).

-behaviour(esrv_request_processor).

%% API
-export([process/1]).

-include("types.hrl").
-include("parser.hrl").
-include("protocol.hrl").
-include("records.hrl").

-type item() :: jsx:json_term().
-type line_info() :: #{atom() => any()}.

-define(SORT_NORMAL, <<"5">>).
-define(SORT_MODULE, ?SORT_NORMAL).
-define(SORT_BEHAVIOR, ?SORT_NORMAL).
-define(SORT_OPTIONAL_CALLBACK, ?SORT_NORMAL).
-define(SORT_MACRO, ?SORT_NORMAL).
-define(SORT_RECORD, ?SORT_NORMAL).
-define(SORT_RECORD_FIELD, ?SORT_NORMAL).
-define(SORT_ATTRIBUTE, ?SORT_NORMAL).
-define(SORT_FUNCTION, ?SORT_NORMAL).
-define(SORT_BIF, ?SORT_NORMAL).
-define(SORT_TYPE, ?SORT_NORMAL).
-define(SORT_BIT, ?SORT_NORMAL).
-define(SORT_VARIABLE, ?SORT_NORMAL).
-define(SORT_SNIPPET, ?SORT_NORMAL).
-define(SORT_KEYWORD, ?SORT_NORMAL).
-define(SORT_ATOM, ?SORT_NORMAL).

-spec process(Request :: request()) -> {response, response()}.
process(#request{id = Id, params = #{<<"position">> := #{<<"line">> := Line,
                                                         <<"character">> := Character},
                                     <<"textDocument">> := #{<<"uri">> := Uri}}}) ->
    CursorLocation = {Line + 1, Character + 1},
    {ok, ModuleData} = esrv_index_mgr:get_current_module_data(Uri),
    {CompletionItems0, LeftLineInfo, RightLineInfo} = do_process(CursorLocation, Uri, ModuleData),
    CompletionItems1 = post_process_items(CursorLocation, LeftLineInfo, RightLineInfo,
                                          Uri, ModuleData, CompletionItems0),
    {response, #response{id = Id, result = CompletionItems1}}.

-spec do_process(CursorLocation :: location(), Uri :: uri(), ModuleData :: module_data()) ->
          {[item()], line_info(), line_info()}.
do_process({_, CursorCharacter} = CursorLocation, Uri, ModuleData) ->
    {ZoneType, StartLocation} = get_zone_info(CursorLocation, ModuleData),
    LinesData = get_lines_data(Uri, ZoneType, StartLocation, CursorLocation),
    [CursorLineData | T] = lists:reverse(LinesData),
    {ReversedTokens, LeftLineInfo, RightLineInfo} =
        parse_cursor_line(CursorLineData, CursorCharacter, ModuleData),
    case check_record_field(ReversedTokens, T, LeftLineInfo, StartLocation) of
        {ok, RecordName, Filter} ->
            {record_fields_completion_items(RecordName, Filter, ModuleData),
             LeftLineInfo,
             RightLineInfo};
        undefined ->
            PreceedingTokens = preceeding_tokens(ReversedTokens),
            CompletionItems =
                completion_items(PreceedingTokens, LeftLineInfo, ZoneType, Uri, ModuleData),
            {CompletionItems, LeftLineInfo, RightLineInfo}
    end.

-spec preceeding_tokens(ReversedTokens :: [erl_scan:token()] | ignore) ->
          [erl_scan:token()] | ignore.
preceeding_tokens(ignore) ->
    ignore;
preceeding_tokens(ReversedTokens) ->
    case ReversedTokens of
        %% behavior
        [{atom, _, _} = T4, {'(', _} = T3, {atom, _, Atom} = T2, {'-', _} = T1 | _]
          when Atom =:= behavior orelse Atom =:= behaviour ->
            [T1, T2, T3, T4];
        [{'(', _} = T3, {atom, _, Atom} = T2, {'-', _} = T1 | _]
          when Atom =:= behavior orelse Atom =:= behaviour ->
            [T1, T2, T3];
        %% import
        [{'[', _} = T6, {',', _} = T5, {atom, _, _} = T4,
         {'(', _} = T3, {atom, _, import} = T2, {'-', _} = T1 | _] ->
            [T1, T2, T3, T4, T5, T6];
        [{atom, _, _} = T4, {'(', _} = T3, {atom, _, import} = T2, {'-', _} = T1 | _] ->
            [T1, T2, T3, T4];
        [{'(', _} = T3, {atom, _, import} = T2, {'-', _} = T1 | _] ->
            [T1, T2, T3];
        %% on load
        [{atom, _, _} = T4, {'(', _} = T3, {atom, _, on_load} = T2, {'-', _} = T1 | _] ->
            [T1, T2, T3, T4];
        [{'(', _} = T3, {atom, _, on_load} = T2, {'-', _} = T1 | _] ->
            [T1, T2, T3];
        %% fun
        [{atom, _, _} = T4, {':', _} = T3, {atom, _, _} = T2, {'fun', _} = T1 | _] ->
            [T1, T2, T3, T4];
        [{var, _, _} = T4, {':', _} = T3, {atom, _, _} = T2, {'fun', _} = T1 | _] ->
            [T1, T2, T3, var_to_atom(T4)];
        [{':', _} = T3, {atom, _, _} = T2, {'fun', _} = T1 | _] ->
            [T1, T2, T3];
        [{atom, _, _} = T2, {'fun', _} = T1 | _] ->
            [T1, T2];
        [{var, _, _} = T2, {'fun', _} = T1 | _] ->
            [T1, var_to_atom(T2)];
        [{'fun', _} = T1 | _] ->
            [T1];
        %% function/type
        [{atom, _, _} = T3, {':', _} = T2, {atom, _, _} = T1 | _] ->
            [T1, T2, T3];
        [{var, _, _} = T3, {':', _} = T2, {atom, _, _} = T1 | _] ->
            [T1, T2, var_to_atom(T3)];
        [{':', _} = T2, {atom, _, _} = T1 | _] ->
            [T1, T2];
        %% record
        [{atom, _, _} = T4, {'{', _} = T3, {atom, _, _} = T2, {'#', _} = T1 | _] ->
            [T1, T2, T3, T4];
        [{var, _, _} = T4, {'{', _} = T3, {atom, _, _} = T2, {'#', _} = T1 | _] ->
            [T1, T2, T3, var_to_atom(T4)];
        [{'{', _} = T3, {atom, _, _} = T2, {'#', _} = T1 | _] ->
            [T1, T2, T3];
        [{atom, _, _} = T4, {'.', _} = T3, {atom, _, _} = T2, {'#', _} = T1 | _] ->
            [T1, T2, T3, T4];
        [{var, _, _} = T4, {'.', _} = T3, {atom, _, _} = T2, {'#', _} = T1 | _] ->
            [T1, T2, T3, var_to_atom(T4)];
        [{dot, _} = T3, {atom, _, _} = T2, {'#', _} = T1 | _] ->
            [T1, T2, T3];
        [{atom, _, _} = T2, {'#', _} = T1 | _] ->
            [T1, T2];
        [{var, _, _} = T2, {'#', _} = T1 | _] ->
            [T1, var_to_atom(T2)];
        [{'#', _} = T1 | _] ->
            [T1];
        %% macro
        [{Type, _, _} = T2, {'?', _} = T1 | _]
          when Type =:= atom orelse Type =:= var ->
            [T1, T2];
        [{'?', _} = T1 | _] ->
            [T1];
        %% attribute
        [{atom, _, _} = T2, {'-', _} = T1 | _] ->
            [T1, T2];
        [{'-', _} = T1 | _] ->
            [T1];
        %% variable
        [{var, _, _} = T1 | _] ->
            [T1];
        %% general
        [{atom, _, _} = T1 | _] ->
            [T1];
        [{Atom, _} | _] when Atom =:= dot orelse
                             Atom =:= ':' orelse
                             Atom =:= '(' orelse
                             Atom =:= '{' orelse
                             Atom =:= '/' orelse
                             Atom =:= 'after' orelse
                             Atom =:= 'and' orelse
                             Atom =:= 'andalso' orelse
                             Atom =:= 'band' orelse
                             Atom =:= 'begin' orelse
                             Atom =:= 'bnot' orelse
                             Atom =:= 'bor' orelse
                             Atom =:= 'bsl' orelse
                             Atom =:= 'bsr' orelse
                             Atom =:= 'bxor' orelse
                             Atom =:= 'case' orelse
                             Atom =:= 'catch' orelse
                             Atom =:= 'cond' orelse
                             Atom =:= 'div' orelse
                             Atom =:= 'end' orelse
                             Atom =:= 'fun' orelse
                             Atom =:= 'if' orelse
                             Atom =:= 'let' orelse
                             Atom =:= 'not' orelse
                             Atom =:= 'of' orelse
                             Atom =:= 'or' orelse
                             Atom =:= 'orelse' orelse
                             Atom =:= 'receive' orelse
                             Atom =:= 'rem' orelse
                             Atom =:= 'try' orelse
                             Atom =:= 'when' orelse
                             Atom =:= 'xor' ->
            ignore;
        [{integer, _, _} | _] ->
            ignore;
        _ ->
            []
    end.

-spec completion_items(Tokens :: [erl_scan:token()] | ignore,
                       LeftLineInfo :: line_info(),
                       ZoneType :: zone_type(),
                       Uri :: uri(),
                       ModuleData :: module_data()) -> [item()].
%% fun
completion_items([{'fun', _}, {atom, _, ModuleName}, {':', _}, {atom, _, Atom}],
                 #{whitespace := false}, _, _, _) ->
    Filter = atom_to_binary(Atom, utf8),
    remote_function_completion_items(ModuleName, Filter, arity);
completion_items([{'fun', _}, {atom, _, ModuleName}, {':', _}],
                 #{whitespace := false}, _, _, _) ->
    remote_function_completion_items(ModuleName, undefined, arity);
completion_items([{'fun', _}, {atom, _, ModuleName}],
                 #{whitespace := false}, _, Uri, ModuleData) ->
    Filter = atom_to_binary(ModuleName, utf8),
    module_completion_items(Filter)
        ++ local_function_completion_items(ModuleData, Filter, Uri, arity)
        ++ import_completion_items(ModuleData, Filter, Uri, arity)
        ++ bif_completion_items(Filter, arity);
completion_items([{'fun', _}], #{whitespace := true}, _, Uri, ModuleData) ->
    module_completion_items(undefined)
        ++ local_function_completion_items(ModuleData, undefined, Uri, arity)
        ++ import_completion_items(ModuleData, undefined, Uri, arity)
        ++ bif_completion_items(undefined, arity);

%% function/type
completion_items([{atom, _, ModuleName}, {':', _} | T], #{whitespace := false}, ZoneType, _, _) ->
    Filter =
        case T of
            [{atom, _, Atom}] ->
                atom_to_binary(Atom, utf8);
            [] ->
                undefined
        end,
    if
        is_tuple(ZoneType) andalso element(1, ZoneType) =:= function orelse
        ZoneType =:= outer ->
            remote_function_completion_items(ModuleName, Filter, args);
        ZoneType =:= type orelse
        ZoneType =:= spec_body ->
            remote_type_completion_items(ModuleName, Filter, args);
        ZoneType =:= record_body ->
            remote_function_completion_items(ModuleName, Filter, args)
                ++ remote_type_completion_items(ModuleName, Filter, args);
        true ->
            []
    end;

%% record
completion_items([{'#', _}, {atom, _, RecordName}, {'{', _}, {atom, _, Field}],
                 #{whitespace := false}, _, _, ModuleData) ->
    record_fields_completion_items(RecordName, atom_to_binary(Field, utf8), ModuleData);
completion_items([{'#', _}, {atom, _, RecordName}, {'{', _}],
                 #{whitespace := false}, _, _, ModuleData) ->
    record_fields_completion_items(RecordName, undefined, ModuleData);
completion_items([{'#', _}, {atom, _, RecordName}, {'.', _}, {atom, _, Field}],
                 #{whitespace := false}, _, _, ModuleData) ->
    record_fields_completion_items(RecordName, atom_to_binary(Field, utf8), ModuleData);
completion_items([{'#', _}, {atom, _, RecordName}, {dot, _}],
                 #{whitespace := false}, _, _, ModuleData) ->
    record_fields_completion_items(RecordName, undefined, ModuleData);
completion_items([{'#', _}, {atom, _, RecordName}], #{whitespace := false}, _, Uri, ModuleData) ->
    record_completion_items(atom_to_binary(RecordName, utf8), Uri, ModuleData);
completion_items([{'#', _}], #{whitespace := false}, _, Uri, ModuleData) ->
    record_completion_items(undefined, Uri, ModuleData);

%% macro
completion_items([{'?', _}, {Type, _, MacroName}], #{whitespace := false}, _, Uri, ModuleData)
  when Type =:= atom orelse Type =:= var ->
    Filter = atom_to_binary(MacroName, utf8),
    macros_completion_items(Filter, Uri, ModuleData)
        ++ predefined_macros_completion_items(Filter, Uri);
completion_items([{'?', _}], #{whitespace := false}, _, Uri, ModuleData) ->
    macros_completion_items(undefined, Uri, ModuleData)
        ++ predefined_macros_completion_items(undefined, Uri);

%% behavior
completion_items([{'-', _}, {atom, _, Atom}, {'(', _}, {atom, _, ModuleName}],
                 #{whitespace := false}, _, _, _)
  when Atom =:= behavior orelse Atom =:= behaviour ->
    Filter = atom_to_binary(ModuleName, utf8),
    behavior_completion_items(Filter);
completion_items([{'-', _}, {atom, _, Atom}, {'(', _}], _, _, _, _)
  when Atom =:= behavior orelse Atom =:= behaviour ->
    behavior_completion_items(undefined);

%% import
completion_items([{'-', _}, {atom, _, import}, {'(', _},
                  {atom, _, ModuleName}, {',', _}, {'[', _}], _, _, _, _) ->
    remote_function_completion_items(ModuleName, undefined, arity);
completion_items([{'-', _}, {atom, _, import}, {'(', _}, {atom, _, ModuleName}],
                 #{whitespace := false}, _, _, _) ->
    Filter = atom_to_binary(ModuleName, utf8),
    module_completion_items(Filter);
completion_items([{'-', _}, {atom, _, import}, {'(', _}], _, _, _, _) ->
    module_completion_items(undefined);

%% on load
completion_items([{'-', _}, {atom, _, on_load}, {'(', _}, {atom, _, FunctionName}],
                 #{whitespace := false}, _, _, ModuleData) ->
    Filter = atom_to_binary(FunctionName, utf8),
    module_function_completion_items(ModuleData, Filter, arity);
completion_items([{'-', _}, {atom, _, on_load}, {'(', _}], _, _, _, ModuleData) ->
    module_function_completion_items(ModuleData, undefined, arity);

%% attribute
completion_items([{'-', Anno} | T], #{whitespace := false}, _, _, _) ->
    case esrv_parser_lib:get_anno_location(Anno) of
        {_, 1} ->
            Filter =
                case T of
                    [{atom, _, Atom}] ->
                        atom_to_binary(Atom, utf8);
                    [] ->
                        undefined
                end,
            attributes_completion_items(Filter);
        _ ->
            []
    end;

%% variable
completion_items([{var, _, Var}], #{whitespace := false}, {function, NameArity}, _,
                 #module_data{functions = Functions}) when is_map_key(NameArity, Functions) ->
    Filter = atom_to_binary(Var, utf8),
    #function_data{clauses = Clauses} = maps:get(NameArity, Functions),
    Items =
        lists:foldl(fun(#clause{variables = Variables0}, Acc) ->
                            Variables1 = lists:delete(Var, Variables0),
                            Variables2 = filter_names(Filter, Variables1),
                            [ #{<<"label">> => atom_to_binary(V, utf8),
                                <<"kind">> => ?COMPLETION_ITEM_KIND_VARIABLE,
                                <<"sortText">> => ?SORT_VARIABLE} || V <- Variables2 ] ++ Acc
                    end, module_completion_items(Filter), Clauses),
    lists:usort(Items);

%% general
completion_items([{atom, _, Atom}], #{whitespace := false}, ZoneType, Uri, ModuleData) ->
    Filter = atom_to_binary(Atom, utf8),
    general_completion_items(Filter, ZoneType, Uri, ModuleData);
completion_items([{var, _, Var}], #{whitespace := false}, ZoneType, Uri, ModuleData) ->
    Filter = atom_to_binary(Var, utf8),
    general_completion_items(Filter, ZoneType, Uri, ModuleData);
completion_items([{atom, _, _}], #{whitespace := true}, ZoneType, Uri, ModuleData) ->
    general_completion_items(undefined, ZoneType, Uri, ModuleData);
completion_items([], _, ZoneType, Uri, ModuleData) ->
    general_completion_items(undefined, ZoneType, Uri, ModuleData);
completion_items(ignore, #{whitespace := true}, ZoneType, Uri, ModuleData) ->
    general_completion_items(undefined, ZoneType, Uri, ModuleData);
completion_items(_, _, _, _, _) ->
    [].

-spec general_completion_items(Filter :: binary() | undefined,
                               ZoneType :: zone_type(),
                               Uri :: uri(),
                               ModuleData :: module_data()) -> [item()].
general_completion_items(Filter, ZoneType, Uri, ModuleData) ->
    if
        ZoneType =:= behavior ->
            behavior_completion_items(Filter);
        ZoneType =:= optional_callbacks ->
            optional_callbacks_completion_items(ModuleData, Filter);
        ZoneType =:= export ->
            module_function_completion_items(ModuleData, Filter, arity);
        ZoneType =:= export_type ->
            module_type_completion_items(ModuleData, Filter, arity);
        is_tuple(ZoneType) andalso element(1, ZoneType) =:= import ->
            remote_function_completion_items(element(2, ZoneType), Filter, arity);
        ZoneType =:= type orelse
        ZoneType =:= spec_body ->
            module_completion_items(Filter)
                ++ local_type_completion_items(ModuleData, Filter, Uri, args)
                ++ bit_completion_items(Filter, args)
                ++ atom_completion_items(Filter);
        ZoneType =:= spec_name ->
            local_function_completion_items(ModuleData, Filter, Uri, args);
        ZoneType =:= record_body ->
            module_completion_items(Filter)
                ++ local_function_completion_items(ModuleData, Filter, Uri, args)
                ++ import_completion_items(ModuleData, undefined, Uri, args)
                ++ bif_completion_items(Filter, args)
                ++ local_type_completion_items(ModuleData, Filter, Uri, args)
                ++ bit_completion_items(Filter, args)
                ++ atom_completion_items(Filter);
        is_tuple(ZoneType) andalso element(1, ZoneType) =:= function orelse
        ZoneType =:= outer ->
            module_completion_items(Filter)
                ++ local_function_completion_items(ModuleData, Filter, Uri, args)
                ++ import_completion_items(ModuleData, undefined, Uri, args)
                ++ bif_completion_items(Filter, args)
                ++ keyword_completion_items(Filter)
                ++ atom_completion_items(Filter);
        true ->
            []
    end.

%%%-------------------------------------------------------------------
%%% Modules
%%%-------------------------------------------------------------------
-spec module_completion_items(Filter :: binary() | undefined) -> [item()].
module_completion_items(Filter) ->
    Modules0 = get_all_module_name(),
    Modules1 = filter_names(Filter, Modules0),
    [ #{<<"label">> => esrv_req_lib:format_atom(M),
        <<"kind">> => ?COMPLETION_ITEM_KIND_MODULE,
        <<"sortText">> => ?SORT_MODULE} || M <- Modules1 ].

%%%-------------------------------------------------------------------
%%% Behaviors
%%%-------------------------------------------------------------------
-spec behavior_completion_items(Filter :: binary() | undefined) -> [item()].
behavior_completion_items(Filter) ->
    Behaviors0 = get_all_behaviors(),
    Behaviors1 = filter_names(Filter, Behaviors0),
    [ #{<<"label">> => esrv_req_lib:format_atom(B),
        <<"kind">> => ?COMPLETION_ITEM_KIND_MODULE,
        <<"sortText">> => ?SORT_BEHAVIOR} || B <- Behaviors1 ].

%%%-------------------------------------------------------------------
%%% Optional callbacks
%%%-------------------------------------------------------------------
-spec optional_callbacks_completion_items(ModuleData :: module_data(),
                                          Filter :: binary() | undefined) -> [item()].
optional_callbacks_completion_items(#module_data{callback = Callback}, Filter) ->
    NameArities = filter_name_arities(Filter, Callback),
    [ #{<<"label">> => esrv_req_lib:format_name_arity(Name, Arity),
        <<"kind">> => ?COMPLETION_ITEM_KIND_MODULE,
        <<"sortText">> => ?SORT_OPTIONAL_CALLBACK} || {Name, Arity} <- NameArities ].

%%%-------------------------------------------------------------------
%%% Functions
%%%-------------------------------------------------------------------
-spec remote_function_completion_items(ModuleName :: name(),
                                       Filter :: binary() | undefined,
                                       Type :: args | arity) -> [item()].
remote_function_completion_items(ModuleName, Filter, Type) ->
    FunctionsData = esrv_req_lib:collect_remote_function(ModuleName),
    function_items(FunctionsData, Filter, Type, ?SORT_FUNCTION, [{<<"module_name">>, ModuleName}]).

-spec local_function_completion_items(ModuleData :: module_data(),
                                      Filter :: binary() | undefined,
                                      Uri :: uri(),
                                      Type :: args | arity) -> [item()].
local_function_completion_items(ModuleData, Filter, Uri, Type) ->
    FunctionsData = esrv_req_lib:collect_local_function(undefined, ModuleData),
    function_items(FunctionsData, Filter, Type, ?SORT_FUNCTION, [{<<"uri">>, Uri}]).

-spec module_function_completion_items(ModuleData :: module_data(),
                                       Filter :: binary() | undefined,
                                       Type :: args | arity) -> [item()].
module_function_completion_items(#module_data{functions = Functions} = ModuleData, Filter, Type) ->
    FunctionsData =
        lists:map(fun(NameArity) ->
                          {NameArity, esrv_req_lib:form_signature(NameArity, ModuleData)}
                  end, maps:keys(Functions)),
    function_items(FunctionsData, Filter, Type, ?SORT_FUNCTION, []).

-spec import_completion_items(ModuleData :: module_data(),
                              Filter :: binary() | undefined,
                              Uri :: uri(),
                              Type :: args | arity) -> [item()].
import_completion_items(ModuleData, Filter, Uri, Type) ->
    lists:foldl(fun(Import, Acc) ->
                        import_completion_items(Import, Filter, Type)  ++ Acc
                end, [], esrv_req_lib:collect_imported(Uri, ModuleData)).

-spec import_completion_items(Import :: import(),
                              Filter :: binary() | undefined,
                              Type :: args | arity) -> [item()].
import_completion_items({ModuleName, NameArity}, Filter, Type) ->
    FunctionsData0 = esrv_req_lib:collect_remote_function(ModuleName),
    FunctionsData1 = lists:filter(fun({NA, _}) -> NA =:= NameArity end, FunctionsData0),
    function_items(FunctionsData1, Filter, Type, ?SORT_FUNCTION, [{<<"module_name">>, ModuleName}]);
import_completion_items(ModuleName, Filter, Type) ->
    remote_function_completion_items(ModuleName, Filter, Type).

-spec bif_completion_items(Filter :: binary() | undefined, Type :: args | arity) -> [item()].
bif_completion_items(Filter, Type) ->
    FunctionsData = esrv_req_lib:collect_remote_function(erlang),
    function_items(FunctionsData, Filter, Type, ?SORT_BIF, [{<<"module_name">>, erlang}]).

-spec function_items(FunctionsData :: [{name_arity(), signature()}],
                     Filter :: binary() | undefined,
                     Type :: args | arity,
                     SortText :: binary(),
                     ItemData :: jsx:json_term()) -> [item()].
function_items(FunctionsData, Filter, Type, SortText, ItemData0) ->
    ItemData1 = [{<<"data_type">>, ?COMPLETION_ITEM_KIND_FUNCTION} | ItemData0],
    lists:foldl(fun({{Name, Arity}, Signature}, Acc) ->
                        case apply_filter(Filter, atom_to_binary(Name, utf8)) of
                            true when Type =:= arity ->
                                [#{<<"label">> => esrv_req_lib:format_name_arity(Name, Arity),
                                   <<"kind">> => ?COMPLETION_ITEM_KIND_FUNCTION,
                                   <<"sortText">> => SortText,
                                   <<"insertText">> => esrv_req_lib:format_name_arity(Name, Arity),
                                   <<"data">> => ItemData1} | Acc];
                            true when Type =:= args ->
                                [#{<<"label">> => esrv_req_lib:format_name_arity(Name, Arity),
                                   <<"kind">> => ?COMPLETION_ITEM_KIND_SNIPPET,
                                   <<"sortText">> =>
                                       iolist_to_binary(io_lib:format("~s_~s_~3.10.0b",
                                                                      [?SORT_SNIPPET,
                                                                       Name,
                                                                       length(Signature)])),
                                   <<"insertText">> => format_name_args_snippet(Name, Signature),
                                   <<"insertTextFormat">> => ?INSERT_TEXT_FORMAT_SNIPPET,
                                   <<"data">> => ItemData1} | Acc];
                            false ->
                                Acc
                        end
                end, [], lists:usort(FunctionsData)).

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------
-spec remote_type_completion_items(ModuleName :: module(),
                                   Filter :: binary() | undefined,
                                   Type :: args | arity) -> [item()].
remote_type_completion_items(ModuleName, Filter, Type) ->
    Types = esrv_req_lib:collect_remote_type(ModuleName),
    type_items(Types, Filter, Type, ?SORT_TYPE, [{<<"module_name">>, ModuleName}]).

-spec local_type_completion_items(ModuleData :: module_data(),
                                  Filter :: binary() | undefined,
                                  Uri :: uri(),
                                  Type :: args | arity) -> [item()].
local_type_completion_items(ModuleData, Filter, Uri, Type) ->
    Types = esrv_req_lib:collect_local_type(undefined, ModuleData),
    type_items(Types, Filter, Type, ?SORT_TYPE, [{<<"uri">>, Uri}]).

-spec module_type_completion_items(ModuleData :: module_data(),
                                   Filter :: binary() | undefined,
                                   Type :: args | arity) -> [item()].
module_type_completion_items(#module_data{types = Types}, Filter, Type) ->
    ModuleTypes = maps:keys(Types),
    type_items(ModuleTypes, Filter, Type, ?SORT_TYPE, []).

-spec bit_completion_items(Filter :: binary() | undefined, Type :: args | arity) -> [item()].
bit_completion_items(Filter, Type) ->
    Bits = esrv_req_lib:get_bits(),
    type_items(Bits, Filter, Type, ?SORT_BIT, []).

-spec type_items(Types :: [name_arity()],
                 Filter :: binary() | undefined,
                 Type :: args | arity,
                 SortText :: binary(),
                 ItemData :: jsx:json_term()) -> [item()].
type_items(Types, Filter, Type, SortText, ItemData0) ->
    ItemData1 = [{<<"data_type">>, ?COMPLETION_ITEM_KIND_TYPE_PARAM} | ItemData0],
    lists:foldl(fun({Name, Arity}, Acc) ->
                        case apply_filter(Filter, atom_to_binary(Name, utf8)) of
                            true when Type =:= arity ->
                                [#{<<"label">> => esrv_req_lib:format_name_arity(Name, Arity),
                                   <<"kind">> => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
                                   <<"sortText">> => SortText,
                                   <<"insertText">> => esrv_req_lib:format_name_arity(Name, Arity),
                                   <<"data">> => ItemData1} | Acc];
                            true when Type =:= args ->
                                Signature = type_signature(Arity),
                                [#{<<"label">> => esrv_req_lib:format_name_arity(Name, Arity),
                                   <<"kind">> => ?COMPLETION_ITEM_KIND_SNIPPET,
                                   <<"sortText">> =>
                                       iolist_to_binary(io_lib:format("~s_~s_~3.10.0b",
                                                                      [?SORT_SNIPPET,
                                                                       Name,
                                                                       length(Signature)])),
                                   <<"insertText">> => format_name_args_snippet(Name, Signature),
                                   <<"insertTextFormat">> => ?INSERT_TEXT_FORMAT_SNIPPET,
                                   <<"data">> => ItemData1} | Acc];
                            false ->
                                Acc
                        end
                end, [], lists:usort(Types)).

-spec type_signature(Arity :: arity()) -> signature().
type_signature(Arity) ->
    type_signature(Arity, []).

-spec type_signature(N :: non_neg_integer(), Names :: [name()]) -> signature().
type_signature(0, Acc) ->
    Acc;
type_signature(N, Acc) ->
    type_signature(N - 1, [list_to_atom("Arg" ++ integer_to_list(N)) | Acc]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
-spec macros_completion_items(Filter :: binary() | undefined,
                              Uri :: uri(),
                              ModuleData :: module_data()) -> [item()].
macros_completion_items(Filter, Uri, ModuleData) ->
    Macros0 =
        maps:fold(fun(_, Macros, Acc) ->
                          maps:merge(Macros, Acc)
                  end, #{}, esrv_req_lib:collect_macros(Uri, ModuleData)),
    Macros1 =
        maps:filter(fun({Name, _}, _) ->
                            apply_filter(Filter, atom_to_binary(Name, utf8))
                    end, Macros0),
    Items =
        maps:fold(fun({Name, undefined}, _, Acc) ->
                          [#{<<"label">> => esrv_req_lib:format_macro_name(Name),
                             <<"kind">> => ?COMPLETION_ITEM_KIND_CONSTANT,
                             <<"sortText">> => ?SORT_MACRO,
                             <<"data">> => [{<<"uri">>, Uri}]} | Acc];
                     ({Name, Arity}, #macro_definition{args = Args}, Acc) ->
                          Signature = [ N || {N, _} <- lists:keysort(2, maps:to_list(Args)) ],
                          [#{<<"label">> => esrv_req_lib:format_macro_name_arity(Name, Arity),
                             <<"kind">> => ?COMPLETION_ITEM_KIND_SNIPPET,
                             <<"sortText">> => iolist_to_binary(io_lib:format("~s_~s_~3.10.0b",
                                                                              [?SORT_SNIPPET,
                                                                               Name,
                                                                               Arity])),
                             <<"insertText">> => format_macro_name_args_snippet(Name, Signature),
                             <<"insertTextFormat">> => ?INSERT_TEXT_FORMAT_SNIPPET,
                             <<"data">> => [{<<"data_type">>, ?COMPLETION_ITEM_KIND_CONSTANT},
                                            {<<"uri">>, Uri}]} | Acc]
                  end, [], Macros1),
    lists:usort(Items).

-spec predefined_macros_completion_items(Filter :: binary() | undefined, Uri :: uri()) ->
          [item()].
predefined_macros_completion_items(Filter, Uri) ->
    lists:foldl(fun(Value, Acc) ->
                        case apply_filter(Filter, Value) of
                            true ->
                                [#{<<"label">> => Value,
                                   <<"kind">> => ?COMPLETION_ITEM_KIND_CONSTANT,
                                   <<"sortText">> => ?SORT_MACRO,
                                   <<"data">> => [{<<"uri">>, Uri}]} | Acc];
                            false ->
                                Acc
                        end
                end, [], [<<"MODULE">>,
                          <<"MODULE_STRING">>,
                          <<"FILE">>,
                          <<"LINE">>,
                          <<"MACHINE">>,
                          <<"FUNCTION_NAME">>,
                          <<"FUNCTION_ARITY">>,
                          <<"OTP_RELEASE">>]).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
-spec record_completion_items(Filter :: binary() | undefined,
                              Uri :: uri(),
                              ModuleData :: module_data()) -> [item()].
record_completion_items(Filter, Uri, ModuleData) ->
    Records = esrv_req_lib:collect_records(undefined, ModuleData),
    RecordsName = filter_names(Filter, maps:keys(Records)),
    [ #{<<"label">> => esrv_req_lib:format_atom(RN),
        <<"kind">> => ?COMPLETION_ITEM_KIND_STRUCT,
        <<"sortText">> => ?SORT_RECORD,
        <<"data">> => [{<<"uri">>, Uri},
                       {<<"record_name">>, RN}]} || RN <- lists:usort(RecordsName) ].

-spec record_fields_completion_items(RecordName :: name(),
                                     Filter :: binary() | undefined,
                                     ModuleData :: module_data()) -> [item()].
record_fields_completion_items(RecordName, Filter, ModuleData) ->
    case esrv_req_lib:get_record_data(undefined, ModuleData, RecordName) of
        {ok, {_, #record_data{record_fields = RecordFields}}} ->
            RecordFieldNames = filter_names(Filter, maps:keys(RecordFields)),
            [ #{<<"label">> => esrv_req_lib:format_atom(RFN),
                <<"kind">> => ?COMPLETION_ITEM_KIND_FIELD,
                <<"sortText">> => ?SORT_RECORD_FIELD} || RFN <- RecordFieldNames ];
        undefined ->
            []
    end.

%%%-------------------------------------------------------------------
%%% Attributes
%%%-------------------------------------------------------------------
-spec attributes_completion_items(Filter :: binary() | undefined) -> [item()].
attributes_completion_items(Filter) ->
    lists:foldl(fun({Label, SnippetPostfix}, Acc) ->
                        case apply_filter(Filter, Label) of
                            true when SnippetPostfix =:= undefined ->
                                [#{<<"label">> => Label,
                                   <<"kind">> => ?COMPLETION_ITEM_KIND_TEXT,
                                   <<"insertText">> => <<Label/binary, " ">>,
                                   <<"sortText">> => ?SORT_ATTRIBUTE} | Acc];
                            true ->
                                [#{<<"label">> => Label,
                                   <<"kind">> => ?COMPLETION_ITEM_KIND_SNIPPET,
                                   <<"insertText">> => <<Label/binary, SnippetPostfix/binary>>,
                                   <<"insertTextFormat">> => ?INSERT_TEXT_FORMAT_SNIPPET,
                                   <<"sortText">> => ?SORT_ATTRIBUTE} | Acc];
                            false ->
                                Acc
                        end
                end, [], [{<<"module">>, <<"(${1:}).">>},
                          {<<"export">>, <<"([${1:}]).">>},
                          {<<"export_type">>, <<"([${1:}]).">>},
                          {<<"import">>, <<"(${1:module_name}, [${2:}]).">>},
                          {<<"compile">>, <<"([${1:}]).">>},
                          {<<"vsn">>, <<"(${1:version}).">>},
                          {<<"on_load">>, <<"(${1:function}).">>},
                          {<<"behaviour">>, <<"(${1:}).">>},
                          {<<"callback">>, undefined},
                          {<<"optional_callbacks">>, <<"([${1:}]).">>},
                          {<<"record">>, <<"(${1:record_name}, {${2:}}).">>},
                          {<<"include">>, <<"(${1:}).">>},
                          {<<"include_lib">>, <<"(${1:}).">>},
                          {<<"define">>, <<"(${1:NAME}, ${2:}).">>},
                          {<<"file">>, <<"(${1:file}, ${2:line}).">>},
                          {<<"type">>, undefined},
                          {<<"spec">>, undefined}]).

%%%-------------------------------------------------------------------
%%% Keywords
%%%-------------------------------------------------------------------
-spec keyword_completion_items(Filter :: binary() | undefined) -> [item()].
keyword_completion_items(Filter) ->
    lists:foldl(fun(Value, Acc) ->
                        case apply_filter(Filter, Value) of
                            true ->
                                [#{<<"label">> => Value,
                                   <<"kind">> => ?COMPLETION_ITEM_KIND_KEYWORD,
                                   <<"sortText">> => ?SORT_KEYWORD} | Acc];
                            false ->
                                Acc
                        end
                end, [], [<<"after">>,
                          <<"and">>,
                          <<"andalso">>,
                          <<"band">>,
                          <<"begin">>,
                          <<"bnot">>,
                          <<"bor">>,
                          <<"bsl">>,
                          <<"bsr">>,
                          <<"bxor">>,
                          <<"case">>,
                          <<"catch">>,
                          %%<<"cond">>,
                          <<"div">>,
                          <<"end">>,
                          <<"fun">>,
                          <<"if">>,
                          %%<<"let">>,
                          <<"not">>,
                          <<"of">>,
                          <<"or">>,
                          <<"orelse">>,
                          <<"receive">>,
                          <<"rem">>,
                          <<"try">>,
                          <<"when">>,
                          <<"xor">>]).

%%%-------------------------------------------------------------------
%%% Atoms
%%%-------------------------------------------------------------------
-spec atom_completion_items(Filter :: binary() | undefined) -> [item()].
atom_completion_items(Filter) ->
    lists:foldl(fun(Value, Acc) ->
                        case apply_filter(Filter, Value) of
                            true ->
                                [#{<<"label">> => Value,
                                   <<"kind">> => ?COMPLETION_ITEM_KIND_CONSTANT,
                                   <<"sortText">> => ?SORT_ATOM} | Acc];
                            false ->
                                Acc
                        end
                end, [], [<<"atomic">>,
                          <<"value">>,
                          <<"true">>,
                          <<"false">>,
                          <<"null">>,
                          <<"none">>,
                          <<"undefined">>,
                          <<"unlimited">>,
                          <<"not_found">>,
                          <<"error">>,
                          <<"trap_exit">>,
                          <<"noreply">>,
                          <<"reply">>,
                          <<"reply_and_hibernate">>,
                          <<"next_state">>,
                          <<"next_event">>,
                          <<"keep_state">>,
                          <<"keep_state_and_data">>,
                          <<"state_timeout">>,
                          <<"asn1_NOVALUE">>]).

%%%-------------------------------------------------------------------
%%% Utils
%%%-------------------------------------------------------------------
-spec get_zone_info(CursorLocation :: location(), ModuleData :: module_data()) ->
          {zone_type(), location() | undefined}.
get_zone_info(CursorLocation, #module_data{zones = Zones}) ->
    case esrv_req_lib:get_zone(CursorLocation, Zones) of
        {ok, #zone{type = ZoneType, start_location = StartLocation}} ->
            {ZoneType, StartLocation};
        undefined ->
            {outer, undefined}
    end.

-spec get_lines_data(Uri :: uri(),
                     ZoneType :: zone_type(),
                     StartLocation :: location() | undefined,
                     CursorLocation :: location()) -> [binary()].
get_lines_data(Uri, {function, _}, {StartLine, _}, {CursorLine, _}) ->
    esrv_lib:fetch_text_document_lines(Uri, StartLine, CursorLine);
get_lines_data(Uri, _, _, {CursorLine, _}) ->
    esrv_lib:fetch_text_document_lines(Uri, CursorLine, CursorLine).

-spec parse_cursor_line(LineData :: binary(),
                        Column :: column(),
                        ModuleData :: module_data()) ->
          {[erl_scan:token()] | ignore, line_info(), line_info()}.
parse_cursor_line(LineData, 1, _) ->
    {[], left_line_info(<<>>), right_line_info(LineData)};
parse_cursor_line(LineData0, Column, ModuleData) ->
    Bound = Column - 1,
    <<LineData1:Bound/binary, LineLeft/binary>> = LineData0,
    {case binary:last(LineData1) of
         $' ->
             ignore;
         _ ->
             LineData2 =
                 case ModuleData of
                     #module_data{module_name = {Module, _}} ->
                         re:replace(LineData1, <<"\\?MODULE">>, atom_to_binary(Module, utf8),
                                    [{return, binary}, global]);
                     _ ->
                         LineData1
                 end,
             line_to_reversed_tokens(LineData2)
     end, left_line_info(LineData1), right_line_info(LineLeft)}.

-spec left_line_info(LeftLine :: binary()) -> line_info().
left_line_info(LeftLine) ->
    #{whitespace => case LeftLine of <<>> -> true; _ -> binary:last(LeftLine) =:= $\s end,
      last_word => case re:run(LeftLine, <<"(\\w*)$">>, [{capture, all_but_first, binary}]) of
                       {match, [LastWord]} when byte_size(LastWord) > 0 ->
                                                 LastWord;
                                           _ ->
                                                 undefined
                                         end}.

-spec right_line_info(RightLine :: binary()) -> line_info().
right_line_info(RightLine) ->
    #{parentheses => re:run(RightLine, <<"^ *\\( *\\)">>) =/= nomatch,
      whitespace => re:run(RightLine, <<"^\\S">>) =:= nomatch,
      next_word => case re:run(RightLine, <<"^(\\w*)">>, [{capture, all_but_first, binary}]) of
                       {match, [NextWord]} when byte_size(NextWord) > 0 ->
                           NextWord;
                       _ ->
                           undefined
                   end}.

-spec check_record_field(ReversedTokens :: [erl_scan:token()] | ignore,
                         LinesDataLeft :: [binary()],
                         LeftLineInfo :: line_info(),
                         StartLocation :: location()) ->
          {ok, name(), binary() | undefined} | undefined.
check_record_field(ReversedTokens,
                   LinesDataLeft0,
                   #{whitespace := Whitespace},
                   {_, StartColumn}) when ReversedTokens =/= ignore ->
    {PrevToken, ReversedTokensLeft, LinesDataLeft1} =
        get_prev_token(ReversedTokens, LinesDataLeft0, StartColumn),
    case PrevToken of
        {Tag, _, Value} when (Tag =:= atom orelse Tag =:= var) andalso not Whitespace ->
            case find_record_field(PrevToken, ReversedTokensLeft, LinesDataLeft1, StartColumn) of
                {ok, RecordName} ->
                    {ok, RecordName, atom_to_binary(Value, utf8)};
                undefined ->
                    undefined
            end;
        {Tag, _} when Tag =:= '{' orelse Tag =:= ',' ->
            case find_record_field(PrevToken, ReversedTokensLeft, LinesDataLeft1, StartColumn) of
                {ok, RecordName} ->
                    {ok, RecordName, undefined};
                undefined ->
                    undefined
            end;
        _ ->
            undefined
    end;
check_record_field(_, _, _, _) ->
    undefined.

-spec find_record_field(Token :: erl_scan:token() | undefined,
                        ReversedTokensLeft :: [erl_scan:token()],
                        LinesDataLeft :: [binary()],
                        StartColumn :: column()) -> {ok, name()} | undefined.
find_record_field(undefined, _, _, _) ->
    undefined;
find_record_field({'{', _}, ReversedTokensLeft, LinesDataLeft, StartColumn) ->
    try_record_field(ReversedTokensLeft, LinesDataLeft, StartColumn);
find_record_field(Token, ReversedTokensLeft0, LinesDataLeft0, StartColumn) ->
    case check_context(Token, ReversedTokensLeft0, LinesDataLeft0, StartColumn, []) of
        {ok, ReversedTokensLeft1, LinesDataLeft1} ->
            {PrevToken, ReversedTokensLeft2, LinesDataLeft2} =
                get_prev_token(ReversedTokensLeft1, LinesDataLeft1, StartColumn),
            find_record_field(PrevToken, ReversedTokensLeft2, LinesDataLeft2, StartColumn);
        undefined ->
            undefined
    end.

-spec try_record_field(ReversedTokensLeft :: [erl_scan:token()],
                       LinesDataLeft :: [binary()],
                       StartColumn :: column()) -> {ok, name()} | undefined.
try_record_field(ReversedTokensLeft0, LinesDataLeft0, StartColumn) ->
    {PrevToken, ReversedTokensLeft1, LinesDataLeft1} =
        get_prev_token(ReversedTokensLeft0, LinesDataLeft0, StartColumn),
    case PrevToken of
        {Tag, _, Value} when Tag =:= atom orelse Tag =:= var ->
            case get_prev_token(ReversedTokensLeft1, LinesDataLeft1, StartColumn) of
                {{'#', _}, _, _} ->
                    {ok, Value};
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

-spec check_context(Token :: erl_scan:token() | undefined,
                    ReversedTokensLeft :: [erl_scan:token()],
                    LinesDataLeft :: [binary()],
                    StartColumn :: column(),
                    Stack :: [erl_scan:token()]) ->
          {ok, [erl_scan:token()], [binary()]} | undefined.
check_context(Token, ReversedTokensLeft, LinesDataLeft, StartColumn, Stack) ->
    case Token of
        {Sign, _} when Sign =:= ')' orelse Sign =:= '}' orelse Sign =:= 'end' ->
            check_context_next(ReversedTokensLeft, LinesDataLeft, StartColumn, [Token | Stack]);
        {Sign, _} when Sign =:= '(' orelse Sign =:= '{' orelse Sign =:= 'begin' ->
            case Stack of
                [{')', _} | T] when Sign =:= '(' ->
                    check_context_closed(ReversedTokensLeft, LinesDataLeft, StartColumn, T);
                [{'}', _} | T] when Sign =:= '{' ->
                    check_context_closed(ReversedTokensLeft, LinesDataLeft, StartColumn, T);
                [{'end', _} | T] when Sign =:= 'begin' ->
                    check_context_closed(ReversedTokensLeft, LinesDataLeft, StartColumn, T);
                _ ->
                    undefined
            end;
        undefined ->
            undefined;
        _ ->
            check_context_closed(ReversedTokensLeft, LinesDataLeft, StartColumn, Stack)
    end.

-spec check_context_closed(ReversedTokensLeft :: [erl_scan:token()],
                           LinesDataLeft :: [binary()],
                           StartColumn :: column(),
                           Stack :: [erl_scan:token()]) ->
          {ok, [erl_scan:token()], [binary()]} | undefined.
check_context_closed(ReversedTokensLeft, LinesDataLeft, _, []) ->
    {ok, ReversedTokensLeft, LinesDataLeft};
check_context_closed(ReversedTokensLeft, LinesDataLeft, StartColumn, Stack) ->
    check_context_next(ReversedTokensLeft, LinesDataLeft, StartColumn, Stack).

-spec check_context_next(ReversedTokensLeft :: [erl_scan:token()],
                         LinesDataLeft :: [binary()],
                         StartColumn :: column(),
                         Stack :: [erl_scan:token()]) ->
          {ok, [erl_scan:token()], [binary()]} | undefined.
check_context_next(ReversedTokensLeft0, LinesDataLeft0, StartColumn, Stack) ->
    {PrevToken, ReversedTokensLeft1, LinesDataLeft1} =
        get_prev_token(ReversedTokensLeft0, LinesDataLeft0, StartColumn),
    check_context(PrevToken, ReversedTokensLeft1, LinesDataLeft1, StartColumn, Stack).

-spec get_prev_token(ReversedTokensLeft :: [erl_scan:token()],
                     LinesDataLeft :: [binary()],
                     StartColumn :: column()) ->
          {erl_scan:token() | undefined, [erl_scan:token()], [binary()]}.
get_prev_token([], [], _) ->
    {undefined, [], []};
get_prev_token([], [LineData0 | LinesDataLeft], StartCharacter) ->
    LineData1 =
        case LinesDataLeft of
            [] when StartCharacter > 1 ->
                Bound = StartCharacter - 1,
                <<_:Bound/binary, LineData00/binary>> = LineData0,
                LineData00;
            _ ->
                LineData0
        end,
    ReversedTokens = line_to_reversed_tokens(LineData1),
    get_prev_token(ReversedTokens, LinesDataLeft, StartCharacter);
get_prev_token([Token | ReversedTokensLeft], LinesDataLeft, _) ->
    {Token, ReversedTokensLeft, LinesDataLeft}.

-spec line_to_reversed_tokens(LineData :: binary()) -> [erl_scan:token()].
line_to_reversed_tokens(LineData) ->
    case erl_scan:string(binary_to_list(LineData), {1, 1}) of
        {ok, Tokens, _} ->
            lists:reverse(Tokens);
        _ ->
            [{dot, {1, 1}}]
    end.

-spec post_process_items(CursorLocation :: location(),
                         LeftLineInfo :: line_info(),
                         RightLineInfo :: line_info(),
                         Uri :: uri(),
                         ModuleData :: module_data(),
                         Items :: [item()]) -> [item()].
post_process_items(CursorLocation, LeftLineInfo, RightLineInfo, Uri, ModuleData, Items0) ->
    Items1 = check_inside_position(LeftLineInfo, RightLineInfo, Items0),
    Items2 = remove_parentheses(RightLineInfo, Items1),
    exclude_function_definition(CursorLocation, Uri, ModuleData, Items2).

-spec check_inside_position(LeftLineInfo :: line_info(),
                            RightLineInfo :: line_info(),
                            Items :: [item()]) -> [item()].
check_inside_position(#{last_word := LastWord}, #{next_word := NextWord}, Items)
  when is_binary(LastWord) andalso is_binary(NextWord) ->
    DiscardItems =
        lists:any(fun(#{<<"label">> := Label}) ->
                          WholeWord = <<LastWord/binary, NextWord/binary>>,
                          esrv_lib:is_binary_prefix(WholeWord, Label)
                  end, Items),
    case DiscardItems of true -> []; false -> Items end;
check_inside_position(_, _, Items) ->
    Items.

-spec remove_parentheses(RightLineInfo :: line_info(), Items :: [item()]) -> [item()].
remove_parentheses(#{parentheses := false}, Items) ->
    Items;
remove_parentheses(#{parentheses := true}, Items0) ->
    lists:map(fun(#{<<"kind">> := ?COMPLETION_ITEM_KIND_SNIPPET,
                    <<"insertText">> := InsertText0} = Item0) ->
                      RegExp = <<"^(.*)\\(\\)$">>,
                      case re:run(InsertText0, RegExp, [{capture, all_but_first, binary}]) of
                          {match, [InsertText1]} ->
                              Item0#{<<"insertText">> := InsertText1};
                          nomatch ->
                              Item0
                      end;
                 (Item) ->
                      Item
              end, Items0).

-spec exclude_function_definition(CursorLocation :: location(),
                                  Uri :: uri(),
                                  ModuleData :: module_data(),
                                  CompletionItems :: [item()]) -> [item()].
exclude_function_definition({Line, Column}, Uri, ModuleData, CompletionItems) ->
    case esrv_req_lib:get_poi({Line, Column - 1}, Uri, ModuleData) of
        {ok, #poi{data = {local_function, NameArity},
                  start_location = Location,
                  definition = {local, Location}}} ->
            exclude_function_definition(NameArity, CompletionItems);
        {ok, #poi{data = {function_clause, NameArity}}} ->
            exclude_function_definition(NameArity, CompletionItems);
        _ ->
            CompletionItems
    end.

-spec exclude_function_definition(NameArity :: name_arity(), CompletionItems :: [item()]) ->
          [item()].
exclude_function_definition({Name, Arity}, CompletionItems) ->
    ToExclude = esrv_req_lib:format_name_arity(Name, Arity),
    lists:filter(fun(#{<<"label">> := Label,
                       <<"kind">> := ?COMPLETION_ITEM_KIND_SNIPPET,
                       <<"data">> := [{<<"data_type">>,
                                       ?COMPLETION_ITEM_KIND_FUNCTION} | _]}) ->
                         Label =/= ToExclude;
                    (_) ->
                         true
                 end, CompletionItems).

-spec filter_names(Filter :: binary() | undefined, Names :: [name()]) -> [name()].
filter_names(undefined, Names) ->
    Names;
filter_names(Filter, Names) ->
    lists:filter(fun(Name) -> apply_filter(Filter, atom_to_binary(Name, utf8)) end, Names).

-spec filter_name_arities(Filter :: binary() | undefined, NameArities :: [name_arity()]) ->
          [name_arity()].
filter_name_arities(undefined, NameArities) ->
    NameArities;
filter_name_arities(Filter, NameArities) ->
    lists:filter(fun({Name, _}) ->
                         apply_filter(Filter, atom_to_binary(Name, utf8)) end,
                 NameArities).

-spec apply_filter(Filter :: binary() | undefined, Value :: binary()) -> boolean().
apply_filter(undefined, _) ->
    true;
apply_filter(Filter, Value) ->
    esrv_lib:is_binary_prefix(Filter, Value).

-spec get_all_module_name() -> [name()].
get_all_module_name() ->
    lists:usort(esrv_db:get_all_module_names()).

-spec get_all_behaviors() -> [name()].
get_all_behaviors() ->
    lists:usort(esrv_db:get_all_behaviors()).

-spec format_name_args_snippet(Name :: atom(), Args :: [name()]) -> binary().
format_name_args_snippet(Name, Args) ->
    iolist_to_binary([esrv_req_lib:format_atom(Name),
                      <<"(">>,
                      format_args_snippet(Args)]).

-spec format_macro_name_args_snippet(Name :: atom(), Args :: [name()]) -> binary().
format_macro_name_args_snippet(Name, Args) ->
    iolist_to_binary([esrv_req_lib:format_macro_name(Name),
                      <<"(">>,
                      format_args_snippet(Args)]).

-spec format_args_snippet(Args :: [name()]) -> binary().
format_args_snippet(Args0) ->
    {Args1, _} =
        lists:foldr(fun(Arg, {Acc, N}) ->
                            {[[<<"${">>,
                               integer_to_binary(N),
                               <<":">>,
                               atom_to_binary(Arg, utf8),
                               <<"}">>] | Acc], N - 1}
                    end, {[], length(Args0)}, Args0),
    iolist_to_binary([lists:join(<<", ">>, Args1), <<")">>]).

-spec var_to_atom(Token :: erl_scan:token()) -> erl_scan:token().
var_to_atom({var, Anno, Value}) ->
    {atom, Anno, Value}.
