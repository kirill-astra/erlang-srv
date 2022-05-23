-module(esrv_req_completion_resolve).

-behaviour(esrv_request_processor).

%% API
-export([process/1]).

-include("types.hrl").
-include("parser.hrl").
-include("protocol.hrl").
-include("records.hrl").

-type item() :: jsx:json_term().

-spec process(Request :: request()) -> {response, response()}.
process(#request{id = Id, params = Item0}) ->
    Item1 =
        case parse_data(Item0) of
            {ok, ParsedData} ->
                MarkupKind = markup_kind(),
                case resolve(MarkupKind, ParsedData, Item0) of
                    {ok, Description} ->
                        Item0#{<<"documentation">> => #{kind => MarkupKind, value => Description}};
                    undefined ->
                        Item0
                end;
            error ->
                Item0
        end,
    {response, #response{id = Id, result = Item1}}.

-spec markup_kind() -> markup_kind().
markup_kind() ->
    Path = [<<"textDocument">>, <<"completion">>, <<"completionItem">>, <<"documentationFormat">>],
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

-spec parse_data(Item :: item()) -> {ok, any()} | error.
parse_data(#{<<"kind">> := ?COMPLETION_ITEM_KIND_MODULE, <<"label">> := Label}) ->
    {ok, {module, parse_name(Label)}};
parse_data(#{<<"data">> := #{<<"uri">> := Uri,
                             <<"data_type">> := ?COMPLETION_ITEM_KIND_FUNCTION}}) ->
    {ok, {uri_function, Uri}};
parse_data(#{<<"data">> := #{<<"module_name">> := ModuleName,
                             <<"data_type">> := ?COMPLETION_ITEM_KIND_FUNCTION}})
  when is_binary(ModuleName) ->
    {ok, {module_name_function, parse_name(ModuleName)}};
parse_data(#{<<"data">> := #{<<"uri">> := Uri,
                             <<"data_type">> := ?COMPLETION_ITEM_KIND_TYPE_PARAM}}) ->
    {ok, {uri_type, Uri}};
parse_data(#{<<"data">> := #{<<"module_name">> := ModuleName,
                             <<"data_type">> := ?COMPLETION_ITEM_KIND_TYPE_PARAM}})
  when is_binary(ModuleName) ->
    {ok, {module_name_type, parse_name(ModuleName)}};
parse_data(#{<<"kind">> := ?COMPLETION_ITEM_KIND_STRUCT,
             <<"data">> := #{<<"uri">> := Uri, <<"record_name">> := RecordName}}) ->
    {ok, {Uri, parse_name(RecordName)}};
parse_data(#{<<"kind">> := ?COMPLETION_ITEM_KIND_CONSTANT,
             <<"data">> := #{<<"uri">> := Uri}}) ->
    {ok, Uri};
parse_data(#{<<"data">> := #{<<"uri">> := Uri,
                             <<"data_type">> := ?COMPLETION_ITEM_KIND_CONSTANT}}) ->
    {ok, {uri_macro, Uri}};
parse_data(_) ->
    error.

-spec resolve(MarkupKind :: markup_kind(),
              ParsedData :: any(),
              Item :: item()) -> {ok, binary()} | undefined.
resolve(MarkupKind, {module, ModuleName}, #{<<"kind">> := Kind})
  when Kind =:= ?COMPLETION_ITEM_KIND_MODULE ->
    {ok, esrv_documentation:module_doc(MarkupKind, ModuleName)};

resolve(MarkupKind, {module_name_function, ModuleName}, #{<<"label">> := Label}) ->
    NameArity = parse_name_arity(Label),
    {ok, esrv_documentation:function_doc(MarkupKind, ModuleName, NameArity)};

resolve(MarkupKind, {uri_function, Uri}, #{<<"label">> := Label}) ->
    NameArity = parse_name_arity(Label),
    {ok, ModuleData} = esrv_lib:get_module_data(Uri),
    {ok, esrv_documentation:function_doc(MarkupKind, Uri, ModuleData, NameArity)};

resolve(MarkupKind, {module_name_type, ModuleName}, #{<<"label">> := Label}) ->
    NameArity = parse_name_arity(Label),
    {ok, esrv_documentation:type_doc(MarkupKind, ModuleName, NameArity)};

resolve(MarkupKind, {uri_type, Uri}, #{<<"label">> := Label}) ->
    NameArity = parse_name_arity(Label),
    {ok, ModuleData} = esrv_lib:get_module_data(Uri),
    {ok, esrv_documentation:type_doc(MarkupKind, Uri, ModuleData, NameArity)};

resolve(MarkupKind, {Uri, RecordName}, #{<<"kind">> := ?COMPLETION_ITEM_KIND_STRUCT}) ->
    {ok, esrv_documentation:record_doc(MarkupKind, Uri, RecordName)};

resolve(MarkupKind, Uri, #{<<"label">> := Label, <<"kind">> := Kind})
  when Kind =:= ?COMPLETION_ITEM_KIND_CONSTANT ->
    MacroName = parse_name(Label),
    {ok, esrv_documentation:macros_doc(MarkupKind, Uri, MacroName, undefined)};

resolve(MarkupKind, {uri_macro, Uri}, #{<<"label">> := Label}) ->
    {MacroName, Arity} =  parse_name_arity(Label),
    {ok, esrv_documentation:macros_doc(MarkupKind, Uri, MacroName, Arity)};

resolve(_, _, _) ->
    undefined.

-spec parse_name(Data :: binary()) -> name().
parse_name(<<$', Body0/binary>>) ->
    Body1 = re:replace(Body0, <<"'$">>, <<"">>, [{return, binary}]),
    binary_to_atom(Body1, utf8);
parse_name(Data) ->
    binary_to_atom(Data, utf8).

-spec parse_name_arity(Data :: binary()) -> {name(), arity()}.
parse_name_arity(Data) ->
    [Name, Arity] = binary:split(Data, <<"/">>),
    {parse_name(Name), binary_to_integer(Arity)}.
