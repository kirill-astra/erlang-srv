-module(esrv_edoc).

-export([module_description/2,
         function_description/3]).

-include_lib("xmerl/include/xmerl.hrl").

-include("types.hrl").
-include("parser.hrl").

-define(EXPIRED_AFTER, 5*60*1000).

-spec module_description(MarkupKind :: markup_kind(), Uri :: uri()) -> {ok, binary()} | undefined.
module_description(MarkupKind, Uri) ->
    esrv_lib:substitute_group_leader(fun() ->
                                             module_description2(MarkupKind, Uri)
                                     end).

-spec module_description2(MarkupKind :: markup_kind(), Uri :: uri()) -> {ok, binary()} | undefined.
module_description2(MarkupKind, Uri) ->
    Filename = esrv_lib:uri_to_file(Uri),
    try
        Edoc = get_edoc(Filename),
        {ok, DescriptionElement} = get_element(description, Edoc),
        {ok, FullDescriptionElement} = get_element(fullDescription, DescriptionElement),
        {ok, format(MarkupKind, FullDescriptionElement)}
    catch
        _:_ ->
            undefined
    end.

-spec function_description(MarkupKind :: markup_kind(),
                           Uri :: uri(),
                           NameArity :: name_arity()) -> {ok, binary()} | undefined.
function_description(MarkupKind, Uri, NameArity) ->
    esrv_lib:substitute_group_leader(fun() ->
                                             function_description2(MarkupKind, Uri, NameArity)
                                     end).

-spec function_description2(MarkupKind :: markup_kind(),
                            Uri :: uri(),
                            NameArity :: name_arity()) -> {ok, binary()} | undefined.
function_description2(MarkupKind, Uri, {Name, Arity}) ->
    Filename = esrv_lib:uri_to_file(Uri),
    try
        Edoc = get_edoc(Filename),
        {ok, FunctionsElement} = get_element(functions, Edoc),
        FunctionChecker = fun(V) ->
                                  check_attributes(V, [{name, atom_to_list(Name)},
                                                       {arity, integer_to_list(Arity)}])
                          end,
        FunctionElements = collect_elements(function, FunctionChecker, FunctionsElement),
        lists:foldl(fun(FunctionElement, undefined) ->
                            format_function(MarkupKind, FunctionElement);
                       (_, {ok, Description}) ->
                            {ok, Description}
                    end, undefined, FunctionElements)
    catch
        _:_ ->
            undefined
    end.

-spec get_edoc(Filename :: file:filename()) -> #xmlElement{}.
get_edoc(Filename) ->
    {_, Edoc} = esrv_db:check_cache({edoc, Filename},
                                    fun() ->
                                            Result = edoc:get_doc(Filename, [{private, true}]),
                                            {cache, Result, ?EXPIRED_AFTER}
                                    end),
    Edoc.

-spec format_function(MarkupKind :: markup_kind(), FunctionElement :: #xmlElement{}) ->
          {ok, binary()} | undefined.
format_function(MarkupKind, FunctionElement) ->
    try
        {ok, DescriptionElement} = get_element(description, FunctionElement),
        {ok, FullDescriptionElement} = get_element(fullDescription, DescriptionElement),
        {ok, format(MarkupKind, FullDescriptionElement)}
    catch
        _:_ ->
            undefined
    end.

-spec get_element(Name :: name(), Element :: #xmlElement{}) -> {ok, #xmlElement{}} | undefined.
get_element(Name, #xmlElement{content = Content}) ->
    case lists:keyfind(Name, #xmlElement.name, Content) of
        #xmlElement{} = ResultElement ->
            {ok, ResultElement};
        false ->
            undefined
    end.

-spec collect_elements(Name :: name(),
                       CheckerFun :: fun((#xmlElement{}) -> boolean()),
                       Element :: #xmlElement{}) -> [#xmlElement{}].
collect_elements(Name, CheckerFun, #xmlElement{content = Content}) ->
    [ Element || Element <- Content,
                 is_record(Element, xmlElement),
                 Element#xmlElement.name =:= Name,
                 CheckerFun(Element) ].

-spec check_attributes(Element :: #xmlElement{},
                       ExpectedAttributes :: [{atom(), string() | any}]) -> boolean().
check_attributes(#xmlElement{attributes = Attributes}, ExpectedAttributes) ->
    lists:all(fun({Name, Value}) ->
                      case xmerl_lib:find_attribute(Name, Attributes) of
                          {value, Value} ->
                              true;
                          _ ->
                              false
                      end
              end, ExpectedAttributes).

-spec format(MarkupKind :: markup_kind(), XmlElement :: #xmlElement{}) -> binary().
format(plaintext, XmlElement) ->
    esrv_edoc_plaintext:format(XmlElement);
format(markdown, XmlElement) ->
    esrv_edoc_markdown:format(XmlElement).
