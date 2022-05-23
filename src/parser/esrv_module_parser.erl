-module(esrv_module_parser).

-include("parser.hrl").

%% API
-export([parse/3]).

-type syntax_tree() :: erl_syntax:syntaxTree().

%%%===================================================================
%%% API
%%%===================================================================
-spec parse(Uri :: uri(), TokensInfo :: tokens_info(), FormsInfo :: forms_info()) -> module_info().
parse(Uri, #tokens_info{pois = Pois}, #forms_info{forms = Forms}) ->
    do_parse(Forms, #module_info{uri = Uri,
                                 behaviors = #{},
                                 callback = [],
                                 parse_transform = [],
                                 export = [],
                                 export_type = [],
                                 export_all = false,
                                 import = [],
                                 types = #{},
                                 records = #{},
                                 local_specs = #{},
                                 remote_specs = #{},
                                 functions = #{},
                                 folding_ranges = [],
                                 zones = [],
                                 pois = Pois}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec do_parse(Forms :: [form_data()], ModuleInfo :: module_info()) -> module_info().
do_parse([], ModuleInfo) ->
    ModuleInfo;
do_parse([#form{data = {abstract_form, _}, zones = Zones} = Form | T],
         #module_info{zones = Zones0} = ModuleInfo0) ->
    ModuleInfo1 = ModuleInfo0#module_info{zones = Zones ++ Zones0},
    ModuleInfo2 = parse_abstract_form(Form, ModuleInfo1),
    do_parse(T, ModuleInfo2);
do_parse([#form{data = {tokens, Tokens}, zones = Zones} = Form | T],
         #module_info{zones = Zones0} = ModuleInfo0) ->
    ModuleInfo1 =
        case Tokens of
            [_|_] ->
                parse_tokens(Form, ModuleInfo0);
            [] ->
                ModuleInfo0#module_info{zones = Zones ++ Zones0}
        end,
    do_parse(T, ModuleInfo1).

-spec parse_abstract_form(Form :: form(), ModuleInfo :: module_info()) ->
          NewModuleInfo :: module_info().
parse_abstract_form(#form{data = {abstract_form, AbstractForm}} = Form, ModuleInfo) ->
    case erl_syntax:type(AbstractForm) of
        attribute ->
            parse_attribute(Form, ModuleInfo);
        function ->
            parse_function(Form, ModuleInfo);
        _ ->
            ModuleInfo
    end.

%%%-------------------------------------------------------------------
%%% Parsing attribute
%%%-------------------------------------------------------------------
-spec parse_attribute(Form :: form(), ModuleInfo :: module_info()) ->
          NewModuleInfo :: module_info().
parse_attribute(#form{data = {abstract_form, AbstractForm}},
                #module_info{behaviors = Behaviors0,
                             callback = Callback0,
                             types = Types0,
                             records = Records0,
                             local_specs = LocalSpecs0,
                             remote_specs = RemoteSpecs0,
                             pois = Pois0} = ModuleInfo0) ->
    case erl_syntax_lib:analyze_attribute(AbstractForm) of
        {module, {ModuleName, _}} ->
            Location = esrv_parser_lib:get_node_location(AbstractForm),
            ModuleInfo0#module_info{module_name = {ModuleName, Location}};
        {module, ModuleName} ->
            Location = esrv_parser_lib:get_node_location(AbstractForm),
            ModuleInfo0#module_info{module_name = {ModuleName, Location}};
        {Tag, {Tag, Behavior}}
          when Tag =:= behavior orelse Tag =:= behaviour ->
            Location = esrv_parser_lib:get_node_location(AbstractForm),
            ModuleInfo0#module_info{behaviors = Behaviors0#{Behavior => Location}};
        {compile, {compile, Options}} when is_list(Options) ->
            parse_compile_options(Options, ModuleInfo0);
        {compile, {compile, Option}} ->
            parse_compile_options([Option], ModuleInfo0);
        {Tag, {Tag, {Name, ValueNode, ArgumentNodes}}}
          when Tag =:= type orelse Tag =:= opaque ->
            NameArity = {Name, length(ArgumentNodes)},
            Location = esrv_parser_lib:get_node_location(AbstractForm),
            ModuleInfo1 = ModuleInfo0#module_info{types = Types0#{NameArity => Location}},
            parse_type_node(ValueNode, ModuleInfo1);
        {record, {RecordName, Fields}} ->
            RecordData =
                #record_data{location = esrv_parser_lib:get_node_location(AbstractForm),
                             record_fields = parse_record_fields(Fields, AbstractForm, #{})},
            Records1 = Records0#{RecordName => RecordData},
            Pois1 = parse_record_field_pois(RecordName, AbstractForm, Pois0),
            ModuleInfo1 = ModuleInfo0#module_info{records = Records1, pois = Pois1},
            ModuleInfo2 = parse_record_defaults(Fields, ModuleInfo1),
            parse_record_types(Fields, ModuleInfo2);
        {callback, {callback, {CallbackNameArity, CallbackTypeNodes}}} ->
            ModuleInfo1 = ModuleInfo0#module_info{callback = [CallbackNameArity | Callback0]},
            parse_type_nodes(CallbackTypeNodes, ModuleInfo1);
        {spec, {spec, {SpecKey, SpecTypeNodes}}} ->
            SpecData = #spec_data{location = esrv_parser_lib:get_node_location(AbstractForm),
                                  signatures = get_spec_signatures(SpecTypeNodes, [])},
            ModuleInfo1 =
                case SpecKey of
                    {ModuleName, SpecName, SpecArity} ->
                        RemoteSpecs1 =
                            maps:update_with(ModuleName,
                                             fun(MS) -> MS#{{SpecName, SpecArity} => SpecData} end,
                                             #{{SpecName, SpecArity} => SpecData}, RemoteSpecs0),
                        ModuleInfo0#module_info{remote_specs = RemoteSpecs1};
                    {SpecName, SpecArity} ->
                        LocalSpecs1 = LocalSpecs0#{{SpecName, SpecArity} => SpecData},
                        ModuleInfo0#module_info{local_specs = LocalSpecs1};
                    _ ->
                        ModuleInfo0
                end,
            parse_type_nodes(SpecTypeNodes, ModuleInfo1);
        _ ->
            ModuleInfo0
    end.

-spec parse_compile_options(Options :: [any()], ModuleInfo :: module_info()) ->
          NewModuleInfo :: module_info().
parse_compile_options(Options, ModuleInfo0) ->
    lists:foldl(fun({parse_transform, ParseTransform},
                    #module_info{parse_transform = ParseTransform00} = ModuleInfo00) ->
                        ParseTransform01 = [ParseTransform | ParseTransform00],
                        ModuleInfo00#module_info{parse_transform = ParseTransform01};
                   (export_all, ModuleInfo00) ->
                        ModuleInfo00#module_info{export_all = true};
                   (_, ModuleInfo00) ->
                        ModuleInfo00
                end, ModuleInfo0, Options).

-spec parse_type_nodes(TypeNodes :: [syntax_tree()],
                       ModuleInfo :: module_info()) -> NewModuleInfo :: module_info().
parse_type_nodes([], ModuleInfo) ->
    ModuleInfo;
parse_type_nodes([TypeNode | T], ModuleInfo0) ->
    ModuleInfo1 = parse_type_node(TypeNode, ModuleInfo0),
    parse_type_nodes(T, ModuleInfo1).

-spec parse_type_node(TypeNode :: syntax_tree(),
                      ModuleInfo :: module_info()) -> NewModuleInfo :: module_info().
parse_type_node(TypeNode, ModuleInfo0) ->
    case erl_syntax:type(TypeNode) of
        annotated_type ->
            BodyNode = erl_syntax:annotated_type_body(TypeNode),
            parse_type_node(BodyNode, ModuleInfo0);
        type_application ->
            NameNode = erl_syntax:type_application_name(TypeNode),
            ArgumentNodes = erl_syntax:type_application_arguments(TypeNode),
            ModuleInfo1 =
                case erl_syntax:type(NameNode) of
                    module_qualifier ->
                        ModuleNode = erl_syntax:module_qualifier_argument(NameNode),
                        FunctionNode = erl_syntax:module_qualifier_body(NameNode),
                        Arity = length(ArgumentNodes),
                        pois_remote_type(ModuleNode, FunctionNode, Arity, ModuleInfo0);
                    atom ->
                        Anno = erl_syntax:get_pos(TypeNode),
                        case erl_anno:text(Anno) of
                            "[" ->
                                ModuleInfo0;
                            _ ->
                                Arity = length(ArgumentNodes),
                                pois_local_type(NameNode, Arity, ModuleInfo0)
                        end;
                    _ ->
                        ModuleInfo0
                end,
            parse_type_nodes(ArgumentNodes, ModuleInfo1);
        user_type_application ->
            NameNode = erl_syntax:user_type_application_name(TypeNode),
            ArgumentNodes = erl_syntax:user_type_application_arguments(TypeNode),
            ModuleInfo1 =
                case erl_syntax:type(NameNode) of
                    atom ->
                        Arity = length(ArgumentNodes),
                        pois_local_type(NameNode, Arity, ModuleInfo0);
                    _ ->
                        ModuleInfo0
                end,
            parse_type_nodes(ArgumentNodes, ModuleInfo1);
        tuple_type ->
            case erl_syntax:tuple_type_elements(TypeNode) of
                ElementNodes when ElementNodes =/= any_size ->
                    parse_type_nodes(ElementNodes, ModuleInfo0);
                any_size ->
                    ModuleInfo0
            end;
        type_union ->
            ElementNodes = erl_syntax:type_union_types(TypeNode),
            parse_type_nodes(ElementNodes, ModuleInfo0);
        map_type ->
            case erl_syntax:map_type_fields(TypeNode) of
                FieldNodes when FieldNodes =/= any_size ->
                    parse_type_nodes(FieldNodes, ModuleInfo0);
                any_size ->
                    ModuleInfo0
            end;
        map_type_assoc ->
            NameNode = erl_syntax:map_type_assoc_name(TypeNode),
            ValueNode = erl_syntax:map_type_assoc_value(TypeNode),
            parse_type_nodes([NameNode, ValueNode], ModuleInfo0);
        map_type_exact ->
            NameNode = erl_syntax:map_type_exact_name(TypeNode),
            ValueNode = erl_syntax:map_type_exact_value(TypeNode),
            parse_type_nodes([NameNode, ValueNode], ModuleInfo0);
        record_type ->
            FieldNodes = erl_syntax:record_type_fields(TypeNode),
            parse_type_nodes(FieldNodes, ModuleInfo0);
        record_type_field ->
            FieldTypeNode = erl_syntax:record_type_field_type(TypeNode),
            parse_type_node(FieldTypeNode, ModuleInfo0);
        function_type ->
            ModuleInfo1 =
                case erl_syntax:function_type_arguments(TypeNode) of
                    ArgumentNodes when ArgumentNodes =/= any_arity ->
                        parse_type_nodes(ArgumentNodes, ModuleInfo0);
                    any_arity ->
                        ModuleInfo0
                end,
            ReturnNode = erl_syntax:function_type_return(TypeNode),
            parse_type_node(ReturnNode, ModuleInfo1);
        constrained_function_type ->
            parse_type_nodes([erl_syntax:constrained_function_type_body(TypeNode),
                              erl_syntax:constrained_function_type_argument(TypeNode)],
                             ModuleInfo0);
        conjunction ->
            BodyNodes = erl_syntax:conjunction_body(TypeNode),
            parse_type_nodes(BodyNodes, ModuleInfo0);
        constraint ->
            BodyNodes = erl_syntax:constraint_body(TypeNode),
            parse_type_nodes(BodyNodes, ModuleInfo0);
        _ ->
            ModuleInfo0
    end.

-spec parse_record_field_pois(RecordName :: name(),
                              AbstractFormm :: erl_parse:abstract_form(),
                              Pois :: pois()) -> pois().
parse_record_field_pois(RecordName, AbstractForm, Pois0) ->
    [_, TupleNode] = erl_syntax:attribute_arguments(AbstractForm),
    lists:foldl(fun(FieldNode, Pois00) ->
                        case get_record_field_name_node(FieldNode) of
                            {ok, FieldNameNode} ->
                                FieldName = erl_syntax:atom_value(FieldNameNode),
                                add_node_poi(FieldNameNode, {field, RecordName, FieldName}, Pois00);
                            undefined ->
                                Pois00
                        end
                end, Pois0, erl_syntax:tuple_elements(TupleNode)).

-spec parse_record_fields(Fields :: [{name(), any()}],
                          AbstractFormm :: erl_parse:abstract_form(),
                          Acc :: #{name() => location()}) ->
          RecordFields :: #{name() => location()}.
parse_record_fields([], _, Acc) ->
    Acc;
parse_record_fields([{FieldName, _} | T], AbstractForm, Acc) ->
    [_, TupleNode] = erl_syntax:attribute_arguments(AbstractForm),
    FieldNodes = erl_syntax:tuple_elements(TupleNode),
    case find_record_field_name_node(FieldName, FieldNodes) of
        {ok, FieldNameNode} ->
            Location = esrv_parser_lib:get_node_location(FieldNameNode),
            parse_record_fields(T, AbstractForm, Acc#{FieldName => Location});
        undefined ->
            parse_record_fields(T, AbstractForm, Acc)
    end.

-spec find_record_field_name_node(FieldName :: name(), FieldNodes :: [syntax_tree()]) ->
          {ok, syntax_tree()} | undefined.
find_record_field_name_node(_, []) ->
    undefined;
find_record_field_name_node(FieldName, [FieldNode | T]) ->
    case get_record_field_name_node(FieldNode) of
        {ok, FieldNameNode} ->
            case erl_syntax:atom_value(FieldNameNode) of
                FieldName ->
                    {ok, FieldNameNode};
                _ ->
                    find_record_field_name_node(FieldName, T)
            end;
        undefined ->
            find_record_field_name_node(FieldName, T)
    end.

-spec get_record_field_name_node(FieldNode :: syntax_tree()) -> {ok, syntax_tree()} | undefined.
get_record_field_name_node(FieldNode) ->
    case erl_syntax:type(FieldNode) of
        record_field ->
            {ok, erl_syntax:record_field_name(FieldNode)};
        typed_record_field ->
            ActualFieldNode = erl_syntax:typed_record_field_body(FieldNode),
            get_record_field_name_node(ActualFieldNode);
        _ ->
            undefined
    end.

-spec parse_record_defaults(Fields :: [{name(), any()}],
                            ModuleInfo :: module_info()) -> NewModuleInfo :: module_info().
parse_record_defaults([], ModuleInfo) ->
    ModuleInfo;
parse_record_defaults([{_, {none, _}} | T], ModuleInfo) ->
    parse_record_defaults(T, ModuleInfo);
parse_record_defaults([{_, {FieldDefaultNode, _}} | T], ModuleInfo0) ->
    {_, ModuleInfo1, _} =
        parse_node(FieldDefaultNode, {#clause{variables = []}, ModuleInfo0, #{}}, []),
    parse_record_defaults(T, ModuleInfo1).

-spec parse_record_types(Fields :: [{name(), any()}],
                         ModuleInfo :: module_info()) -> NewModuleInfo :: module_info().
parse_record_types([], ModuleInfo) ->
    ModuleInfo;
parse_record_types([{_, {_, none}} | T], ModuleInfo) ->
    parse_record_types(T, ModuleInfo);
parse_record_types([{_, {_, FieldTypeNode}} | T], ModuleInfo0) ->
    ModuleInfo1 = parse_type_node(FieldTypeNode, ModuleInfo0),
    parse_record_types(T, ModuleInfo1).

-spec get_spec_signatures(SpecTypeNodes :: [syntax_tree()], Acc :: [signature()]) -> [signature()].
get_spec_signatures([], Acc) ->
    lists:reverse(Acc);
get_spec_signatures([SpecTypeNode | T], Acc) ->
    case erl_syntax:type(SpecTypeNode) of
        function_type ->
            ArgumentNodes = erl_syntax:function_type_arguments(SpecTypeNode),
            get_spec_signatures(T, [get_spec_signature(ArgumentNodes, []) | Acc]);
        constrained_function_type ->
            BodyNode = erl_syntax:constrained_function_type_body(SpecTypeNode),
            get_spec_signatures([BodyNode | T], Acc);
        _ ->
            get_spec_signatures(T, Acc)
    end.

-spec get_spec_signature(ArgumentNodes :: [syntax_tree()], Acc :: signature()) -> signature().
get_spec_signature([], Acc) ->
    lists:reverse(Acc);
get_spec_signature([ArgumentNode | T], Acc) ->
    case erl_syntax:type(ArgumentNode) of
        variable ->
            Name = erl_syntax:variable_name(ArgumentNode),
            get_spec_signature(T, [Name | Acc]);
        annotated_type ->
            NameNode = erl_syntax:annotated_type_name(ArgumentNode),
            get_spec_signature([NameNode | T], Acc);
        _ ->
            get_spec_signature(T, [none | Acc])
    end.

%%%-------------------------------------------------------------------
%%% Parsing function
%%%-------------------------------------------------------------------
-type context() :: #{name() => location()}.

-spec parse_function(Form :: form(), ModuleInfo :: module_info()) ->
          NewModuleInfo :: module_info().
parse_function(#form{start_location = StartLocation,
                     end_location = EndLocation,
                     data = {abstract_form, AbstractForm}},
               #module_info{functions = Functions,
                            pois = Pois0} = ModuleInfo0) ->
    NameArity = erl_syntax_lib:analyze_function(AbstractForm),
    if
        not is_map_key(NameArity, Functions) ->
            Pois1 = add_node_poi(AbstractForm, {local_function, NameArity}, Pois0),
            ModuleInfo1 = ModuleInfo0#module_info{pois = Pois1},
            ModuleInfo2 = add_function_zone(NameArity, StartLocation, EndLocation, ModuleInfo1),
            ClauseNodes = erl_syntax:function_clauses(AbstractForm),
            ModuleInfo3 = pois_function_clauses(ClauseNodes, NameArity, ModuleInfo2),
            ModuleInfo4 = add_function_data(NameArity, StartLocation, ClauseNodes, ModuleInfo3),
            add_function_folding_ranges(ClauseNodes, EndLocation, ModuleInfo4);
        true ->
            ModuleInfo0
    end.

-spec add_function_zone(NameArity :: name_arity(),
                        StartLocation :: location(),
                        EndLocation :: location(),
                        ModuleInfo :: module_info()) -> NewModuleInfo :: module_info().
add_function_zone(NameArity, StartLocation, EndLocation,
                  #module_info{zones = Zones0} = ModuleInfo0) ->
    Zone = #zone{type = {function, NameArity},
                 start_location = StartLocation,
                 end_location = EndLocation},
    ModuleInfo0#module_info{zones = [Zone | Zones0]}.

-spec add_function_data(NameArity :: name_arity(),
                        Location :: location(),
                        ClauseNodes :: [syntax_tree()],
                        ModuleInfo :: module_info()) -> NewModuleInfo :: module_info().
add_function_data(NameArity, Location, ClauseNodes, ModuleInfo0) ->
    {Clauses, #module_info{functions = Functions1} = ModuleInfo1} =
        lists:foldl(fun(ClauseNode, {Acc, ModuleInfo00}) ->
                            {Clause, ModuleInfo01} = parse_clause(ClauseNode,
                                                                  {#clause{variables = []},
                                                                   ModuleInfo00},
                                                                  [], []),
                            {[normalize_clause(Clause) | Acc], ModuleInfo01}
                    end, {[], ModuleInfo0}, ClauseNodes),
    Signatures = get_function_signatures(ClauseNodes, []),
    FunctionData = #function_data{location = Location,
                                  clauses = Clauses,
                                  signatures = Signatures},
    ModuleInfo1#module_info{functions = Functions1#{NameArity => FunctionData}}.

-spec normalize_clause(Clause :: #clause{}) -> NewClause :: #clause{}.
normalize_clause(#clause{variables = Variables} = Clause) ->
    Clause#clause{variables = lists:usort(Variables)}.

-spec get_function_signatures(ClauseNodes :: [syntax_tree()], Acc :: [signature()]) ->
          [signature()].
get_function_signatures([], Acc) ->
    lists:reverse(Acc);
get_function_signatures([ClauseNode | T], Acc) ->
    PatternNodes = erl_syntax:clause_patterns(ClauseNode),
    get_function_signatures(T, [get_function_signature(PatternNodes, []) | Acc]).

-spec get_function_signature(PatternNodes :: [syntax_tree()], Acc :: signature()) -> signature().
get_function_signature([], Acc) ->
    lists:reverse(Acc);
get_function_signature([PatternNode | T], Acc) ->
    case erl_syntax:type(PatternNode) of
        variable ->
            case erl_syntax:variable_name(PatternNode) of
                '_' ->
                    get_function_signature(T, [none | Acc]);
                Name ->
                    get_function_signature(T, [Name | Acc])
            end;
        match_expr ->
            MatchExprPatternNode = erl_syntax:match_expr_pattern(PatternNode),
            get_function_signature([MatchExprPatternNode | T], Acc);
        _ ->
            get_function_signature(T, [none | Acc])
    end.

-spec add_function_folding_ranges(ClauseNodes :: [syntax_tree()],
                                  TerminalLocation :: location(),
                                  ModuleInfo :: module_info()) -> NewModuleInfo :: module_info().
add_function_folding_ranges([], _, ModuleInfo) ->
    ModuleInfo;
add_function_folding_ranges([ClauseNode | T], TerminalLocation,
                            #module_info{folding_ranges = FoldingRanges0} = ModuleInfo0) ->
    StartLocation = esrv_parser_lib:get_node_location(ClauseNode),
    StartLine = esrv_parser_lib:location_to_line(StartLocation),
    EndLine =
        case T of
            [NextClauseNode | _] ->
                EndLocation = esrv_parser_lib:get_node_location(NextClauseNode),
                esrv_parser_lib:location_to_line(EndLocation) - 1;
            [] ->
                esrv_parser_lib:location_to_line(TerminalLocation)
        end,
    ModuleInfo1 =
        if
            EndLine > StartLine ->
                ModuleInfo0#module_info{folding_ranges = [{StartLine, EndLine} | FoldingRanges0]};
            true ->
                ModuleInfo0
        end,
    add_function_folding_ranges(T, TerminalLocation, ModuleInfo1).

-type cmi() :: {clause(), module_info()}.
-type cmic() :: {clause(), module_info(), context()}.

-spec parse_clauses(ClauseNodes :: [syntax_tree()],
                    CMI :: cmi(),
                    PContextsA :: [context()],
                    PContextsB :: [context()]) -> cmi().
parse_clauses([], CMI, _, _) ->
    CMI;
parse_clauses([ClauseNode | T], CMI0, PContextsA, PContextsB) ->
    CMI1 = parse_clause(ClauseNode, CMI0, PContextsA, PContextsB),
    parse_clauses(T, CMI1, PContextsA, PContextsB).

-spec parse_clause(ClauseNode :: syntax_tree(),
                   CMI :: cmi(),
                   PContextsA :: [context()],
                   PContextsB :: [context()]) -> cmi().
parse_clause(ClauseNode, {Clause0, ModuleInfo0}, PContextsA, PContextsB) ->
    PatternNodes = erl_syntax:clause_patterns(ClauseNode),
    CMIC1 = parse_nodes(PatternNodes, {Clause0, ModuleInfo0, #{}}, PContextsA, true),
    CMIC2 =
        case erl_syntax:clause_guard(ClauseNode) of
            GuardNode when GuardNode =/= none ->
                parse_node(GuardNode, CMIC1, PContextsB);
            none ->
                CMIC1
        end,
    BodyNodes = erl_syntax:clause_body(ClauseNode),
    {Clause3, ModuleInfo3, _} = parse_nodes(BodyNodes, CMIC2, PContextsB),
    {Clause3, ModuleInfo3}.

-spec parse_nodes(Nodes :: [syntax_tree()],
                  CMIC :: cmic(),
                  PContexts :: [context()]) -> cmic().
parse_nodes(Nodes, CMIC, PContexts) ->
    parse_nodes(Nodes, CMIC, PContexts, false).

-spec parse_nodes(Nodes :: [syntax_tree()],
                  CMIC :: cmic(),
                  PContexts :: [context()],
                  IsPattern :: boolean()) -> cmic().
parse_nodes([], CMIC, _, _) ->
    CMIC;
parse_nodes([Node | T], CMIC0, PContexts, IsPattern) ->
    CMIC1 = parse_node(Node, CMIC0, PContexts, IsPattern),
    parse_nodes(T, CMIC1, PContexts, IsPattern).

-spec parse_node(Node :: syntax_tree(),
                 CMIC :: cmic(),
                 PContexts :: [context()]) -> cmic().
parse_node(Node, CMIC, PContexts) ->
    parse_node(Node, CMIC, PContexts, false).

-spec parse_node(Node :: syntax_tree(),
                 CMIC :: cmic(),
                 PContexts :: [context()],
                 IsPattern :: boolean()) -> cmic().
parse_node(?UNRESOLVED_MACRO_TOKEN(MarcroArgsTokens), CMIC, PContexts, IsPattern) ->
    parse_node_umt(MarcroArgsTokens, CMIC, PContexts, IsPattern);
parse_node(Node, {Clause0, ModuleInfo0, Context0} = CMIC0, PContexts, IsPattern) ->
    case erl_syntax:type(Node) of
        variable ->
            VariableName = erl_syntax:variable_name(Node),
            VariableLocation = esrv_parser_lib:get_node_location(Node),
            case find_variable_definition(VariableName, [Context0 | PContexts]) of
                {ok, Definition} ->
                    ModuleInfo1 = pois_variable(Node, {local, Definition}, ModuleInfo0),
                    {Clause0, ModuleInfo1, Context0};
                undefined when IsPattern ->
                    #clause{variables = Variables0} = Clause0,
                    Clause1 = Clause0#clause{variables = [VariableName | Variables0]},
                    ModuleInfo1 = pois_variable(Node, {local, VariableLocation}, ModuleInfo0),
                    Context1 = Context0#{VariableName => VariableLocation},
                    {Clause1, ModuleInfo1, Context1};
                undefined ->
                    {Clause0, ModuleInfo0, Context0}
            end;
        tuple ->
            parse_nodes(erl_syntax:tuple_elements(Node), CMIC0, PContexts, IsPattern);
        list ->
            CMIC1 = parse_node(erl_syntax:list_head(Node), CMIC0, PContexts, IsPattern),
            parse_node(erl_syntax:list_tail(Node), CMIC1, PContexts, IsPattern);
        binary ->
            parse_nodes(erl_syntax:binary_fields(Node), CMIC0, PContexts, IsPattern);
        binary_field ->
            parse_node(erl_syntax:binary_field_body(Node), CMIC0, PContexts, IsPattern);
        size_qualifier ->
            parse_node(erl_syntax:size_qualifier_body(Node), CMIC0, PContexts, IsPattern);
        record_expr ->
            CMIC1 =
                case erl_syntax:record_expr_argument(Node) of
                    ArgumentNode when ArgumentNode =/= none ->
                        parse_node(ArgumentNode, CMIC0, PContexts);
                    none ->
                        CMIC0
                end,
            RecordNameNode = erl_syntax:record_expr_type(Node),
            lists:foldl(fun(FieldNode, {Clause10, ModuleInfo10, Context10}) ->
                                FieldNameNode = erl_syntax:record_field_name(FieldNode),
                                FieldValueNode = erl_syntax:record_field_value(FieldNode),
                                parse_node(FieldValueNode,
                                           {Clause10,
                                            pois_field_name(RecordNameNode,
                                                            FieldNameNode,
                                                            ModuleInfo10),
                                            Context10},
                                           PContexts,
                                           IsPattern)
                        end, CMIC1, erl_syntax:record_expr_fields(Node));
        record_access ->
            {Clause1, ModuleInfo1, Context1} =
                parse_node(erl_syntax:record_access_argument(Node), CMIC0, PContexts),
            RecordNameNode = erl_syntax:record_access_type(Node),
            FieldNameNode = erl_syntax:record_access_field(Node),
            ModuleInfo2 = pois_field_name(RecordNameNode, FieldNameNode, ModuleInfo1),
            {Clause1, ModuleInfo2, Context1};
        record_index_expr ->
            RecordNameNode = erl_syntax:record_index_expr_type(Node),
            FieldNameNode = erl_syntax:record_index_expr_field(Node),
            ModuleInfo1 = pois_field_name(RecordNameNode, FieldNameNode, ModuleInfo0),
            {Clause0, ModuleInfo1, Context0};
        map_expr ->
            CMIC1 =
                case erl_syntax:map_expr_argument(Node) of
                    ArgumentNode when ArgumentNode =/= none ->
                        parse_node(ArgumentNode, CMIC0, PContexts);
                    none ->
                        CMIC0
                end,
            parse_nodes(erl_syntax:map_expr_fields(Node), CMIC1, PContexts, IsPattern);
        map_field_assoc ->
            parse_node(erl_syntax:map_field_assoc_value(Node), CMIC0, PContexts);
        map_field_exact ->
            parse_node(erl_syntax:map_field_exact_value(Node), CMIC0, PContexts, IsPattern);
        infix_expr ->
            parse_nodes([erl_syntax:infix_expr_left(Node),
                         erl_syntax:infix_expr_right(Node)],
                        CMIC0, PContexts);
        prefix_expr ->
            parse_node(erl_syntax:prefix_expr_argument(Node), CMIC0, PContexts);
        list_comp ->
            ModuleInfo1 = parse_list_comp(Node, {Clause0, ModuleInfo0}, [Context0 | PContexts]),
            {Clause0, ModuleInfo1, Context0};
        binary_comp ->
            ModuleInfo1 = parse_binary_comp(Node, {Clause0, ModuleInfo0}, [Context0 | PContexts]),
            {Clause0, ModuleInfo1, Context0};
        block_expr ->
            parse_nodes(erl_syntax:block_expr_body(Node), CMIC0, PContexts);
        application ->
            OperatorNode = erl_syntax:application_operator(Node),
            ArgumentNodes = erl_syntax:application_arguments(Node),
            ModuleInfo1 =
                case erl_syntax:type(OperatorNode) of
                    module_qualifier ->
                        ModuleNode = erl_syntax:module_qualifier_argument(OperatorNode),
                        FunctionNode = erl_syntax:module_qualifier_body(OperatorNode),
                        Arity = length(ArgumentNodes),
                        pois_remote_function(ModuleNode, FunctionNode, Arity, ModuleInfo0);
                    atom ->
                        Arity = length(ArgumentNodes),
                        pois_local_function(OperatorNode, Arity, ModuleInfo0);
                    _ ->
                        ModuleInfo0
                end,
            parse_nodes(ArgumentNodes, {Clause0, ModuleInfo1, Context0}, PContexts);
        match_expr ->
            CMIC1 = parse_node(erl_syntax:match_expr_pattern(Node), CMIC0, PContexts, true),
            parse_node(erl_syntax:match_expr_body(Node), CMIC1, PContexts, IsPattern);
        case_expr ->
            {Clause1, ModuleInfo1, Context1} =
                parse_node(erl_syntax:case_expr_argument(Node), CMIC0, PContexts),
            {Clause2, ModuleInfo2} = parse_clauses(erl_syntax:case_expr_clauses(Node),
                                                   {Clause1, ModuleInfo1},
                                                   [Context1 | PContexts],
                                                   [Context1 | PContexts]),
            {Clause2, ModuleInfo2, Context0};
        if_expr ->
            {Clause1, ModuleInfo1} = parse_clauses(erl_syntax:if_expr_clauses(Node),
                                                   {Clause0, ModuleInfo0},
                                                   [Context0 | PContexts],
                                                   [Context0 | PContexts]),
            {Clause1, ModuleInfo1, Context0};
        receive_expr ->
            {Clause1, ModuleInfo1} = parse_clauses(erl_syntax:receive_expr_clauses(Node),
                                                   {Clause0, ModuleInfo0},
                                                   [Context0 | PContexts],
                                                   [Context0 | PContexts]),
            {Clause2, ModuleInfo2, _} =
                case erl_syntax:receive_expr_timeout(Node) of
                    TimeoutNode when TimeoutNode =/= none ->
                        parse_node(TimeoutNode, {Clause1, ModuleInfo1, Context0}, PContexts);
                    none ->
                        {Clause1, ModuleInfo1, Context0}
                end,
            {Clause3, ModuleInfo3, _} = parse_nodes(erl_syntax:receive_expr_action(Node),
                                                    {Clause2, ModuleInfo2, Context0}, PContexts),
            {Clause3, ModuleInfo3, Context0};
        catch_expr ->
            {Clause1, ModuleInfo1, _} =
                parse_node(erl_syntax:catch_expr_body(Node), CMIC0, PContexts),
            {Clause1, ModuleInfo1, Context0};
        try_expr ->
            {Clause1, ModuleInfo1, _} =
                parse_nodes(erl_syntax:try_expr_body(Node), CMIC0, PContexts),
            {Clause2, ModuleInfo2} = parse_clauses(erl_syntax:try_expr_clauses(Node),
                                                   {Clause1, ModuleInfo1},
                                                   [Context0 | PContexts],
                                                   [Context0 | PContexts]),
            {Clause3, ModuleInfo3} = parse_clauses(erl_syntax:try_expr_handlers(Node),
                                                   {Clause2, ModuleInfo2},
                                                   [Context0 | PContexts],
                                                   [Context0 | PContexts]),
            {Clause4, ModuleInfo4, _} = parse_nodes(erl_syntax:try_expr_after(Node),
                                                    {Clause3, ModuleInfo3, Context0}, PContexts),
            {Clause4, ModuleInfo4, Context0};
        class_qualifier ->
            CMIC1 = parse_node(erl_syntax:class_qualifier_argument(Node), CMIC0, PContexts, true),
            CMIC2 = parse_node(erl_syntax:class_qualifier_body(Node), CMIC1, PContexts, true),
            parse_node(erl_syntax:class_qualifier_stacktrace(Node), CMIC2, PContexts, true);
        fun_expr ->
            {Clause1, ModuleInfo1} = parse_clauses(erl_syntax:fun_expr_clauses(Node),
                                                   {Clause0, ModuleInfo0},
                                                   [],
                                                   [Context0 | PContexts]),
            {Clause1, ModuleInfo1, Context0};
        named_fun_expr ->
            {Clause1, ModuleInfo1, Context1} = parse_node(erl_syntax:named_fun_expr_name(Node),
                                                          {Clause0, ModuleInfo0, #{}}, [], true),
            {Clause2, ModuleInfo2} = parse_clauses(erl_syntax:named_fun_expr_clauses(Node),
                                                   {Clause1, ModuleInfo1},
                                                   [],
                                                   [Context1 | PContexts]),
            {Clause2, ModuleInfo2, Context0};
        disjunction ->
            DisjunctionNodes = erl_syntax:disjunction_body(Node),
            parse_nodes(DisjunctionNodes, CMIC0, PContexts);
        conjunction ->
            ConjunctionNodes = erl_syntax:disjunction_body(Node),
            parse_nodes(ConjunctionNodes, CMIC0, PContexts);
        _ ->
            CMIC0
    end.

-spec parse_node_umt(MarcroArgsTokens :: [marcro_arg_tokens()],
                     CMIC :: cmic(),
                     PContexts :: [context()],
                     IsPattern :: boolean()) -> cmic().
parse_node_umt(MarcroArgsTokens, CMIC0, PContexts, IsPattern) ->
    ParsedExprs =
        lists:map(fun(MarcroArgTokens) ->
                          case erl_parse:parse_exprs(MarcroArgTokens ++ [{dot, {1, 1}}]) of
                              {ok, MarcroArgNodes} ->
                                  {nodes, MarcroArgNodes};
                              _ ->
                                  {tokens, MarcroArgTokens}
                          end
                  end, MarcroArgsTokens),
    lists:foldl(fun({nodes, Nodes}, CMIC00) ->
                        parse_nodes(Nodes, CMIC00, PContexts, IsPattern);
                   ({tokens, Tokens}, {Clause00, ModuleInfo00, Context00}) ->
                        #module_info{pois = Pois00} = ModuleInfo00,
                        {_, Pois01, Variables} = process_function_umt(Tokens, [], Pois00, []),
                        {lists:foldl(fun variable_into_clause/2, Clause00, Variables),
                         ModuleInfo00#module_info{pois = Pois01},
                         Context00}
                end, CMIC0, ParsedExprs).

-spec parse_list_comp(ListCompNode :: syntax_tree(),
                      CMI :: cmi(),
                      PContexts :: [context()]) -> module_info().
parse_list_comp(ListCompNode, {Clause0, ModuleInfo0}, PContexts) ->
    CMIC1 =
        lists:foldl(fun(BodyNode, CMIC00) ->
                            case erl_syntax:type(BodyNode) of
                                generator ->
                                    parse_generator(BodyNode, CMIC00, PContexts);
                                binary_generator ->
                                    parse_binary_generator(BodyNode, CMIC00, PContexts);
                                _ ->
                                    parse_node(BodyNode, CMIC00, PContexts)
                            end
                    end, {Clause0, ModuleInfo0, #{}}, erl_syntax:list_comp_body(ListCompNode)),
    TemplateNode = erl_syntax:list_comp_template(ListCompNode),
    {_, ModuleInfo2, _} = parse_node(TemplateNode, CMIC1, PContexts),
    ModuleInfo2.

-spec parse_binary_comp(BinaryCompNode :: syntax_tree(),
                        CMI :: cmi(),
                        PContexts :: [context()]) -> module_info().
parse_binary_comp(BinaryCompNode, {Clause0, ModuleInfo0}, PContexts) ->
    CMIC1 =
        lists:foldl(fun(BodyNode, CMIC00) ->
                            case erl_syntax:type(BodyNode) of
                                generator ->
                                    parse_generator(BodyNode, CMIC00, PContexts);
                                binary_generator ->
                                    parse_binary_generator(BodyNode, CMIC00, PContexts);
                                _ ->
                                    parse_node(BodyNode, CMIC00, PContexts)
                            end
                    end, {Clause0, ModuleInfo0, #{}}, erl_syntax:binary_comp_body(BinaryCompNode)),
    TemplateNode = erl_syntax:binary_comp_template(BinaryCompNode),
    {_, ModuleInfo2, _} = parse_node(TemplateNode, CMIC1, PContexts),
    ModuleInfo2.

-spec parse_generator(GeneratorNode :: syntax_tree(),
                      CMIC :: cmic(),
                      PContexts :: [context()]) -> cmic().
parse_generator(GeneratorNode, CMIC0, PContexts) ->
    BodyNode = erl_syntax:generator_body(GeneratorNode),
    CMIC1 = parse_node(BodyNode, CMIC0, PContexts),
    PatternNode = erl_syntax:generator_pattern(GeneratorNode),
    parse_node(PatternNode, CMIC1, [], true).

-spec parse_binary_generator(BinaryGeneratorNode :: syntax_tree(),
                             CMIC :: cmic(),
                             PContexts :: [context()]) -> cmic().
parse_binary_generator(BinaryGeneratorNode, CMIC0, PContexts) ->
    BodyNode = erl_syntax:binary_generator_body(BinaryGeneratorNode),
    CMIC1 = parse_node(BodyNode, CMIC0, PContexts),
    PatternNode = erl_syntax:binary_generator_pattern(BinaryGeneratorNode),
    parse_node(PatternNode, CMIC1, PContexts, true).

-spec find_variable_definition(Name :: name(), Contexts :: [context()]) ->
          {ok, location()} | undefined.
find_variable_definition(_, []) ->
    undefined;
find_variable_definition(Name, [Context | _]) when is_map_key(Name, Context) ->
    {ok, maps:get(Name, Context)};
find_variable_definition(Name, [_ | T]) ->
    find_variable_definition(Name, T).

%%%-------------------------------------------------------------------
%%% Parsing tokens
%%%-------------------------------------------------------------------
-record(current_zone, {opened_zone :: opened_zone(),
                       function_data :: function_data() | undefined,
                       variable_tokens :: [token()] | undefined}).

-record(pt_state, {tokens_in :: [token()],
                   tokens_out :: [token()],
                   export :: [name_arity()],
                   export_type :: [name_arity()],
                   import :: [import()],
                   functions :: #{name_arity() => function_data()},
                   current_zone :: #current_zone{} | undefined,
                   zones :: [zone()],
                   pois :: pois(),
                   form :: form() | undefined}).

-spec parse_tokens(Form :: form(), ModuleInfo :: module_info()) -> module_info().
parse_tokens(#form{data = {tokens, Tokens}}, ModuleInfo0) ->
    State0 = init_state(Tokens, ModuleInfo0),
    State1 = process_tokens(false, State0),
    ModuleInfo1 = finalize_state(State1, ModuleInfo0),
    case State1 of
        #pt_state{form = Form} when Form =/= undefined ->
            parse_abstract_form(Form, ModuleInfo1);
        #pt_state{form = undefined} ->
            ModuleInfo1
    end.

-spec init_state(Tokens :: [token()], ModuleInfo :: module_info()) -> #pt_state{}.
init_state(Tokens, #module_info{export = Export,
                                export_type = ExportType,
                                import = Import,
                                functions = Functions,
                                zones = Zones,
                                pois = Pois}) ->
    #pt_state{tokens_in = Tokens,
              tokens_out = [],
              export = Export,
              export_type = ExportType,
              import = Import,
              functions = Functions,
              zones = Zones,
              pois = Pois}.

-spec finalize_state(State :: #pt_state{}, ModuleInfo :: module_info()) -> module_info().
finalize_state(#pt_state{export = Export,
                         export_type = ExportType,
                         import = Import,
                         functions = Functions,
                         zones = Zones,
                         pois = Pois}, ModuleInfo0) ->
    ModuleInfo0#module_info{export = Export,
                            export_type = ExportType,
                            import = Import,
                            functions = Functions,
                            zones = Zones,
                            pois = Pois}.

-spec process_tokens(CheckForm :: boolean(), State :: #pt_state{}) -> #pt_state{}.
process_tokens(_, #pt_state{tokens_in = []} = State) ->
    close_zone(State);
process_tokens(CheckForm, State0) ->
    State1 = process_zone(CheckForm, State0),
    State2 = process_token(State1),
    process_tokens(true, State2).

-spec process_zone(CheckForm :: boolean(), State :: #pt_state{}) -> #pt_state{}.
process_zone(CheckForm, #pt_state{tokens_in = TokensIn, current_zone = CurrentZone0} = State0) ->
    case check_zone(TokensIn, CurrentZone0) of
        {new, CurrentZone, Zones, StartToken, TokensLeft} ->
            State1 = close_zone(State0),
            State2 = open_zone(CheckForm, CurrentZone, Zones, StartToken, TokensLeft, State1),
            State3 = add_function_poi(TokensIn, CurrentZone, State2),
            process_zone(CheckForm, State3);
        {update, CurrentZone1, TokensLeft} ->
            State1 = State0#pt_state{tokens_in = TokensLeft, current_zone = CurrentZone1},
            State2 = add_function_clause_poi(TokensIn, CurrentZone1, State1),
            process_zone(CheckForm, State2);
        undefined ->
            State0
    end.

-spec open_zone(CheckForm :: boolean(),
                CurrentZone :: #current_zone{},
                Zones :: [zone()],
                StartToken :: token(),
                TokensLeft :: [token()],
                State :: #pt_state{}) -> #pt_state{}.
open_zone(true, CurrentZone, Zones, StartToken, TokensLeft,
          #pt_state{tokens_in = TokensIn0,
                    export = Export0,
                    export_type = ExportType0,
                    import = Import0,
                    zones = Zones0,
                    pois = Pois0} = State0) ->
    case try_parse(TokensIn0) of
        {export, function, Parsed} ->
            #current_zone{opened_zone = OpenedZone} = CurrentZone,
            Reversed = lists:reverse(TokensIn0),
            Zones1 = [esrv_zone_parser:zone_end(OpenedZone, Reversed) | Zones0],
            {Export1, Pois1} =
                lists:foldl(fun({NameArity, Token}, {Export00, Pois00}) ->
                                    {[NameArity | Export00],
                                     esrv_parser_lib:add_token_poi(Token,
                                                                   {local_function,
                                                                    NameArity},
                                                                   undefined,
                                                                   Pois00)}
                            end, {Export0, Pois0}, Parsed),
            State0#pt_state{tokens_in = [], export = Export1,
                            zones = Zones1, pois = Pois1};
        {export, type, Parsed} ->
            #current_zone{opened_zone = OpenedZone} = CurrentZone,
            Reversed = lists:reverse(TokensIn0),
            Zones1 = [esrv_zone_parser:zone_end(OpenedZone, Reversed) | Zones0],
            {ExportType1, Pois1} =
                lists:foldl(fun({NameArity, Token}, {ExportType00, Pois00}) ->
                                    {[NameArity | ExportType00],
                                     esrv_parser_lib:add_token_poi(Token,
                                                                   {local_type,
                                                                    NameArity},
                                                                   undefined,
                                                                   Pois00)}
                            end, {ExportType0, Pois0}, Parsed),
            State0#pt_state{tokens_in = [], export_type = ExportType1,
                            zones = Zones1, pois = Pois1};
        {import, {atom, _, ModuleName} = Token} ->
            #current_zone{opened_zone = OpenedZone} = CurrentZone,
            Reversed = lists:reverse(TokensIn0),
            Zones1 = [esrv_zone_parser:zone_end(OpenedZone, Reversed) | Zones0],
            Pois1 = esrv_parser_lib:add_token_poi(Token, {module, ModuleName}, undefined, Pois0),
            State0#pt_state{tokens_in = [], import = [ModuleName | Import0],
                            zones = Zones1, pois = Pois1};
        {import, {atom, _, ModuleName} = Token, Parsed} ->
            #current_zone{opened_zone = OpenedZone} = CurrentZone,
            Reversed = lists:reverse(TokensIn0),
            Zones1 = [esrv_zone_parser:zone_end(OpenedZone, Reversed) | Zones0],
            {Import1, Pois1} =
                lists:foldl(fun({NameArity, FunctionToken}, {Import00, Pois00}) ->
                                    {[{ModuleName, NameArity} | Import00],
                                     esrv_parser_lib:add_token_poi(FunctionToken,
                                                                   {remote_function,
                                                                    ModuleName,
                                                                    NameArity},
                                                                   undefined,
                                                                   Pois00)}
                            end, {Import0, Pois0}, Parsed),
            Pois2 = esrv_parser_lib:add_token_poi(Token, {module, ModuleName}, undefined, Pois1),
            State0#pt_state{tokens_in = [], import = Import1,
                            zones = Zones1, pois = Pois2};
        {form, Form} ->
            Zones1 =
                case CurrentZone of
                    #current_zone{opened_zone = #opened_zone{type = {function, _}}} ->
                        Zones0;
                    #current_zone{opened_zone = OpenedZone} ->
                        Reversed = lists:reverse(TokensIn0),
                        [esrv_zone_parser:zone_end(OpenedZone, Reversed) | Zones ++ Zones0]
                end,
            State0#pt_state{tokens_in = [], current_zone = undefined, zones = Zones1, form = Form};
        undefined ->
            open_zone(false, CurrentZone, Zones, StartToken, TokensLeft, State0)
    end;
open_zone(false, CurrentZone, Zones, StartToken, TokensLeft,
          #pt_state{tokens_out = TokensOut0, zones = Zones0} = State0) ->
    State0#pt_state{tokens_in = TokensLeft,
                    tokens_out = [StartToken | TokensOut0],
                    current_zone = CurrentZone,
                    zones = Zones ++ Zones0}.

-spec try_parse(Tokens :: [token()]) ->
          {export, export_kind(), [parsed_name_arity()]} |
          {import, token()} |
          {import, token(), [parsed_name_arity()]} |
          {form, form()} |
          undefined.
try_parse([{'-', _}, {atom, _, export}, {'(', _}, {'[', _} | T]) ->
    try_parse_export(function, T);
try_parse([{'-', _}, {atom, _, export_type}, {'(', _}, {'[', _} | T]) ->
    try_parse_export(type, T);
try_parse([{'-', _}, {atom, _, import}, {'(', _} | T]) ->
    try_parse_import(T);
try_parse(Tokens) ->
    try_parse_form(Tokens).

-spec try_parse_export(ExportKind :: export_kind(), Tokens :: [token()]) ->
          {export, export_kind(), [parsed_name_arity()]} |
          undefined.
try_parse_export(ExportKind, Tokens) ->
    case esrv_parser_lib:parse_name_arity_tokens(Tokens) of
        {ok, Parsed} ->
            {export, ExportKind, Parsed};
        undefined ->
            undefined
    end.

-spec try_parse_import(Tokens :: [token()]) ->
          {import, token()} |
          {import, token(), [parsed_name_arity()]} |
          undefined.
try_parse_import([{atom, _, _} = Token, {')', _}, {dot, _}]) ->
    {import, Token};
try_parse_import([{atom, _, _} = Token, {',', _}, {'[', _} | T]) ->
    case esrv_parser_lib:parse_name_arity_tokens(T) of
        {ok, Parsed} ->
            {import, Token, Parsed};
        undefined ->
            undefined
    end;
try_parse_import(_) ->
    undefined.

-spec try_parse_form(Tokens :: [token()]) ->
          {form, form()} |
          undefined.
try_parse_form(Tokens) ->
    case erl_parse:parse_form(Tokens) of
        {ok, AbstractForm} ->
            Form = #form{start_location = esrv_parser_lib:first_token_location(Tokens),
                         end_location = esrv_parser_lib:last_token_location(Tokens),
                         data = {abstract_form, AbstractForm},
                         zones = []},
            {form, Form};
        _ ->
            undefined
    end.

-spec check_zone(Tokens :: [token()], CurrentZone :: #current_zone{} | undefined) ->
          {new, #current_zone{}, [zone()], token(), [token()]} |
          {update, #current_zone{}, [token()]} |
          undefined.
check_zone([], _) ->
    undefined;
check_zone(Tokens, CurrentZone0) ->
    case esrv_zone_parser:zone_begin(Tokens) of
        {ok, OpenedZone, Zones, StartToken, TokensLeft} ->
            CurrentZone = #current_zone{opened_zone = OpenedZone},
            {new, CurrentZone, Zones, StartToken, TokensLeft};
        undefined ->
            case function_zone_begin(Tokens) of
                {ok, OpenedZone, FunctionData, VariableTokens, StartToken, TokensLeft} ->
                    case {OpenedZone, CurrentZone0} of
                        {#opened_zone{type = {function, NameArity}},
                         #current_zone{opened_zone = #opened_zone{type = {function, NameArity}},
                                       function_data = FunctionData0,
                                       variable_tokens = VariableTokens0}} ->
                            FunctionData1 = merge_function_data(FunctionData, FunctionData0),
                            VariableTokens1 = VariableTokens ++ VariableTokens0,
                            CurrentZone1 =
                                CurrentZone0#current_zone{function_data = FunctionData1,
                                                          variable_tokens = VariableTokens1},
                            {update, CurrentZone1, TokensLeft};
                        _ ->
                            CurrentZone = #current_zone{opened_zone = OpenedZone,
                                                        function_data = FunctionData,
                                                        variable_tokens = VariableTokens},
                            {new, CurrentZone, [], StartToken, TokensLeft}
                    end;
                undefined ->
                    undefined
            end
    end.

-spec function_zone_begin(Tokens :: [token()]) ->
          {ok, opened_zone(), function_data(), [token()], token(), [token()]} | undefined.
function_zone_begin([?UNRESOLVED_MACRO_TOKEN(_) | _]) ->
    undefined;
function_zone_begin([{atom, _, FunctionName} = FirstToken, {'(', _} | TokensLeft0]) ->
    Location = esrv_parser_lib:get_token_location(FirstToken),
    Column = esrv_parser_lib:location_to_column(Location),
    if
        Column =:= 1 ->
            case function_zone_args(TokensLeft0, [], [], [], []) of
                {ok, Variables, Args, [{Sign, _} = StartToken | TokensLeft1]}
                  when Sign =:= '->' orelse Sign =:= 'when' ->
                    NameArity = {FunctionName, length(Args)},
                    OpenedZone = #opened_zone{type = {function, NameArity},
                                              start_location = Location},
                    VariableTokens =
                        lists:foldl(fun(Arg, Acc) ->
                                            [ T || {var, _, _} = T <- Arg ] ++ Acc
                                    end, [], Args),
                    FunctionData =
                        #function_data{location = Location,
                                       clauses = [#clause{variables = Variables}],
                                       signatures = [lists:map(fun(Arg) ->
                                                                       case lists:reverse(Arg) of
                                                                           [{var, _, Name} | _] ->
                                                                               Name;
                                                                           _ ->
                                                                               none
                                                                       end
                                                               end, Args)]},
                    {ok, OpenedZone, FunctionData, VariableTokens, StartToken, TokensLeft1};
                _ ->
                    undefined
            end;
        true ->
            undefined
    end;
function_zone_begin(_) ->
    undefined.

-spec function_zone_args(Tokens :: [token()],
                         TokensAcc :: [token()],
                         InnerStack :: [token()],
                         VAcc :: [name()],
                         AAcc :: [[token()]]) ->
          {ok, [name()], [[token()]], [token()]} | undefined.
function_zone_args([], _, _, _, _) ->
    undefined;
function_zone_args([{')', _} | T], [], [], VAcc, []) ->
    {ok, VAcc, [], T};
function_zone_args([{')', _} | T], TokensAcc, [], VAcc, AAcc) ->
    {ok, VAcc, lists:reverse([lists:reverse(TokensAcc) | AAcc]), T};
function_zone_args([{',', _} | T], TokensAcc, [], VAcc, AAcc) ->
    function_zone_args(T, [], [], VAcc, [lists:reverse(TokensAcc) | AAcc]);
function_zone_args([{'[', _} = Token | T], TokensAcc0, InnerStack, VAcc, AAcc) ->
    function_zone_args(T, [Token | TokensAcc0], [Token | InnerStack], VAcc, AAcc);
function_zone_args([{']', _} = Token | T], TokensAcc0, [{'[', _} | InnerStack], VAcc, AAcc) ->
    function_zone_args(T, [Token | TokensAcc0], InnerStack, VAcc, AAcc);
function_zone_args([{'{', _} = Token | T], TokensAcc0, InnerStack, VAcc, AAcc) ->
    function_zone_args(T, [Token | TokensAcc0], [Token | InnerStack], VAcc, AAcc);
function_zone_args([{'}', _} = Token | T], TokensAcc0, [{'{', _} | InnerStack], VAcc, AAcc) ->
    function_zone_args(T, [Token | TokensAcc0], InnerStack, VAcc, AAcc);
function_zone_args([{'<<', _} = Token | T], TokensAcc0, InnerStack, VAcc, AAcc) ->
    function_zone_args(T, [Token | TokensAcc0], [Token | InnerStack], VAcc, AAcc);
function_zone_args([{'>>', _} = Token | T], TokensAcc0, [{'<<', _} | InnerStack], VAcc, AAcc) ->
    function_zone_args(T, [Token | TokensAcc0], InnerStack, VAcc, AAcc);
function_zone_args([{var, _, Variable} = Token | T], TokensAcc0, InnerStack, VAcc, AAcc) ->
    function_zone_args(T, [Token | TokensAcc0], InnerStack, [Variable | VAcc], AAcc);
function_zone_args([Token | T], TokensAcc0, InnerStack, VAcc, AAcc) ->
    function_zone_args(T, [Token | TokensAcc0], InnerStack, VAcc, AAcc).

-spec close_zone(State :: #pt_state{}) -> #pt_state{}.
close_zone(#pt_state{tokens_out = TokensOut,
                     functions = Functions0,
                     current_zone = CurrentZone,
                     zones = Zones0,
                     pois = Pois0} = State0) ->
    case CurrentZone of
        #current_zone{opened_zone = #opened_zone{type = {function, NameArity},
                                                 start_location = StartLocation},
                      function_data = FunctionData,
                      variable_tokens = VariableTokens} ->
            Functions1 =
                maps:update_with(NameArity,
                                 fun(FunctionData0) ->
                                         merge_function_data(FunctionData, FunctionData0)
                                 end, FunctionData, Functions0),
            EndLocation =
                case TokensOut of
                    [{dot, _} = Token | _] ->
                        esrv_parser_lib:get_token_location(Token);
                    [Token | _] ->
                        esrv_parser_lib:get_after_token_location(Token)
                end,
            Zone = #zone{type = {function, NameArity},
                         start_location = StartLocation,
                         end_location = EndLocation},
            Pois1 =
                lists:foldl(fun({var, _, Variable} = VariableToken, Pois00) ->
                                    esrv_parser_lib:add_token_poi(VariableToken,
                                                                  {variable, Variable},
                                                                  undefined,
                                                                  Pois00)
                            end, Pois0, VariableTokens),
            State0#pt_state{tokens_out = [],
                            functions = Functions1,
                            current_zone = undefined,
                            zones = [Zone | Zones0],
                            pois = Pois1};
        #current_zone{opened_zone = OpenedZone} ->
            Zone = esrv_zone_parser:zone_end(OpenedZone, TokensOut),
            State0#pt_state{tokens_out = [],
                            current_zone = undefined,
                            zones = [Zone | Zones0]};
        undefined ->
            State0
    end.

-spec merge_function_data(ToMerge :: function_data(), Acc :: function_data()) -> function_data().
merge_function_data(#function_data{clauses = ClausesToMerge,
                                   signatures = SignaturesToMerge},
                    #function_data{clauses = ClausesAcc,
                                   signatures = SignaturesAcc} = Acc) ->
    Acc#function_data{clauses = ClausesAcc ++ ClausesToMerge,
                      signatures = SignaturesAcc ++ SignaturesToMerge}.

-spec add_function_poi(Tokens :: [token()],
                       CurrentZone :: #current_zone{},
                       State :: #pt_state{}) -> #pt_state{}.
add_function_poi([{atom, _, _} = Token, {'(', _} | _],
                 #current_zone{opened_zone = #opened_zone{type = {function, NameArity}}},
                 #pt_state{pois = Pois0} = State0) ->
    State0#pt_state{pois = add_token_poi(Token, {local_function, NameArity}, Pois0)};
add_function_poi(_, _, State) ->
    State.

-spec add_function_clause_poi(Tokens :: [token()],
                              CurrentZone :: #current_zone{},
                              State :: #pt_state{}) -> #pt_state{}.
add_function_clause_poi([{atom, _, _} = Token, {'(', _} | _],
                        #current_zone{opened_zone = #opened_zone{type = {function, NameArity}}},
                        #pt_state{pois = Pois0} = State0) ->
    State0#pt_state{pois = esrv_parser_lib:add_token_poi(Token,
                                                         {function_clause, NameArity},
                                                         undefined,
                                                         Pois0)};
add_function_clause_poi(_, _, State) ->
    State.

-spec process_token(State :: #pt_state{}) -> #pt_state{}.
process_token(#pt_state{current_zone = #current_zone{opened_zone = OpenedZone}} = State) ->
    case OpenedZone of
        #opened_zone{type = export} ->
            process_export(function, State);
        #opened_zone{type = export_type} ->
            process_export(type, State);
        #opened_zone{type = {import, ModuleName}} ->
            process_import(ModuleName, State);
        #opened_zone{type = type} ->
            process_type(State);
        #opened_zone{type = spec_body} ->
            process_type(State);
        #opened_zone{type = {function, _}} ->
            process_function(State);
        #opened_zone{type = record_body} ->
            process_type_or_function(State);
        _ ->
            process_undefined(State)
    end;
process_token(State) ->
    process_undefined(State).

-spec process_export(Kind :: function | type, State :: #pt_state{}) -> #pt_state{}.
process_export(Kind, #pt_state{tokens_in = TokensIn, pois = Pois0} = State0) ->
    case TokensIn of
        [{Type, _, Name} = Token, {'/', _}, {integer, _, Arity} | T]
          when Type =:= atom orelse Type =:= var ->
            PoiData =
                case Kind of
                    function ->
                        {local_function, {Name, Arity}};
                    type ->
                        {local_type, {Name, Arity}}
                end,
            Pois1 = esrv_parser_lib:add_token_poi(Token, PoiData, undefined, Pois0),
            State0#pt_state{tokens_in = T, pois = Pois1};
        _ ->
            process_undefined(State0)
    end.

-spec process_import(ModuleName :: module(), State :: #pt_state{}) -> #pt_state{}.
process_import(ModuleName, #pt_state{tokens_in = TokensIn, pois = Pois0} = State0) ->
    case TokensIn of
        [{Type, _, Name} = Token, {'/', _}, {integer, _, Arity} | T]
          when Type =:= atom orelse Type =:= var ->
            PoiData = {remote_function, ModuleName, {Name, Arity}},
            Pois1 = esrv_parser_lib:add_token_poi(Token, PoiData, undefined, Pois0),
            State0#pt_state{tokens_in = T, pois = Pois1};
        _ ->
            process_undefined(State0)
    end.

-spec process_type(State :: #pt_state{}) -> #pt_state{}.
process_type(#pt_state{tokens_in = TokensIn0,
                       tokens_out = TokensOut0,
                       pois = Pois0} = State0) ->
    case process_type(TokensIn0, TokensOut0, Pois0) of
        {ok, TokensIn1, TokensOut1, Pois1} ->
            State0#pt_state{tokens_in = TokensIn1,
                            tokens_out = TokensOut1,
                            pois = Pois1};
        undefined ->
            process_undefined(State0)
    end.

-spec process_type(TokensIn :: [token()],
                   TokensOut :: [token()],
                   Pois :: pois()) -> {ok, [token()], [token()], pois()} | undefined.
process_type(TokensIn0, TokensOut0, Pois0) ->
    case TokensIn0 of
        [?UNRESOLVED_MACRO_TOKEN(_) | TokensIn1] ->
            {ok, TokensIn1, TokensOut0, Pois0};
        [{atom, _, ModuleName} = ModuleToken, {':', _},
         {atom, _, TypeName} = TypeToken, {'(', _} = Token | TokensIn1] ->
            TokensOut1 = [Token | TokensOut0],
            Pois1 = esrv_parser_lib:add_token_poi(ModuleToken,
                                                  {module, ModuleName},
                                                  undefined,
                                                  Pois0),
            case process_type_args(TokensIn1, TokensOut1, Pois1, [], false, 0) of
                {ok, TokensIn2, TokensOut2, Pois2, Arity} ->
                    Pois3 = esrv_parser_lib:add_token_poi(TypeToken,
                                                          {remote_type,
                                                           ModuleName,
                                                           {TypeName, Arity}},
                                                          undefined,
                                                          Pois2),
                    {ok, TokensIn2, TokensOut2, Pois3};
                undefined ->
                    Pois2 = esrv_parser_lib:add_token_poi(TypeToken,
                                                          {remote_type_name,
                                                           ModuleName,
                                                           TypeName},
                                                          undefined,
                                                          Pois1),
                    {ok, TokensIn1, TokensOut1, Pois2}
            end;
        [{atom, _, TypeName} = TypeToken, {'(', _} = Token | TokensIn1] ->
            TokensOut1 = [Token | TokensOut0],
            case process_type_args(TokensIn1, TokensOut1, Pois0, [], false, 0) of
                {ok, TokensIn2, TokensOut2, Pois1, Arity} ->
                    Pois2 = esrv_parser_lib:add_token_poi(TypeToken,
                                                          {local_type,
                                                           {TypeName, Arity}},
                                                          undefined,
                                                          Pois1),
                    {ok, TokensIn2, TokensOut2, Pois2};
                undefined ->
                    Pois1 = esrv_parser_lib:add_token_poi(TypeToken,
                                                          {local_type_name, TypeName},
                                                          undefined,
                                                          Pois0),
                    {ok, TokensIn1, TokensOut1, Pois1}
            end;
        _ ->
            undefined
    end.

-spec process_type_args(TokensIn :: [token()],
                        TokensOut :: [token()],
                        Pois :: pois(),
                        InnerStack :: [token()],
                        IsNotEmpty :: boolean(),
                        Acc :: non_neg_integer()) ->
          {ok, [token()], [token()], pois(), arity()} | undefined.
process_type_args([], _, _, _, _, _) ->
    undefined;
process_type_args([{')', _} = Token | T], TokensOut, Pois, [], true, Acc) ->
    {ok, T, [Token | TokensOut], Pois, Acc + 1};
process_type_args([{')', _} = Token | T], TokensOut, Pois, [], false, 0) ->
    {ok, T, [Token | TokensOut], Pois, 0};
process_type_args([{')', _} | _], _, _, [], false, _) ->
    undefined;
process_type_args([{',', _} | T], TokensOut, Pois, [], true, Acc) ->
    process_type_args(T, TokensOut, Pois, [], false, Acc + 1);
process_type_args([{',', _} | _], _, _, [], false, _) ->
    undefined;
process_type_args([{'(', _} = Token | T], TokensOut, Pois, InnerStack, _, Acc) ->
    process_type_args(T, TokensOut, Pois, [Token | InnerStack], true, Acc);
process_type_args([{')', _} | T], TokensOut, Pois, [{'(', _} | InnerStack], _, Acc) ->
    process_type_args(T, TokensOut, Pois, InnerStack, true, Acc);
process_type_args([{'[', _} = Token | T], TokensOut, Pois, InnerStack, _, Acc) ->
    process_type_args(T, TokensOut, Pois, [Token | InnerStack], true, Acc);
process_type_args([{']', _} | T], TokensOut, Pois, [{'[', _} | InnerStack], _, Acc) ->
    process_type_args(T, TokensOut, Pois, InnerStack, true, Acc);
process_type_args([{'{', _} = Token | T], TokensOut, Pois, InnerStack, _, Acc) ->
    process_type_args(T, TokensOut, Pois, [Token | InnerStack], true, Acc);
process_type_args([{'}', _} | T], TokensOut, Pois, [{'{', _} | InnerStack], _, Acc) ->
    process_type_args(T, TokensOut, Pois, InnerStack, true, Acc);
process_type_args([{'<<', _} = Token | T], TokensOut, Pois, InnerStack, _, Acc) ->
    process_type_args(T, TokensOut, Pois, [Token | InnerStack], true, Acc);
process_type_args([{'>>', _} | T], TokensOut, Pois, [{'<<', _} | InnerStack], _, Acc) ->
    process_type_args(T, TokensOut, Pois, InnerStack, true, Acc);
process_type_args([_ | T] = TokensIn0, TokensOut0, Pois0, InnerStack, _, Acc) ->
    case check_zone(TokensIn0, undefined) of
        undefined ->
            case process_type(TokensIn0, TokensOut0, Pois0) of
                {ok, TokensIn1, TokensOut1, Pois1} ->
                    process_type_args(TokensIn1, TokensOut1, Pois1, InnerStack, true, Acc);
                undefined ->
                    process_type_args(T, TokensOut0, Pois0, InnerStack, true, Acc)
            end;
        _ ->
            undefined
    end.

-spec process_function(State :: #pt_state{}) -> #pt_state{}.
process_function(#pt_state{tokens_in = TokensIn0,
                           tokens_out = TokensOut0,
                           current_zone = CurrentZone0,
                           pois = Pois0} = State0) ->
    case process_function(TokensIn0, TokensOut0, Pois0) of
        {ok, TokensIn1, TokensOut1, Pois1, VariableTokens} ->
            CurrentZone1 =
                lists:foldl(fun variable_token_into_current_zone/2, CurrentZone0, VariableTokens),
            State0#pt_state{tokens_in = TokensIn1,
                            tokens_out = TokensOut1,
                            current_zone = CurrentZone1,
                            pois = Pois1};
        undefined ->
            process_variable(State0)
    end.

-spec process_function(TokensIn :: [token()],
                       TokensOut :: [token()],
                       Pois :: pois()) ->
          {ok, [token()], [token()], pois(), [token()]} | undefined.
process_function(TokensIn0, TokensOut0, Pois0) ->
    case TokensIn0 of
        [?UNRESOLVED_MACRO_TOKEN(MarcroArgsTokens) | TokensIn1] ->
            {TokensOut, Pois1, VariableTokens} = process_function_umt(MarcroArgsTokens, Pois0),
            {ok, TokensIn1, TokensOut ++ TokensOut0, Pois1, VariableTokens};
        [{atom, _, ModuleName} = ModuleToken, {':', _},
         {atom, _, FunctionName} = FunctionToken, {'(', _} = Token | TokensIn1] ->
            TokensOut1 = [Token | TokensOut0],
            Pois1 = esrv_parser_lib:add_token_poi(ModuleToken,
                                                  {module, ModuleName},
                                                  undefined,
                                                  Pois0),
            case process_function_args(TokensIn1, TokensOut1, Pois1, [], false, 0, []) of
                {ok, TokensIn2, TokensOut2, Pois2, Arity, VariableTokens} ->
                    Pois3 = esrv_parser_lib:add_token_poi(FunctionToken,
                                                          {remote_function,
                                                           ModuleName,
                                                           {FunctionName, Arity}},
                                                          undefined,
                                                          Pois2),
                    {ok, TokensIn2, TokensOut2, Pois3, VariableTokens};
                undefined ->
                    Pois2 = esrv_parser_lib:add_token_poi(FunctionToken,
                                                          {remote_function_name,
                                                           ModuleName,
                                                           FunctionName},
                                                          undefined,
                                                          Pois1),
                    {ok, TokensIn1, TokensOut1, Pois2, []}
            end;
        [{atom, _, FunctionName} = FunctionToken, {'(', _} = Token | TokensIn1] ->
            TokensOut1 = [Token | TokensOut0],
            case process_function_args(TokensIn1, TokensOut1, Pois0, [], false, 0, []) of
                {ok, TokensIn2, TokensOut2, Pois1, Arity, VariableTokens} ->
                    Pois2 = esrv_parser_lib:add_token_poi(FunctionToken,
                                                          {local_function,
                                                           {FunctionName, Arity}},
                                                          undefined,
                                                          Pois1),
                    {ok, TokensIn2, TokensOut2, Pois2, VariableTokens};
                undefined ->
                    Pois1 = esrv_parser_lib:add_token_poi(FunctionToken,
                                                          {local_function_name, FunctionName},
                                                          undefined,
                                                          Pois0),
                    {ok, TokensIn1, TokensOut1, Pois1, []}
            end;
        _ ->
            undefined
    end.

-spec process_function_umt(MarcroArgsTokens :: [marcro_arg_tokens()], Pois :: pois()) ->
          {[token()], pois(), [token()]}.
process_function_umt(MarcroArgsTokens, Pois0) ->
    lists:foldl(fun(MarcroArgTokens, {TokensOut00, Pois00, VariableTokens00}) ->
                        process_function_umt(MarcroArgTokens, TokensOut00, Pois00, VariableTokens00)
                end, {[], Pois0, []}, MarcroArgsTokens).

-spec process_function_umt(TokensIn :: [token()],
                           TokensOut :: [token()],
                           Pois :: pois(),
                           VariableTokens :: [token()]) -> {[token()], pois(), [token()]}.
process_function_umt([], TokensOut, Pois, VariableTokens) ->
    {case TokensOut of [Token | _] -> [Token]; [] -> [] end, Pois, VariableTokens};
process_function_umt(TokensIn0, TokensOut0, Pois0, VariableTokens0) ->
    case process_function(TokensIn0, TokensOut0, Pois0) of
        {ok, TokensIn1, TokensOut1, Pois1, VariableTokens} ->
            process_function_umt(TokensIn1, TokensOut1, Pois1, VariableTokens ++ VariableTokens0);
        undefined ->
            {TokensIn1, TokensOut1} = move_token(TokensIn0, TokensOut0),
            process_function_umt(TokensIn1, TokensOut1, Pois0, VariableTokens0)
    end.

-spec process_function_args(TokensIn :: [token()],
                            TokensOut :: [token()],
                            Pois :: pois(),
                            InnerStack :: [token()],
                            IsNotEmpty :: boolean(),
                            AAcc :: non_neg_integer(),
                            VAcc :: [token()]) ->
          {ok, [token()], [token()], pois(), arity(), [token()]} | undefined.
process_function_args([], _, _, _, _, _, _) ->
    undefined;
process_function_args([{')', _} = Token | T], TokensOut, Pois, [], true, AAcc, VAcc) ->
    {ok, T, [Token | TokensOut], Pois, AAcc + 1, VAcc};
process_function_args([{')', _} = Token | T], TokensOut, Pois, [], false, 0, VAcc) ->
    {ok, T, [Token | TokensOut], Pois, 0, VAcc};
process_function_args([{')', _} | _], _, _, [], false, _, _) ->
    undefined;
process_function_args([{',', _} | T], TokensOut, Pois, [], true, AAcc, VAcc) ->
    process_function_args(T, TokensOut, Pois, [], false, AAcc + 1, VAcc);
process_function_args([{',', _} | _], _, _, [], false, _, _) ->
    undefined;
process_function_args([{'(', _} = Token | T], TokensOut, Pois, InnerStack, _, AAcc, VAcc) ->
    process_function_args(T, TokensOut, Pois, [Token | InnerStack], true, AAcc, VAcc);
process_function_args([{')', _} | T], TokensOut, Pois,
                      [{'(', _} | InnerStack], _, AAcc, VAcc) ->
    process_function_args(T, TokensOut, Pois, InnerStack, true, AAcc, VAcc);
process_function_args([{'[', _} = Token | T], TokensOut, Pois, InnerStack, _, AAcc, VAcc) ->
    process_function_args(T, TokensOut, Pois, [Token | InnerStack], true, AAcc, VAcc);
process_function_args([{']', _} | T], TokensOut, Pois,
                      [{'[', _} | InnerStack], _, AAcc, VAcc) ->
    process_function_args(T, TokensOut, Pois, InnerStack, true, AAcc, VAcc);
process_function_args([{'{', _} = Token | T], TokensOut, Pois, InnerStack, _, AAcc, VAcc) ->
    process_function_args(T, TokensOut, Pois, [Token | InnerStack], true, AAcc, VAcc);
process_function_args([{'}', _} | T], TokensOut, Pois,
                      [{'{', _} | InnerStack], _, AAcc, VAcc) ->
    process_function_args(T, TokensOut, Pois, InnerStack, true, AAcc, VAcc);
process_function_args([{'<<', _} = Token | T], TokensOut, Pois, InnerStack, _, AAcc, VAcc) ->
    process_function_args(T, TokensOut, Pois, [Token | InnerStack], true, AAcc, VAcc);
process_function_args([{'>>', _} | T], TokensOut, Pois,
                      [{'<<', _} | InnerStack], _, AAcc, VAcc) ->
    process_function_args(T, TokensOut, Pois, InnerStack, true, AAcc, VAcc);
process_function_args([{Atom, _} | _], _, _, _, _, _, _)
  when Atom =:= ')'; Atom =:= ']'; Atom =:= '}'; Atom =:= '>>' ->
    undefined;
process_function_args([{Atom, _} = Token | T], TokensOut, Pois, InnerStack, _, AAcc, VAcc)
  when Atom =:= 'case'; Atom =:= 'if'; Atom =:= 'try'; Atom =:= 'begin'; Atom =:= 'fun' ->
    process_function_args(T, TokensOut, Pois, [Token | InnerStack], true, AAcc, VAcc);
process_function_args([{'end', _} | T], TokensOut, Pois, [{Atom, _} | InnerStack], _, AAcc, VAcc)
  when Atom =:= 'case'; Atom =:= 'if'; Atom =:= 'try'; Atom =:= 'begin'; Atom =:= 'fun' ->
    process_function_args(T, TokensOut, Pois, InnerStack, true, AAcc, VAcc);
process_function_args([{var, _, _} = Token | T], TokensOut0, Pois0, InnerStack, _, AAcc, VAcc) ->
    process_function_args(T, [Token | TokensOut0], Pois0, InnerStack, true, AAcc, [Token | VAcc]);
process_function_args([_ | T] = TokensIn0, TokensOut0, Pois0, InnerStack, _, AAcc, VAcc) ->
    case check_zone(TokensIn0, undefined) of
        undefined ->
            case process_function(TokensIn0, TokensOut0, Pois0) of
                {ok, TokensIn1, TokensOut1, Pois1, VariableTokens} ->
                    process_function_args(TokensIn1, TokensOut1, Pois1,
                                          InnerStack, true, AAcc, VariableTokens ++ VAcc);
                undefined ->
                    process_function_args(T, TokensOut0, Pois0,
                                          InnerStack, true, AAcc, VAcc)
            end;
        _ ->
            undefined
    end.

-spec process_type_or_function(State :: #pt_state{}) -> #pt_state{}.
process_type_or_function(#pt_state{tokens_in = TokensIn0,
                                   tokens_out = TokensOut0,
                                   pois = Pois0} = State0) ->
    case process_type_or_function(TokensIn0, TokensOut0, Pois0) of
        {ok, TokensIn1, TokensOut1, Pois1} ->
            State0#pt_state{tokens_in = TokensIn1,
                            tokens_out = TokensOut1,
                            pois = Pois1};
        undefined ->
            process_undefined(State0)
    end.

-spec process_type_or_function(TokensIn :: [token()],
                               TokensOut :: [token()],
                               Pois :: pois()) ->
          {ok, [token()], [token()], pois()} | undefined.
process_type_or_function(TokensIn0, TokensOut0, Pois0) ->
    case TokensIn0 of
        [?UNRESOLVED_MACRO_TOKEN(_) | TokensIn1] ->
            {ok, TokensIn1, TokensOut0, Pois0};
        [{atom, _, ModuleName} = ModuleToken, {':', _},
         {atom, _, TypeOrFunctionName} = TypeOrFunctionToken, {'(', _} = Token | TokensIn1] ->
            TokensOut1 = [Token | TokensOut0],
            Pois1 = esrv_parser_lib:add_token_poi(ModuleToken,
                                                  {module, ModuleName},
                                                  undefined,
                                                  Pois0),
            case process_type_or_function_args(TokensIn1, TokensOut1, Pois1, [], false, 0) of
                {ok, TokensIn2, TokensOut2, Pois2, Arity} ->
                    Pois3 = esrv_parser_lib:add_token_poi(TypeOrFunctionToken,
                                                          {remote_type_or_function,
                                                           ModuleName,
                                                           {TypeOrFunctionName, Arity}},
                                                          undefined,
                                                          Pois2),
                    {ok, TokensIn2, TokensOut2, Pois3};
                undefined ->
                    Pois2 = esrv_parser_lib:add_token_poi(TypeOrFunctionToken,
                                                          {remote_type_or_function_name,
                                                           ModuleName,
                                                           TypeOrFunctionName},
                                                          undefined,
                                                          Pois1),
                    {ok, TokensIn1, TokensOut1, Pois2}
            end;
        [{atom, _, TypeOrFunctionName} = TypeOrFunctionToken, {'(', _} = Token | TokensIn1] ->
            TokensOut1 = [Token | TokensOut0],
            case process_type_or_function_args(TokensIn1, TokensOut1, Pois0, [], false, 0) of
                {ok, TokensIn2, TokensOut2, Pois1, Arity} ->
                    Pois2 = esrv_parser_lib:add_token_poi(TypeOrFunctionToken,
                                                          {local_type_or_function,
                                                           {TypeOrFunctionName, Arity}},
                                                          undefined,
                                                          Pois1),
                    {ok, TokensIn2, TokensOut2, Pois2};
                undefined ->
                    Pois1 = esrv_parser_lib:add_token_poi(TypeOrFunctionToken,
                                                          {local_type_or_function_name,
                                                           TypeOrFunctionName},
                                                          undefined,
                                                          Pois0),
                    {ok, TokensIn1, TokensOut1, Pois1}
            end;
        _ ->
            undefined
    end.

-spec process_type_or_function_args(TokensIn :: [token()],
                                    TokensOut :: [token()],
                                    Pois :: pois(),
                                    InnerStack :: [token()],
                                    IsNotEmpty :: boolean(),
                                    Acc :: non_neg_integer()) ->
          {ok, [token()], [token()], pois(), arity()} | undefined.
process_type_or_function_args([], _, _, _, _, _) ->
    undefined;
process_type_or_function_args([{')', _} = Token | T], TokensOut, Pois, [], true, Acc) ->
    {ok, T, [Token | TokensOut], Pois, Acc + 1};
process_type_or_function_args([{')', _} = Token | T], TokensOut, Pois, [], false, 0) ->
    {ok, T, [Token | TokensOut], Pois, 0};
process_type_or_function_args([{')', _} | _], _, _, [], false, _) ->
    undefined;
process_type_or_function_args([{',', _} | T], TokensOut, Pois, [], true, Acc) ->
    process_type_or_function_args(T, TokensOut, Pois, [], false, Acc + 1);
process_type_or_function_args([{',', _} | _], _, _, [], false, _) ->
    undefined;
process_type_or_function_args([{'(', _} = Token | T], TokensOut, Pois, InnerStack, _, Acc) ->
    process_type_or_function_args(T, TokensOut, Pois, [Token | InnerStack], true, Acc);
process_type_or_function_args([{')', _} | T], TokensOut, Pois,
                              [{'(', _} | InnerStack], _, Acc) ->
    process_type_or_function_args(T, TokensOut, Pois, InnerStack, true, Acc);
process_type_or_function_args([{'[', _} = Token | T], TokensOut, Pois, InnerStack, _, Acc) ->
    process_type_or_function_args(T, TokensOut, Pois, [Token | InnerStack], true, Acc);
process_type_or_function_args([{']', _} | T], TokensOut, Pois,
                              [{'[', _} | InnerStack], _, Acc) ->
    process_type_or_function_args(T, TokensOut, Pois, InnerStack, true, Acc);
process_type_or_function_args([{'{', _} = Token | T], TokensOut, Pois, InnerStack, _, Acc) ->
    process_type_or_function_args(T, TokensOut, Pois, [Token | InnerStack], true, Acc);
process_type_or_function_args([{'}', _} | T], TokensOut, Pois,
                              [{'{', _} | InnerStack], _, Acc) ->
    process_type_or_function_args(T, TokensOut, Pois, InnerStack, true, Acc);
process_type_or_function_args([{'<<', _} = Token | T], TokensOut, Pois, InnerStack, _, Acc) ->
    process_type_or_function_args(T, TokensOut, Pois, [Token | InnerStack], true, Acc);
process_type_or_function_args([{'>>', _} | T], TokensOut, Pois,
                              [{'<<', _} | InnerStack], _, Acc) ->
    process_type_or_function_args(T, TokensOut, Pois, InnerStack, true, Acc);
process_type_or_function_args([{Atom, _} = Token | T], TokensOut, Pois, InnerStack, _, Acc)
  when Atom =:= 'case'; Atom =:= 'if'; Atom =:= 'try'; Atom =:= 'begin'; Atom =:= 'fun' ->
    process_type_or_function_args(T, TokensOut, Pois, [Token | InnerStack], true, Acc);
process_type_or_function_args([{'end', _} | T], TokensOut, Pois, [{Atom, _} | InnerStack], _, Acc)
  when Atom =:= 'case'; Atom =:= 'if'; Atom =:= 'try'; Atom =:= 'begin'; Atom =:= 'fun' ->
    process_type_or_function_args(T, TokensOut, Pois, InnerStack, true, Acc);
process_type_or_function_args([_ | T] = TokensIn0, TokensOut0, Pois0, InnerStack, _, Acc) ->
    case check_zone(TokensIn0, undefined) of
        undefined ->
            case process_type_or_function(TokensIn0, TokensOut0, Pois0) of
                {ok, TokensIn1, TokensOut1, Pois1} ->
                    process_type_or_function_args(TokensIn1, TokensOut1, Pois1,
                                                  InnerStack, true, Acc);
                undefined ->
                    process_type_or_function_args(T, TokensOut0, Pois0,
                                                  InnerStack, true, Acc)
            end;
        _ ->
            undefined
    end.

-spec process_variable(State :: #pt_state{}) -> #pt_state{}.
process_variable(#pt_state{tokens_in = TokensIn,
                           current_zone = CurrentZone0} = State0) ->
    State1 =
        case TokensIn of
            [{var, _, _} = Token | _] ->
                CurrentZone1 = variable_token_into_current_zone(Token, CurrentZone0),
                State0#pt_state{current_zone = CurrentZone1};
            _ ->
                State0
        end,
    process_undefined(State1).

-spec process_undefined(State :: #pt_state{}) -> #pt_state{}.
process_undefined(#pt_state{tokens_in = []} = State) ->
    State;
process_undefined(#pt_state{tokens_in = TokensIn0, tokens_out = TokensOut0} = State0) ->
    {TokensIn1, TokensOut1} = move_token(TokensIn0, TokensOut0),
    State0#pt_state{tokens_in = TokensIn1, tokens_out = TokensOut1}.

-spec move_token(TokensIn :: [token()], TokensOut :: [token()]) -> {[token()], [token()]}.
move_token([Token | T], TokensOut) ->
    {T, [Token | TokensOut]}.

-spec variable_token_into_current_zone(VariableToken :: token(), CurrentZone :: #current_zone{}) ->
          #current_zone{}.
variable_token_into_current_zone({var, _, Variable} = Token, CurrentZone0) ->
    case CurrentZone0 of
        #current_zone{opened_zone = #opened_zone{type = {function, _}},
                      function_data = FunctionData0,
                      variable_tokens = VariableTokens0} ->
            FunctionData1 = variable_into_function_data(Variable, FunctionData0),
            CurrentZone0#current_zone{function_data = FunctionData1,
                                      variable_tokens = [Token | VariableTokens0]};
        _ ->
            CurrentZone0
    end.

-spec variable_into_function_data(Variable :: name(), FunctionData :: #function_data{}) ->
          #function_data{}.
variable_into_function_data(Variable, FunctionData0) ->
    case FunctionData0 of
        #function_data{clauses = [Clause0 | T]} ->
            FunctionData0#function_data{clauses = [variable_into_clause(Variable, Clause0) | T]};
        _ ->
            FunctionData0
    end.

-spec variable_into_clause(Variable :: name(), Clause :: #clause{}) -> #clause{}.
variable_into_clause(Variable, #clause{variables = Variables0} = Clause0) ->
    Clause0#clause{variables = [Variable | Variables0]}.

%%%-------------------------------------------------------------------
%%% Points of interest
%%%-------------------------------------------------------------------
-spec pois_module(ModuleNode :: syntax_tree(), ModuleInfo :: module_info()) ->
          NewModuleInfo :: module_info().
pois_module(ModuleNode, #module_info{pois = Pois0} = ModuleInfo0) ->
    case check_type({ModuleNode, atom}) of
        true ->
            Module = erl_syntax:atom_value(ModuleNode),
            Pois1 = esrv_parser_lib:add_node_poi(ModuleNode,
                                                 {module, Module},
                                                 undefined,
                                                 Pois0),
            ModuleInfo0#module_info{pois = Pois1};
        false ->
            ModuleInfo0
    end.

-spec pois_function_clauses(ClauseNode :: syntax_tree(),
                            NameArity :: name_arity(),
                            ModuleInfo :: module_info()) -> NewModuleInfo :: module_info().
pois_function_clauses(ClauseNodes, NameArity, ModuleInfo0) ->
    lists:foldl(fun(ClauseNode, #module_info{pois = Pois00} = ModuleInfo00) ->
                        Pois01 = esrv_parser_lib:add_node_poi(ClauseNode,
                                                              {function_clause, NameArity},
                                                              undefined,
                                                              Pois00),
                        ModuleInfo00#module_info{pois = Pois01}
                end, ModuleInfo0, ClauseNodes).

-spec pois_variable(VariableNode :: syntax_tree(),
                    PoiDefinition :: poi_definition(),
                    ModuleInfo :: module_info()) -> NewModuleInfo :: module_info().
pois_variable(VariableNode, PoiDefinition, #module_info{pois = Pois0} = ModuleInfo0) ->
    VariableName = erl_syntax:variable_name(VariableNode),
    Pois1 = esrv_parser_lib:add_node_poi(VariableNode,
                                         {variable, VariableName},
                                         PoiDefinition,
                                         Pois0),
    ModuleInfo0#module_info{pois = Pois1}.

-spec pois_field_name(RecordNameNode :: syntax_tree(),
                      FieldNameNode :: syntax_tree(),
                      ModuleInfo :: module_info()) -> NewModuleInfo :: module_info().
pois_field_name(RecordNameNode, FieldNameNode, #module_info{pois = Pois0} = ModuleInfo0) ->
    case check_types([{RecordNameNode, atom}, {FieldNameNode, atom}]) of
        true ->
            RecordName = erl_syntax:atom_value(RecordNameNode),
            FieldName = erl_syntax:atom_value(FieldNameNode),
            Pois1 = esrv_parser_lib:add_node_poi(FieldNameNode,
                                                 {field, RecordName, FieldName},
                                                 undefined,
                                                 Pois0),
            ModuleInfo0#module_info{pois = Pois1};
        false ->
            ModuleInfo0
    end.

-spec pois_remote_type(ModuleNode :: syntax_tree(),
                       FunctionNode :: syntax_tree(),
                       Arity :: arity(),
                       ModuleInfo :: module_info()) -> NewModuleInfo :: module_info().
pois_remote_type(ModuleNode, FunctionNode, Arity, #module_info{pois = Pois0} = ModuleInfo0) ->
    case check_types([{ModuleNode, atom}, {FunctionNode, atom}]) of
        true ->
            Module = erl_syntax:atom_value(ModuleNode),
            NameArity = {erl_syntax:atom_value(FunctionNode), Arity},
            Pois1 = esrv_parser_lib:add_node_poi(FunctionNode,
                                                 {remote_type, Module, NameArity},
                                                 undefined,
                                                 Pois0),
            pois_module(ModuleNode, ModuleInfo0#module_info{pois = Pois1});
        false ->
            ModuleInfo0
    end.

-spec pois_remote_function(ModuleNode :: syntax_tree(),
                           FunctionNode :: syntax_tree(),
                           Arity :: arity(),
                           ModuleInfo :: module_info()) -> NewModuleInfo :: module_info().
pois_remote_function(ModuleNode, FunctionNode, Arity, #module_info{pois = Pois0} = ModuleInfo0) ->
    case check_types([{ModuleNode, atom}, {FunctionNode, atom}]) of
        true ->
            Module = erl_syntax:atom_value(ModuleNode),
            NameArity = {erl_syntax:atom_value(FunctionNode), Arity},
            Pois1 = esrv_parser_lib:add_node_poi(FunctionNode,
                                                 {remote_function, Module, NameArity},
                                                 undefined,
                                                 Pois0),
            pois_module(ModuleNode, ModuleInfo0#module_info{pois = Pois1});
        false ->
            ModuleInfo0
    end.

-spec pois_local_type(FunctionNode :: syntax_tree(),
                      Arity :: arity(),
                      ModuleInfo :: module_info()) -> NewModuleInfo :: module_info().
pois_local_type(FunctionNode, Arity, #module_info{pois = Pois0} = ModuleInfo0) ->
    case check_type({FunctionNode, atom}) of
        true ->
            NameArity = {erl_syntax:atom_value(FunctionNode), Arity},
            Pois1 = esrv_parser_lib:add_node_poi(FunctionNode,
                                                 {local_type, NameArity},
                                                 undefined,
                                                 Pois0),
            ModuleInfo0#module_info{pois = Pois1};
        false ->
            ModuleInfo0
    end.

-spec pois_local_function(FunctionNode :: syntax_tree(),
                          Arity :: arity(),
                          ModuleInfo :: module_info()) -> NewModuleInfo :: module_info().
pois_local_function(FunctionNode, Arity, #module_info{pois = Pois0} = ModuleInfo0) ->
    case check_type({FunctionNode, atom}) of
        true ->
            NameArity = {erl_syntax:atom_value(FunctionNode), Arity},
            Pois1 = esrv_parser_lib:add_node_poi(FunctionNode,
                                                 {local_function, NameArity},
                                                 undefined,
                                                 Pois0),
            ModuleInfo0#module_info{pois = Pois1};
        false ->
            ModuleInfo0
    end.

-spec add_node_poi(Node :: syntax_tree(),
                   PoiData :: poi_data(),
                   Pois :: pois()) -> pois().
add_node_poi(Node, PoiData, Pois0) ->
    Location = esrv_parser_lib:get_node_location(Node),
    esrv_parser_lib:add_node_poi(Node, PoiData, {local, Location}, Pois0).

-spec add_token_poi(Token :: token(),
                    PoiData :: poi_data(),
                    Pois :: pois()) -> pois().
add_token_poi(Token, PoiData, Pois0) ->
    Location = esrv_parser_lib:get_token_location(Token),
    esrv_parser_lib:add_token_poi(Token, PoiData, {local, Location}, Pois0).

-spec check_types(Elements :: [{syntax_tree(), atom()}]) -> boolean().
check_types(Elements) ->
    lists:all(fun check_type/1, Elements).

-spec check_type(Element :: {syntax_tree(), atom()}) -> boolean().
check_type({Node, Type}) ->
    erl_syntax:type(Node) =:= Type.
