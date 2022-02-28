-module(esrv_tokens_parser).

-include("types.hrl").
-include("parser.hrl").
-include("records.hrl").
-include("log.hrl").

%% API
-export([parse/5]).

-type uri_location() :: {uri(), location()}.
-type fetched_macro() :: {[macro_token()], uri_location() | undefined}.
-type fetched_macros() :: #{macro_arity() => fetched_macro()}.
-type predefined_macros() :: #{name() => [token()]}.

-record(define_data, {else_occurred :: boolean()}).

-record(condition_data, {positive_occurred :: boolean(),
                         else_occurred :: boolean()}).

-record(context, {data :: #define_data{} | #condition_data{} | undefined,
                  is_active :: boolean(),
                  is_parent_active :: boolean(),
                  disabled_from :: line() | undefined}).

-record(state, {uri :: uri(),
                app_path :: path(),
                module_type :: module_type(),
                include_chain :: [uri()],
                local_macros :: macros(),
                per_uri_macros :: [{uri(), macros()}],
                predefined_macros :: predefined_macros(),
                include_data :: include_data(),
                export :: [name_arity()],
                export_type :: [name_arity()],
                import :: [import()],
                grey_ranges :: [line_range()],
                pois :: pois(),
                context :: #context{},
                parents :: [#context{}]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec parse(Uri :: uri(),
            AppPath :: path(),
            ModuleType :: module_type(),
            Data :: binary(),
            IncludeChain :: [uri()]) -> tokens_info().
parse(Uri, AppPath, ModuleType, Data, IncludeChain) ->
    StateData0 = init_data(Uri, AppPath, ModuleType, Data, IncludeChain),
    StateData1 = check_preprocessor(StateData0),
    StateData2 = check_pois(StateData1),
    StateData3 = check_tokens(StateData2),
    finalize_data(StateData3).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-type scanned() :: [tokens_data()].
-type state_data() :: {scanned(), #state{}}.

-spec check_preprocessor(StateData :: state_data()) -> NewStateData :: state_data().
check_preprocessor({Scanned0, State0}) ->
    {Scanned1, State1} =
        lists:foldl(fun(#tokens_data{tokens = Tokens00} = TokensData00, {Acc, State00}) ->
                            {Tokens01, State01} = check_macro_usage(Tokens00, [], State00),
                            {Tokens02, State02} = dodge_flow_control(Tokens01, State01),
                            case is_flow_control(Tokens02) of
                                true ->
                                    {Acc, apply_flow_control(Tokens02, State02)};
                                false ->
                                    case is_context_active(State02) of
                                        true ->
                                            {Tokens03, State03} = check_define(Tokens02, State02),
                                            {Tokens04, State04} = check_undef(Tokens03, State03),
                                            {Tokens05, State05} = check_include(Tokens04, State04),
                                            {[TokensData00#tokens_data{tokens = Tokens05} | Acc],
                                             State05};
                                        false ->
                                            {Acc, State02}
                                    end
                            end
                    end, {[], State0}, Scanned0),
    {lists:reverse(Scanned1), State1}.

%%%-------------------------------------------------------------------
%%% Init
%%%-------------------------------------------------------------------
-spec init_data(Uri :: uri(),
                AppPath :: path(),
                ModuleType :: module_type(),
                Data :: binary(),
                IncludeChain :: [uri()]) -> state_data().
init_data(Uri, AppPath, ModuleType, Data0, IncludeChain) ->
    {try
         Text =
             case filename:extension(Uri) of
                 <<".escript">> ->
                     case esrv_lib:get_escript_src(Data0) of
                         {ok, Data1} ->
                             unicode:characters_to_list(Data1);
                         no_escript ->
                             unicode:characters_to_list(Data0);
                         no_source ->
                             ""
                     end;
                 _ ->
                     unicode:characters_to_list(Data0)
             end,
         scan_text(Text, {1, 1})
     catch
         _:_ ->
             ?LOG_ERROR("Unable to scan ~s", [Uri]),
             ""
     end,
     #state{uri = Uri,
            app_path = AppPath,
            module_type = ModuleType,
            include_chain = [Uri | IncludeChain],
            local_macros = #{},
            per_uri_macros = [],
            predefined_macros = esrv_config:get_value(predefined_macros, #{}),
            include_data = #include_data{resolved = #{},
                                         unresolved = [],
                                         unresolved_libs = []},
            export = [],
            export_type = [],
            import = [],
            grey_ranges = [],
            pois = {gb_trees:empty(), sets:new()},
            context = #context{is_active = true,
                               is_parent_active = true},
            parents = []}}.

-spec scan_text(Text :: text(), Start :: erl_anno:location()) -> scanned().
scan_text(Text, Start) ->
    case erl_scan:tokens([], Text, Start, [text]) of
        {done, {ok, [{dot, _}], _}, _} ->
            [];
        {done, {ok, Tokens, End}, TextLeft} ->
            TokensData =
                #tokens_data{start_location = esrv_parser_lib:first_token_location(Tokens),
                             end_location = esrv_parser_lib:last_token_location(Tokens),
                             tokens = Tokens,
                             zones = []},
            [TokensData | scan_text(TextLeft, End)];
        {done, {error, _, End}, TextLeft} ->
            scan_text(TextLeft, End);
        {more, _} ->
            case lists:reverse(Text) of
                "\n." ++ _ ->
                    [];
                "." ++ _ ->
                    scan_text(Text ++ "\n", Start);
                _ ->
                    scan_text(Text ++ ".\n", Start)
            end
    end.

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
-spec check_macro_usage(Tokens :: [erl_scan:token()],
                        TokensAcc :: [token()],
                        State :: #state{}) -> {NewTokens :: [token()],
                                               NewState :: #state{}}.
check_macro_usage([], TokensAcc, State) ->
    {lists:reverse(TokensAcc), State};
check_macro_usage([{'?', _}, _ | _] = Tokens0, TokensAcc0, State0) ->
    {Tokens1, TokensAcc1, State1} = maybe_macro_usage(Tokens0, TokensAcc0, State0),
    check_macro_usage(Tokens1, TokensAcc1, State1);
check_macro_usage([Token | T], TokensAcc0, State) ->
    check_macro_usage(T, [Token | TokensAcc0], State).

-spec maybe_macro_usage(Tokens :: [erl_scan:token()],
                        TokensAcc :: [token()],
                        State :: #state{}) -> {NewTokens :: [erl_scan:token()],
                                               NewTokensAcc :: [token()],
                                               NewState :: #state{}}.
maybe_macro_usage([{'?', Anno}, Token | T] = Tokens0, TokensAcc0, State0) ->
    case read_macro_usage(Tokens0, State0) of
        {ok, MacroId, {MacroTokens, UriLocation}, Args, Tokens1, State1} ->
            TargetLocation = esrv_parser_lib:get_token_location(Token),
            TokensAcc1 = merge_macro_tokens(TargetLocation, MacroTokens, Args, TokensAcc0),
            State2 = add_poi(Token, {macro, MacroId}, UriLocation, State1),
            {Tokens1, TokensAcc1, State2};
        {undefined, MacroId, Args, Tokens1, State1} ->
            State2 = add_poi(Token, {macro, MacroId}, undefined, State1),
            {Tokens1, [unresolved_macro_token(Args) | TokensAcc0], State2};
        badly_formed ->
            {[Token | T], [{'?', Anno} | TokensAcc0], State0}
    end.

-spec read_macro_usage(Tokens :: [erl_scan:token()], State :: #state{}) ->
          {ok, macro_id(), fetched_macro(), mu_args(), [erl_scan:token()], #state{}} |
          {undefined, macro_id(), mu_args(), [erl_scan:token()], #state{}} |
          badly_formed.
read_macro_usage([{'?', _}, {Type, _, MacroName} | T0], State0)
  when Type =:= atom orelse Type =:= var ->
    try
        FetchedMacros = get_macros_by_name(MacroName, State0),
        IsSimpleMacro = maps:is_key(undefined, FetchedMacros),
        IsArgedMacro = lists:any(fun is_integer/1, maps:keys(FetchedMacros)),
        if
            IsArgedMacro ->
                {T1, MacroArity, Args, State1} = read_mu_args(T0, State0),
                case maps:find(MacroArity, FetchedMacros) of
                    {ok, FetchedMacro} ->
                        {ok, {MacroName, MacroArity}, FetchedMacro, Args, T1, State1};
                    error ->
                        {undefined, {MacroName, MacroArity}, Args, T1, State1}
                end;
            IsSimpleMacro ->
                FetchedMacro = maps:get(undefined, FetchedMacros),
                {ok, {MacroName, undefined}, FetchedMacro, undefined, T0, State0};
            true ->
                {T1, MacroArity, Args, State1} = read_mu_args(T0, State0),
                {undefined, {MacroName, MacroArity}, Args, T1, State1}
        end
    catch
        _:_ ->
            badly_formed
    end;
read_macro_usage(_, _) ->
    badly_formed.

-spec read_mu_args(Tokens :: [erl_scan:token()], State :: #state{}) ->
          {[erl_scan:token()], macro_arity(), mu_args(), #state{}}.
read_mu_args([{'(', _}, {')', _} | T], State) ->
    {T, 0, #{}, State};
read_mu_args([{'(', _} | T], State0) ->
    read_mu_args2(T, [], [], #{}, State0);
read_mu_args(Tokens, State) ->
    {Tokens, undefined, undefined, State}.

-spec read_mu_args2(Tokens :: [erl_scan:token()],
                    TokensAcc :: [token()],
                    InnerStack :: [token()],
                    Acc :: mu_args(),
                    State :: #state{}) -> {[erl_scan:token()], arity(), mu_args(), #state{}}.
read_mu_args2([{')', _} | T], TokensAcc, [], Acc0, State) ->
    Acc1 = add_mu_arg(TokensAcc, Acc0),
    {T, map_size(Acc1), Acc1, State};
read_mu_args2([{'?', _}, _ | _] = Tokens0, TokensAcc0, InnerStack, Acc, State0) ->
    {Tokens1, TokensAcc1, State1} = maybe_macro_usage(Tokens0, TokensAcc0, State0),
    read_mu_args2(Tokens1, TokensAcc1, InnerStack, Acc, State1);
read_mu_args2([{',', _} | T], TokensAcc, [], Acc0, State) ->
    Acc1 = add_mu_arg(TokensAcc, Acc0),
    read_mu_args2(T, [], [], Acc1, State);
read_mu_args2([{'(', _} = Token | T], TokensAcc0, InnerStack, Acc, State) ->
    read_mu_args2(T, [Token | TokensAcc0], [Token | InnerStack], Acc, State);
read_mu_args2([{')', _} = Token | T], TokensAcc0, [{'(', _} | InnerStack], Acc, State) ->
    read_mu_args2(T, [Token | TokensAcc0], InnerStack, Acc, State);
read_mu_args2([{'[', _} = Token | T], TokensAcc0, InnerStack, Acc, State) ->
    read_mu_args2(T, [Token | TokensAcc0], [Token | InnerStack], Acc, State);
read_mu_args2([{']', _} = Token | T], TokensAcc0, [{'[', _} | InnerStack], Acc, State) ->
    read_mu_args2(T, [Token | TokensAcc0], InnerStack, Acc, State);
read_mu_args2([{'<<', _} = Token | T], TokensAcc0, InnerStack, Acc, State) ->
    read_mu_args2(T, [Token | TokensAcc0], [Token | InnerStack], Acc, State);
read_mu_args2([{'>>', _} = Token | T], TokensAcc0, [{'<<', _} | InnerStack], Acc, State) ->
    read_mu_args2(T, [Token | TokensAcc0], InnerStack, Acc, State);
read_mu_args2([{Atom, _} = Token | T], TokensAcc0, InnerStack, Acc, State)
  when Atom =:= 'case'; Atom =:= 'if'; Atom =:= 'try'; Atom =:= 'begin' ->
    read_mu_args2(T, [Token | TokensAcc0], [Token | InnerStack], Acc, State);
read_mu_args2([{'end', _} = Token | T], TokensAcc0, [{Atom, _} | InnerStack], Acc, State)
  when Atom =:= 'case'; Atom =:= 'if'; Atom =:= 'try'; Atom =:= 'begin' ->
    read_mu_args2(T, [Token | TokensAcc0], InnerStack, Acc, State);
read_mu_args2([Token | T], TokensAcc0, InnerStack, Acc, State) ->
    read_mu_args2(T, [Token | TokensAcc0], InnerStack, Acc, State).

-spec add_mu_arg(TokensAcc :: [token()], Acc :: mu_args()) -> mu_args().
add_mu_arg([_|_] = TokensAcc, Acc) ->
    Acc#{map_size(Acc) + 1 => lists:reverse(TokensAcc)}.

-spec merge_macro_tokens(Location :: location(),
                         MacroTokens :: [macro_token()],
                         Args :: mu_args(),
                         TokensAcc :: [token()]) -> NewTokensAcc :: [token()].
merge_macro_tokens(Location, MacroTokens, Args, TokensAcc0) ->
    case lists:any(fun(?UNRESOLVED_MACRO_TOKEN(_)) -> true; (_) -> false end, MacroTokens) of
        true ->
            [unresolved_macro_token(Args) | TokensAcc0];
        false ->
            merge_macro_tokens2(Location, MacroTokens, Args, TokensAcc0)
    end.

-spec merge_macro_tokens2(Location :: location(),
                          MacroTokens :: [macro_token()],
                          Args :: mu_args(),
                          TokensAcc :: [token()]) -> NewTokensAcc :: [token()].
merge_macro_tokens2(Location, MacroTokens, Args, TokensAcc0) ->
    lists:foldl(fun({placeholder, N}, TokensAcc00) ->
                        lists:foldl(fun(PlaceholderToken, TokensAcc000) ->
                                            [PlaceholderToken | TokensAcc000]
                                    end, TokensAcc00, maps:get(N, Args));
                   (MacroToken, TokensAcc00) ->
                        [esrv_parser_lib:set_token_location(MacroToken, Location) | TokensAcc00]
                end, TokensAcc0, MacroTokens).

-spec unresolved_macro_token(Args :: mu_args()) -> unresolved_macro_token().
unresolved_macro_token(undefined) ->
    ?UNRESOLVED_MACRO_TOKEN([]);
unresolved_macro_token(Args) ->
    ?UNRESOLVED_MACRO_TOKEN([ Tokens || {_, Tokens} <- lists:keysort(1, maps:to_list(Args)) ]).

-spec check_define(Tokens :: [token()],
                   State :: #state{}) -> {NewTokens :: [token()],
                                          NewState :: #state{}}.
check_define(Tokens0, State0) ->
    case read_macro_definition(Tokens0, State0) of
        {ok, MacroId, MacroDefinition, State1} ->
            #state{local_macros = LocalMacros1} = State1,
            {[], State1#state{local_macros = LocalMacros1#{MacroId => MacroDefinition}}};
        undefined ->
            {Tokens0, State0}
    end.

-spec read_macro_definition(Tokens :: [token()], State :: #state{}) ->
          {ok, macro_id(), macro_definition(), #state{}} | undefined.
read_macro_definition([{'-', _}, {atom, _, define}, {'(', _},
                       {Type, _, MacroName} = Token | T0], State0)
  when Type =:= atom orelse Type =:= var ->
    try
        {T1, MacroArity, Args} = read_md_args(T0),
        {MacroTokens, State1} = read_md_body(T1, [], Args, State0),
        MacroDefinition = #macro_definition{tokens = MacroTokens,
                                            location = esrv_parser_lib:get_token_location(Token),
                                            args = Args},
        State2 = add_poi(Token, {macro, {MacroName, MacroArity}}, State1),
        {ok, {MacroName, MacroArity}, MacroDefinition, State2}
    catch
        _:_ ->
            undefined
    end;
read_macro_definition(_, _) ->
    undefined.

-spec read_md_args(Tokens :: [token()]) -> {[token()], macro_arity(), md_args()}.
read_md_args([{',', _} | T]) ->
    {T, undefined, undefined};
read_md_args([{'(', _}, {')', _}, {',', _} | T]) ->
    {T, 0, #{}};
read_md_args([{'(', _} | T]) ->
    read_md_args2(T, #{}).

-spec read_md_args2(Tokens :: [token()], Acc :: md_args()) -> {[token()], arity(), md_args()}.
read_md_args2([{var, _, Var} | T0], Acc0) ->
    Acc1 = Acc0#{Var => map_size(Acc0) + 1},
    case T0 of
        [{',', _} | T1] ->
            read_md_args2(T1, Acc1);
        [{')', _}, {',', _} | T1] ->
            {T1, map_size(Acc1), Acc1}
    end.

-spec read_md_body(Tokens :: [token()],
                   TokensAcc :: [token()],
                   Args :: md_args(),
                   State :: #state{}) -> {[macro_token()], #state{}}.
read_md_body([{')', _}, {dot, _}], TokensAcc, _, State) ->
    {lists:reverse(TokensAcc), State};
read_md_body([{var, _, Var} | T], TokensAcc0, Args, State)
  when Args =/= undefined andalso is_map_key(Var, Args) ->
    read_md_body(T, [{placeholder, maps:get(Var, Args)} | TokensAcc0], Args, State);
read_md_body([Token | T], TokensAcc0, Args, State) ->
    read_md_body(T, [Token | TokensAcc0], Args, State).

-spec check_undef(Tokens :: [token()],
                  State :: #state{}) -> {NewTokens :: [token()],
                                         NewState :: #state{}}.
check_undef([{'-', _}, {atom, _, undef},
             {'(', _}, {Type, _, MacroName} = Token, {')', _},
             {dot, _}], State0)
  when Type =:= atom orelse Type =:= var ->
    FetchedMacros = get_macros_by_name(MacroName, State0),
    State1 =
        maps:fold(fun(MacroArity, {_, UriLocation}, State00) ->
                          State01 = purge_macro({MacroName, MacroArity}, UriLocation, State00),
                          add_poi(Token, {macro, {MacroName, MacroArity}}, UriLocation, State01)
                  end, State0, FetchedMacros),
    {[], State1};
check_undef(Tokens, State) ->
    {Tokens, State}.

-spec purge_macro(MacroId :: macro_id(),
                  UriLocation :: uri_location() | undefined,
                  State :: #state{}) -> NewState :: #state{}.
purge_macro(MacroId, {Uri, _}, #state{uri = Uri, local_macros = LocalMacros0} = State0) ->
    State0#state{local_macros = maps:remove(MacroId, LocalMacros0)};
purge_macro(MacroId, {Uri, _}, #state{per_uri_macros = PerUriMacros0} = State0) ->
    PerUriMacros1 =
        lists:map(fun(UriMacros) ->
                          case UriMacros of
                              {Uri, Macros} ->
                                  {Uri, maps:remove(MacroId, Macros)};
                              _ ->
                                  UriMacros
                          end
                  end, PerUriMacros0),
    State0#state{per_uri_macros = PerUriMacros1};
purge_macro({MacroName, 0}, undefined, #state{predefined_macros = PredefinedMacros0} = State0) ->
    State0#state{predefined_macros = maps:remove(MacroName, PredefinedMacros0)}.

%%%-------------------------------------------------------------------
%%% Flow control
%%%-------------------------------------------------------------------
-define(DODGED_DEF_TAKE, '?dodged_def_take?').
-define(DODGED_DEF_SKIP, '?dodged_def_skip?').
-define(DODGED_DEF_TOGGLE, '?dodged_def_toggle?').
-define(DODGED_IF_TAKE, '?dodged_if_take?').
-define(DODGED_IF_SKIP, '?dodged_if_skip?').
-define(DODGED_ELIF_TAKE, '?dodged_elif_take?').
-define(DODGED_ELIF_SKIP, '?dodged_elif_skip?').
-define(DODGED_END, '?dodged_end?').

-spec dodge_flow_control(Tokens :: [erl_scan:token()],
                         State :: #state{}) -> {NewTokens :: [erl_scan:token()],
                                                NewState :: #state{}}.
dodge_flow_control([{'-', _}, {atom, _, Atom},
                    {'(', _}, {Type, Anno, MacroName} = Token, {')', _},
                    {dot, _}], State0)
  when (Atom =:= ifdef orelse Atom =:= ifndef)
       andalso
       (Type =:= atom orelse Type =:= var) ->
    FetchedMacros = get_macros_by_name(MacroName, State0),
    {if
         map_size(FetchedMacros) > 0 andalso Atom =:= ifdef
         orelse
         map_size(FetchedMacros) =:= 0 andalso Atom =:= ifndef ->
             [{atom, Anno, ?DODGED_DEF_TAKE}];
         true ->
             [{atom, Anno, ?DODGED_DEF_SKIP}]
     end,
     maps:fold(fun(MacroArity, {_, UriLocation}, State00) ->
                       add_poi(Token, {macro, {MacroName, MacroArity}}, UriLocation, State00)
               end, State0, FetchedMacros)};
dodge_flow_control([{'-', _}, {atom, Anno, else}, {dot, _}], State) ->
    {[{atom, Anno, ?DODGED_DEF_TOGGLE}], State};
dodge_flow_control([{'-', _}, {'if', Anno}, {'(', _} | T], State) ->
    case get_condition_value(T) of
        {ok, true} ->
            {[{atom, Anno, ?DODGED_IF_TAKE}], State};
        {ok, false} ->
            {[{atom, Anno, ?DODGED_IF_SKIP}], State};
        undefined ->
            {[], State}
    end;
dodge_flow_control([{'-', _}, {atom, Anno, elif}, {'(', _} | T], State) ->
    case get_condition_value(T) of
        {ok, true} ->
            {[{atom, Anno, ?DODGED_ELIF_TAKE}], State};
        {ok, false} ->
            {[{atom, Anno, ?DODGED_ELIF_SKIP}], State};
        undefined ->
            {[], State}
    end;
dodge_flow_control([{'-', _}, {atom, Anno, endif}, {dot, _}], State) ->
    {[{atom, Anno, ?DODGED_END}], State};
dodge_flow_control(Tokens, State) ->
    {Tokens, State}.

-spec get_condition_value(Tokens :: [erl_scan:token()]) -> {ok, boolean()} | undefined.
get_condition_value(Tokens) ->
    try
        Condition = read_condition(Tokens, []),
        {ok, Parsed} = erl_parse:parse_exprs(Condition),
        {value, Value, _} = erl_eval:exprs(Parsed, []),
        {ok, Value =:= true}
    catch
        _:_ ->
            undefined
    end.

-spec read_condition(Tokens :: [erl_scan:token()], Acc :: [erl_scan:token()]) ->
          Condition :: [erl_scan:token()].
read_condition([{')', _}, {dot, _} = Token | _], Acc) ->
    lists:reverse([Token | Acc]);
read_condition([Token | T], Acc) ->
    read_condition(T, [Token | Acc]).

-spec is_flow_control(Tokens :: [erl_scan:token()]) -> boolean().
is_flow_control([{atom, _, Atom}]) when Atom =:= ?DODGED_DEF_TAKE orelse
                                        Atom =:= ?DODGED_DEF_SKIP orelse
                                        Atom =:= ?DODGED_DEF_TOGGLE orelse
                                        Atom =:= ?DODGED_IF_TAKE orelse
                                        Atom =:= ?DODGED_IF_SKIP orelse
                                        Atom =:= ?DODGED_ELIF_TAKE orelse
                                        Atom =:= ?DODGED_ELIF_SKIP orelse
                                        Atom =:= ?DODGED_END ->
    true;
is_flow_control(_) ->
    false.

-spec apply_flow_control(Tokens :: [erl_scan:token()], State :: #state{}) -> NewState :: #state{}.
apply_flow_control([{atom, Anno, Atom}], #state{context = #context{data = Data}} = State0) ->
    if
        Atom =:= ?DODGED_DEF_TAKE orelse
        Atom =:= ?DODGED_DEF_SKIP orelse
        Atom =:= ?DODGED_IF_TAKE orelse
        Atom =:= ?DODGED_IF_SKIP ->
            apply_new_context({Atom, Anno}, State0);
        is_record(Data, define_data) ->
            apply_define_context({Atom, Anno}, State0);
        is_record(Data, condition_data) ->
            apply_condition_context({Atom, Anno}, State0);
        true ->
            State0
    end.

-spec apply_new_context({Atom :: atom(), Anno :: erl_anno:anno()}, State :: #state{}) -> #state{}.
apply_new_context({Atom, Anno}, #state{context = Context, parents = Parents0} = State0) ->
    #context{is_active = IsActive, is_parent_active = IsParentActive} = Context,
    NewContext = #context{data = if
                                     Atom =:= ?DODGED_DEF_TAKE orelse
                                     Atom =:= ?DODGED_DEF_SKIP ->
                                         #define_data{else_occurred = false};
                                     Atom =:= ?DODGED_IF_TAKE ->
                                         #condition_data{positive_occurred = true,
                                                         else_occurred = false};
                                     Atom =:= ?DODGED_IF_SKIP ->
                                         #condition_data{positive_occurred = false,
                                                         else_occurred = false}
                                 end,
                          is_active = if
                                          Atom =:= ?DODGED_DEF_TAKE orelse
                                          Atom =:= ?DODGED_IF_TAKE ->
                                              true;
                                          Atom =:= ?DODGED_DEF_SKIP orelse
                                          Atom =:= ?DODGED_IF_SKIP ->
                                              false
                                      end,
                          is_parent_active = IsActive andalso IsParentActive},
    State1 = State0#state{context = NewContext, parents = [Context | Parents0]},
    check_context_disabled_from(Anno, State1).

-spec apply_define_context({Atom :: atom(), Anno :: erl_anno:anno()}, State :: #state{}) ->
          NewState :: #state{}.
apply_define_context({Atom, Anno}, #state{context = Context0, parents = Parents0} = State0) ->
    #context{data = Data0, is_active = IsActive} = Context0,
    #define_data{else_occurred = ElseOccurred} = Data0,
    case IsActive of
        true ->
            if
                Atom =:= ?DODGED_DEF_TOGGLE andalso not ElseOccurred ->
                    Data1 = Data0#define_data{else_occurred = true},
                    Context1 = Context0#context{data = Data1, is_active = false},
                    State1 = State0#state{context = Context1},
                    check_context_disabled_from(Anno, State1);
                Atom =:= ?DODGED_END ->
                    [PrevContext | Parents1] = Parents0,
                    State0#state{context = PrevContext, parents = Parents1};
                true ->
                    State0
            end;
        false ->
            if
                Atom =:= ?DODGED_DEF_TOGGLE andalso not ElseOccurred ->
                    Data1 = Data0#define_data{else_occurred = true},
                    Context1 = Context0#context{data = Data1, is_active = true},
                    State1 = State0#state{context = Context1},
                    check_context_disabled_to(Anno, State1);
                Atom =:= ?DODGED_END ->
                    State1 = check_context_disabled_to(Anno, State0),
                    #state{parents = [PrevContext | Parents2]} = State1,
                    State1#state{context = PrevContext, parents = Parents2};
                true ->
                    State0
            end
    end.

-spec apply_condition_context({Atom :: atom(), Anno :: erl_anno:anno()}, State :: #state{}) ->
          NewState :: #state{}.
apply_condition_context({Atom, Anno}, #state{context = Context0, parents = Parents0} = State0) ->
    #context{data = Data0, is_active = IsActive} = Context0,
    #condition_data{positive_occurred = PositiveOccurred, else_occurred = ElseOccurred} = Data0,
    case IsActive of
        true ->
            if
                Atom =:= ?DODGED_ELIF_TAKE orelse
                Atom =:= ?DODGED_ELIF_SKIP ->
                    Context1 = Context0#context{is_active = false},
                    State1 = State0#state{context = Context1},
                    check_context_disabled_from(Anno, State1);
                not ElseOccurred andalso
                Atom =:= ?DODGED_DEF_TOGGLE ->
                    Data1 = Data0#condition_data{else_occurred = true},
                    Context1 = Context0#context{data = Data1},
                    State1 = State0#state{context = Context1},
                    apply_condition_context({?DODGED_ELIF_SKIP, Anno}, State1);
                Atom =:= ?DODGED_END ->
                    [PrevContext | Parents1] = Parents0,
                    State0#state{context = PrevContext, parents = Parents1};
                true ->
                    State0
            end;
        false ->
            if
                not PositiveOccurred andalso Atom =:= ?DODGED_ELIF_TAKE ->
                    Data1 = Data0#condition_data{positive_occurred = true},
                    Context1 = Context0#context{data = Data1, is_active = true},
                    State1 = State0#state{context = Context1},
                    check_context_disabled_to(Anno, State1);
                not PositiveOccurred andalso
                not ElseOccurred andalso
                Atom =:= ?DODGED_DEF_TOGGLE ->
                    Data1 = Data0#condition_data{else_occurred = true},
                    Context1 = Context0#context{data = Data1},
                    State1 = State0#state{context = Context1},
                    apply_condition_context({?DODGED_ELIF_TAKE, Anno}, State1);
                Atom =:= ?DODGED_ELIF_TAKE orelse
                Atom =:= ?DODGED_ELIF_SKIP ->
                    State1 = check_context_disabled_to(Anno, State0),
                    check_context_disabled_from(Anno, State1);
                not ElseOccurred andalso
                Atom =:= ?DODGED_DEF_TOGGLE ->
                    Data1 = Data0#condition_data{else_occurred = true},
                    Context1 = Context0#context{data = Data1},
                    State1 = State0#state{context = Context1},
                    apply_condition_context({?DODGED_ELIF_SKIP, Anno}, State1);
                Atom =:= ?DODGED_END ->
                    State1 = check_context_disabled_to(Anno, State0),
                    #state{parents = [PrevContext | Parents2]} = State1,
                    State1#state{context = PrevContext, parents = Parents2};
                true ->
                    State0
            end
    end.

-spec check_context_disabled_from(Anno :: erl_anno:anno(), State :: #state{}) -> #state{}.
check_context_disabled_from(Anno, #state{context = Context0} = State0) ->
    case Context0 of
        #context{is_active = false, is_parent_active = true, disabled_from = undefined} ->
            {Line, _} = esrv_parser_lib:get_anno_location(Anno),
            DisabledFrom = Line + 1,
            State0#state{context = Context0#context{disabled_from = DisabledFrom}};
        _ ->
            State0
    end.

-spec check_context_disabled_to(Anno :: erl_anno:anno(), State :: #state{}) -> #state{}.
check_context_disabled_to(Anno, #state{grey_ranges = GreyRanges0, context = Context0} = State0) ->
    case Context0 of
        #context{disabled_from = DisabledFrom} when is_integer(DisabledFrom) ->
            {Line, _} = esrv_parser_lib:get_anno_location(Anno),
            DisabledTo = Line - 1,
            State0#state{grey_ranges = if
                                           DisabledTo >= DisabledFrom ->
                                               [{DisabledFrom, DisabledTo} | GreyRanges0];
                                           true ->
                                               GreyRanges0
                                       end,
                         context = Context0#context{disabled_from = undefined}};
        _ ->
            State0
    end.

-spec is_context_active(State :: #state{}) -> boolean().
is_context_active(#state{context = #context{is_active = true, is_parent_active = true}}) ->
    true;
is_context_active(_) ->
    false.

%%%-------------------------------------------------------------------
%%% Include
%%%-------------------------------------------------------------------
-spec check_include(Tokens :: [token()],
                    State :: #state{}) -> {NewTokens :: [token()],
                                           NewState :: #state{}}.
check_include([{'-', _}, {atom, _, Atom},
               {'(', _}, {string, Anno, Include}, {')', _},
               {dot, _}], #state{include_data = IncludeData0} = State0)
  when Atom =:= include orelse Atom =:= include_lib ->
    case get_included_uri(Atom, Include, State0) of
        {ok, Uri} ->
            Line = erl_anno:line(Anno),
            State1 = apply_included_uri(Line, Uri, State0),
            State2 = add_poi({string, Anno, Include}, {Atom, Include}, {Uri, {1, 1}}, State1),
            {[], State2};
        undefined ->
            IncludeData1 =
                case IncludeData0 of
                    #include_data{unresolved = Unresolved0} when Atom =:= include ->
                        IncludeData0#include_data{unresolved = [Include | Unresolved0]};
                    #include_data{unresolved_libs = UnresolvedLibs0} when Atom =:= include_lib ->
                        IncludeData0#include_data{unresolved_libs = [Include | UnresolvedLibs0]}
                end,
            State1 = State0#state{include_data = IncludeData1},
            State2 = add_poi({string, Anno, Include}, {Atom, Include}, undefined, State1),
            {[], State2}
    end;
check_include(Tokens, State) ->
    {Tokens, State}.

-spec get_included_uri(IncludeType :: include | include_lib,
                       Include :: string(),
                       State :: #state{}) -> {ok, uri()} | undefined.
get_included_uri(IncludeType, Include, #state{uri = Uri,
                                              app_path = AppPath,
                                              module_type = ModuleType}) ->
    if
        IncludeType =:= include ->
            esrv_lib:find_included_uri(Uri, AppPath, ModuleType, Include);
        IncludeType =:= include_lib ->
            esrv_lib:find_included_lib_uri(Include)
    end.

-spec apply_included_uri(Line :: line(), IncludedUri :: uri(), State :: #state{}) -> #state{}.
apply_included_uri(Line, IncludedUri, #state{include_chain = IncludeChain} = State0) ->
    case lists:member(IncludedUri, IncludeChain) of
        false ->
            ModuleData = get_module_data(IncludedUri, IncludeChain),
            apply_module_data(Line, IncludedUri, ModuleData, State0);
        true ->
            State0
    end.

-spec apply_module_data(Line :: line(),
                        Uri :: uri(),
                        Moduledata:: module_data(),
                        State :: #state{}) -> #state{}.
apply_module_data(Line, Uri, ModuleData, #state{per_uri_macros = PerUriMacros0,
                                                include_data = IncludeData0} = State0) ->
    case IncludeData0 of
        #include_data{resolved = Resolved0} when not is_map_key(Uri, Resolved0) ->
            {Macros, IncludedUris} = get_included_module_info(ModuleData),
            Resolved1 = Resolved0#{Uri => #resolved_include{line = Line}},
            State1 = State0#state{per_uri_macros = case maps:size(Macros) of
                                                       Size when Size > 0 ->
                                                           [{Uri, Macros} | PerUriMacros0];
                                                       0 ->
                                                           PerUriMacros0
                                                   end,
                                  include_data = IncludeData0#include_data{resolved = Resolved1}},
            lists:foldl(fun(IncludedUri, State10) ->
                                apply_included_uri(Line, IncludedUri, State10)
                        end, State1, IncludedUris);
        _ ->
            State0
    end.

-spec get_included_module_info(ModuleData :: module_data()) -> {macros(), [uri()]}.
get_included_module_info(#module_data{include_data = #include_data{resolved = Resolved},
                                      macros = Macros}) ->
    IncludedUris =
        lists:filter(fun(Uri) ->
                             Path = esrv_lib:uri_to_path(Uri),
                             filelib:is_regular(Path)
                     end, maps:keys(Resolved)),
    {Macros, IncludedUris}.

-spec get_module_data(Uri :: uri(), IncludeChain :: [uri()]) -> module_data().
get_module_data(Uri, IncludeChain) ->
    case esrv_db:get_module_data(Uri) of
        {ok, ModuleData} ->
            ModuleData;
        undefined ->
            Path = esrv_lib:uri_to_path(Uri),
            {ok, Content} = file:read_file(Path),
            Hash = crypto:hash(md5, Content),
            {AppPath, ModuleType} = esrv_lib:app_path_and_module_type(Uri),
            ParsedModuleData = esrv_parser:parse(Uri, AppPath, ModuleType, Content, IncludeChain),
            ok = esrv_db:write_parsed_module(Uri, Hash, ParsedModuleData),
            ok = esrv_db:write_module_meta(Uri, Hash, ModuleType, ParsedModuleData),
            ParsedModuleData
    end.

%%%-------------------------------------------------------------------
%%% Pois
%%%-------------------------------------------------------------------
-spec check_pois(StateData :: state_data()) -> state_data().
check_pois({Scanned, #state{pois = Pois0} = State0}) ->
    Pois1 =
        lists:foldl(fun(#tokens_data{tokens = Tokens}, Pois00) ->
                            check_pois(Tokens, Pois00)
                    end, Pois0, Scanned),
    {Scanned, State0#state{pois = Pois1}}.

-spec check_pois(Tokens :: [token()], Pois :: pois()) -> pois().
check_pois([], Pois) ->
    Pois;
check_pois([{'fun', _}, {atom, _, ModuleName} = ModuleToken, {':', _},
            {atom, _, FunctionName} = FunctionToken, {'/', _},
            {integer, _, Arity} | T], Pois0) ->
    Pois1 = esrv_parser_lib:add_token_poi(ModuleToken,
                                          {module, ModuleName},
                                          undefined, Pois0),
    Pois2 = esrv_parser_lib:add_token_poi(FunctionToken,
                                          {remote_function, ModuleName, {FunctionName, Arity}},
                                          undefined, Pois1),
    check_pois(T, Pois2);
check_pois([{'fun', _},
            {atom, _, FunctionName} = FunctionToken, {'/', _},
            {integer, _, Arity} | T], Pois0) ->
    Pois1 = esrv_parser_lib:add_token_poi(FunctionToken,
                                          {local_function, {FunctionName, Arity}},
                                          undefined, Pois0),
    check_pois(T, Pois1);
check_pois([{'#', _}, {atom, _, RecordName} = Token, {Symbol, _} | T], Pois0)
  when Symbol =:= '{' orelse Symbol =:= '.' orelse Symbol =:= dot ->
    Pois1 = esrv_parser_lib:add_token_poi(Token, {record, RecordName}, undefined, Pois0),
    check_pois(T, Pois1);
check_pois([?UNRESOLVED_MACRO_TOKEN(MarcroArgsTokens) | T], Pois0) ->
    Pois1 = lists:foldl(fun check_pois/2, Pois0, MarcroArgsTokens),
    check_pois(T, Pois1);
check_pois([_ | T], Pois) ->
    check_pois(T, Pois).

%%%-------------------------------------------------------------------
%%% Tokens
%%%-------------------------------------------------------------------
-spec check_tokens(StateData :: state_data()) -> NewStateData :: state_data().
check_tokens({Scanned0, State0}) ->
    {Scanned1, State1} =
        lists:foldl(fun(#tokens_data{tokens = Tokens00} = TokensData00, {Acc, State00}) ->
                            Zones =
                                case esrv_zone_parser:zone_begin(Tokens00) of
                                    {ok, OpenedZone, ClosedZones, _, _} ->
                                        Reversed = lists:reverse(Tokens00),
                                        [esrv_zone_parser:zone_end(OpenedZone, Reversed) |
                                         ClosedZones];
                                    undefined ->
                                        []
                                end,
                            {Tokens01, State01} = check_attribute(Tokens00, State00),
                            {[TokensData00#tokens_data{tokens = Tokens01, zones = Zones} | Acc],
                             State01}
                    end, {[], State0}, Scanned0),
    {lists:reverse(Scanned1), State1}.

-spec check_attribute(Tokens :: [token()],
                      State :: #state{}) -> {NewTokens :: [token()],
                                             NewState :: #state{}}.
check_attribute([{'-', _}, {atom, _, module},
                 {atom, _, ModuleName} = Token | _] = Tokens, State0) ->
    State1 = add_poi(Token, {module, ModuleName}, State0),
    {Tokens, State1};
check_attribute([{'-', _}, {atom, _, module},
                 {'(', _}, {atom, _, ModuleName} = Token | _] = Tokens, State0) ->
    State1 = add_poi(Token, {module, ModuleName}, State0),
    {Tokens, State1};
check_attribute([{'-', _}, {atom, _, Atom},
                 {atom, _, Behavior} = Token | _] = Tokens, State0)
  when Atom =:= behavior orelse Atom =:= behaviour ->
    State1 = add_poi(Token, {behavior, Behavior}, undefined, State0),
    {Tokens, State1};
check_attribute([{'-', _}, {atom, _, Atom},
                 {'(', _}, {atom, _, Behavior} = Token | _] = Tokens, State0)
  when Atom =:= behavior orelse Atom =:= behaviour ->
    State1 = add_poi(Token, {behavior, Behavior}, undefined, State0),
    {Tokens, State1};
check_attribute([{'-', _}, {atom, _, callback},
                 {atom, _, FunctionName} = Token, {'(', _} | T] = Tokens, State0) ->
    State1 =
        case read_spec_arity(T) of
            {ok, Arity} ->
                add_poi(Token, {callback, {FunctionName, Arity}}, State0);
            undefined ->
                State0
        end,
    {Tokens, State1};
check_attribute([{'-', _}, {atom, _, Atom},
                 {atom, _, TypeName} = Token, {'(', _} | T] = Tokens, State0)
  when Atom =:= type orelse Atom =:= opaque ->
    State1 =
        case read_type_arity(T) of
            {ok, Arity} ->
                add_poi(Token, {local_type, {TypeName, Arity}}, State0);
            undefined ->
                State0
        end,
    {Tokens, State1};
check_attribute([{'-', _}, {atom, _, record}, {'(', _},
                 {atom, _, RecordName} = Token | _] = Tokens, State0) ->
    State1 = add_poi(Token, {record, RecordName}, State0),
    {Tokens, State1};
check_attribute([{'-', _}, {atom, _, export}, {'(', _}, {'[', _} | _] = Tokens, State0) ->
    parse_export(function, Tokens, State0);
check_attribute([{'-', _}, {atom, _, export_type}, {'(', _}, {'[', _} | _] = Tokens, State0) ->
    parse_export(type, Tokens, State0);
check_attribute([{'-', _}, {atom, _, import}, {'(', _},
                 {atom, _, ModuleName} = Token, {')', _}, {dot, _}],
                #state{import = Import0} = State0) ->
    State1 = State0#state{import = [ModuleName | Import0]},
    State2 = add_poi(Token, {module, ModuleName}, undefined, State1),
    {[], State2};
check_attribute([{'-', _}, {atom, _, import}, {'(', _},
                 {atom, _, ModuleName} = Token, {',', _}, {'[', _} | T] = Tokens, State0) ->
    State1 = add_poi(Token, {module, ModuleName}, undefined, State0),
    case esrv_parser_lib:parse_name_arity_tokens(T) of
        {ok, Parsed} ->
            State2 = lists:foldl(fun(ParsedNameArity, State10) ->
                                         merge_import(ModuleName, ParsedNameArity, State10)
                                 end, State1, Parsed),
            {[], State2};
        undefined ->
            {Tokens, State1}
    end;
check_attribute([{'-', _}, {atom, _, spec},
                 {atom, _, FunctionName} = Token, {'(', _} | T] = Tokens, State0) ->
    State1 =
        case read_spec_arity(T) of
            {ok, Arity} ->
                add_poi(Token, {local_spec, {FunctionName, Arity}}, undefined, State0);
            undefined ->
                State0
        end,
    {Tokens, State1};
check_attribute([{'-', _}, {atom, _, spec}, {atom, _, ModuleName} = ModuleToken, {':', _},
                 {atom, _, FunctionName} = FunctionToken, {'(', _} | T] = Tokens, State0) ->
    State1 = add_poi(ModuleToken, {module, ModuleName}, undefined, State0),
    State2 =
        case read_spec_arity(T) of
            {ok, Arity} ->
                PoiData = {remote_spec, ModuleName, {FunctionName, Arity}},
                add_poi(FunctionToken, PoiData, undefined, State1);
            undefined ->
                State1
        end,
    {Tokens, State2};
check_attribute(Tokens, State) ->
    {Tokens, State}.

-spec read_type_arity(Tokens :: [token()]) -> {ok, arity()} | undefined.
read_type_arity([{')', _} | _]) ->
    {ok, 0};
read_type_arity(Tokens) ->
    read_type_arity2(Tokens, 0).

-spec read_type_arity2(Tokens :: [token()], Acc :: non_neg_integer()) -> {ok, arity()} | undefined.
read_type_arity2([], _) ->
    undefined;
read_type_arity2([{var, _, _}, {')', _} | _], Acc) ->
    {ok, Acc + 1};
read_type_arity2([{var, _, _}, {',', _} | T], Acc) ->
    read_type_arity2(T, Acc + 1);
read_type_arity2([_ | T], Acc) ->
    read_type_arity2(T, Acc).

-spec parse_export(ExportKind :: export_kind(),
                   Tokens :: [token()],
                   State :: #state{}) -> {NewTokens :: [token()],
                                          NewState :: #state{}}.
parse_export(ExportKind, [_, _, _, _ | T] = Tokens, State0) ->
    case esrv_parser_lib:parse_name_arity_tokens(T) of
        {ok, Parsed} ->
            State1 = lists:foldl(fun(ParsedNameArity, State00) ->
                                         merge_export(ExportKind, ParsedNameArity, State00)
                                 end, State0, Parsed),
            {[], State1};
        undefined ->
            {Tokens, State0}
    end.

-spec merge_export(ExportKind :: export_kind(),
                   ParsedNameArity :: parsed_name_arity(),
                   State :: #state{}) -> NewState :: #state{}.
merge_export(function, {NameArity, Token}, #state{export = Export0} = State0) ->
    State1 = State0#state{export = [NameArity | Export0]},
    add_poi(Token, {local_function, NameArity}, undefined, State1);
merge_export(type, {NameArity, Token}, #state{export_type = ExportType0} = State0) ->
    State1 = State0#state{export_type = [NameArity | ExportType0]},
    add_poi(Token, {local_type, NameArity}, undefined, State1).

-spec merge_import(ModuleName :: module(),
                   ParsedNameArity :: parsed_name_arity(),
                   State :: #state{}) -> NewState :: #state{}.
merge_import(ModuleName, {NameArity, Token}, #state{import = Import0} = State0) ->
    State1 = State0#state{import = [{ModuleName, NameArity} | Import0]},
    add_poi(Token, {remote_function, ModuleName, NameArity}, undefined, State1).

-spec read_spec_arity(Tokens :: [token()]) -> {ok, arity()} | undefined.
read_spec_arity([{')', _} | _]) ->
    {ok, 0};
read_spec_arity(Tokens) ->
    read_spec_arity2(Tokens, [], 0).

-spec read_spec_arity2(Tokens :: [erl_scan:token()],
                       InnerStack :: [token()],
                       Acc :: non_neg_integer()) -> {ok, arity()} | undefined.
read_spec_arity2([], _, _) ->
    undefined;
read_spec_arity2([{')', _} | _], [], Acc) ->
    {ok, Acc + 1};
read_spec_arity2([{',', _} | T], [], Acc) ->
    read_spec_arity2(T, [], Acc + 1);
read_spec_arity2([{'(', _} = Token | T], InnerStack, Acc) ->
    read_spec_arity2(T, [Token | InnerStack], Acc);
read_spec_arity2([{')', _} | T], [{'(', _} | InnerStack], Acc) ->
    read_spec_arity2(T, InnerStack, Acc);
read_spec_arity2([{'[', _} = Token | T], InnerStack, Acc) ->
    read_spec_arity2(T, [Token | InnerStack], Acc);
read_spec_arity2([{']', _} | T], [{'[', _} | InnerStack], Acc) ->
    read_spec_arity2(T, InnerStack, Acc);
read_spec_arity2([{'{', _} = Token | T], InnerStack, Acc) ->
    read_spec_arity2(T, [Token | InnerStack], Acc);
read_spec_arity2([{'}', _} | T], [{'{', _} | InnerStack], Acc) ->
    read_spec_arity2(T, InnerStack, Acc);
read_spec_arity2([{'<<', _} = Token | T], InnerStack, Acc) ->
    read_spec_arity2(T, [Token | InnerStack], Acc);
read_spec_arity2([{'>>', _} | T], [{'<<', _} | InnerStack], Acc) ->
    read_spec_arity2(T, InnerStack, Acc);
read_spec_arity2([_ | T], InnerStack, Acc) ->
    read_spec_arity2(T, InnerStack, Acc).

%%%-------------------------------------------------------------------
%%% Finalize
%%%-------------------------------------------------------------------
-spec finalize_data(StateData :: state_data()) -> tokens_info().
finalize_data({Scanned, #state{local_macros = LocalMacros,
                               include_data = IncludeData,
                               export = Export,
                               export_type = ExportType,
                               import = Import,
                               grey_ranges = GreyRanges,
                               pois = Pois}}) ->
    #tokens_info{scanned = Scanned,
                 macros = LocalMacros,
                 include_data = IncludeData,
                 export = Export,
                 export_type = ExportType,
                 import = Import,
                 grey_ranges = GreyRanges,
                 pois = Pois}.

%%%-------------------------------------------------------------------
%%% Misc
%%%-------------------------------------------------------------------
-spec get_macros_by_name(MacroName :: name(), State :: #state{}) -> fetched_macros().
get_macros_by_name(MacroName, #state{uri = Uri,
                                     local_macros = LocalMacros,
                                     per_uri_macros = PerUriMacros,
                                     predefined_macros = PredefinedMacros}) ->
    FetchedMacros0 =
        case maps:get(MacroName, PredefinedMacros, undefined) of
            Tokens when Tokens =/= undefined ->
                #{undefined => {Tokens, undefined}};
            undefined ->
                #{}
        end,
    FetchedMacros1 = find_macro_by_name(MacroName, PerUriMacros, FetchedMacros0),
    find_macro_by_name(MacroName, [{Uri, LocalMacros}], FetchedMacros1).

-spec find_macro_by_name(MacroName :: name(),
                         PerUriMacros :: [{uri(), macros()}],
                         FetchedMacros :: fetched_macros()) -> fetched_macros().
find_macro_by_name(_, [], FetchedMacros) ->
    FetchedMacros;
find_macro_by_name(MacroName, [{Uri, Macros} | T], FetchedMacros0) ->
    FetchedMacros1 =
        maps:fold(fun(MacroId, MacroDefinition, FetchedMacros00) ->
                          case MacroId of
                              {MacroName, MacroArity} ->
                                  #macro_definition{tokens = Tokens,
                                                    location = Location} = MacroDefinition,
                                  FetchedMacros00#{MacroArity => {Tokens, {Uri, Location}}};
                              _ ->
                                  FetchedMacros00
                          end
                  end, FetchedMacros0, Macros),
    find_macro_by_name(MacroName, T, FetchedMacros1).

-spec add_poi(Token :: token(),
              PoiData :: poi_data(),
              State :: #state{}) -> NewState :: #state{}.
add_poi(?UNRESOLVED_MACRO_TOKEN(_), _, State) ->
    State;
add_poi(Token, PoiData, State) ->
    Location = esrv_parser_lib:get_token_location(Token),
    add_poi2(Token, PoiData, {local, Location}, State).

-spec add_poi(Token :: erl_scan:token(),
              PoiData :: poi_data(),
              UriLocation :: uri_location() | undefined,
              State :: #state{}) -> NewState :: #state{}.
add_poi(Token, PoiData, {Uri, Location}, #state{uri = Uri} = State) ->
    add_poi2(Token, PoiData, {local, Location}, State);
add_poi(Token, PoiData, {Uri, Location}, State) ->
    add_poi2(Token, PoiData, {remote, Uri, Location}, State);
add_poi(Token, PoiData, undefined, State) ->
    add_poi2(Token, PoiData, undefined, State).

-spec add_poi2(Token :: erl_scan:token(),
               PoiData :: poi_data(),
               PoiDefinition :: poi_definition() | undefined,
               State :: #state{}) -> NewState :: #state{}.
add_poi2(Token, PoiData, PoiDefinition, #state{pois = Pois0} = State0) ->
    State0#state{pois = esrv_parser_lib:add_token_poi(Token, PoiData, PoiDefinition, Pois0)}.
