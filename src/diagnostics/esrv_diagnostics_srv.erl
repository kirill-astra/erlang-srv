-module(esrv_diagnostics_srv).

-behaviour(gen_statem).

-include("types.hrl").
-include("records.hrl").
-include("diagnostics.hrl").
-include("log.hrl").

%% API
-export([start_link/0,
         register_diagnostic_modules/1,
         request/2,
         result/3,
         add_dependency/2]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([waiting_for_modules/3,
         active/3]).

-define(SERVER, ?MODULE).

-record(uri_data, {diagnostics :: [diagnostic()],
                   notify_required :: boolean(),
                   total_workers :: pos_integer(),
                   completed_workers :: non_neg_integer(),
                   workers :: [pid()],
                   token :: lp_token() | undefined}).

-type diagnostic_module() :: {module(), map()}.

-record(dependency, {dependent :: sets:set(uri()),
                     post_diagnostics :: boolean()}).

-type dependencies() :: #{module() => #dependency{}}.

-record(data, {diagnostic_modules :: [diagnostic_module()],
               uris :: #{uri() => #uri_data{}},
               dependencies :: dependencies()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec register_diagnostic_modules(DiagnosticModules :: [{module(), map()}]) -> ok.
register_diagnostic_modules(DiagnosticModules) ->
    gen_statem:cast(?SERVER, {register_diagnostic_modules, DiagnosticModules}).

-spec request(Uri :: uri(), AppId :: app_id()) -> ok.
request(Uri, AppId) ->
    gen_statem:cast(?SERVER, {request, Uri, AppId}).

-spec result(Worker :: pid(), Uri :: uri(), Diagnostics :: [diagnostic()]) -> ok.
result(Worker, Uri, Diagnostics) ->
    gen_statem:cast(?SERVER, {result, Worker, Uri, Diagnostics}).

-spec add_dependency(ModuleName :: module(), Uri :: uri()) -> ok.
add_dependency(ModuleName, Uri) ->
    gen_statem:cast(?SERVER, {add_dependency, ModuleName, Uri}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> gen_statem:init_result(atom()).
init([]) ->
    {ok, waiting_for_modules, #data{diagnostic_modules = [],
                                    uris = #{},
                                    dependencies = #{}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one function like this for each state name.
%% Whenever a gen_statem receives an event, the function
%% with the name of the current state (StateName)
%% is called to handle the event.
%% @end
%%--------------------------------------------------------------------
-spec waiting_for_modules('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
                         (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
waiting_for_modules(cast, {register_diagnostic_modules, DiagnosticModules},  Data0) ->
    ?LOG_INFO("Registered diagnostic modules: ~p", [[ M || {M, _} <- DiagnosticModules ]]),
    Data1 = Data0#data{diagnostic_modules = DiagnosticModules},
    {next_state, active, Data1, [{next_event, internal, init_modules}]};

waiting_for_modules(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, waiting_for_modules, Data).

-spec active('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
            (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
active(internal, init_modules, #data{diagnostic_modules = DiagnosticModules0} = Data0) ->
    DiagnosticModules1 =
        lists:filtermap(fun({Module, Options0}) ->
                                try Module:init(Options0) of
                                    Options1 ->
                                        {true, {Module, Options1}}
                                catch
                                    T:E:S ->
                                        case {T, E, S} of
                                            {error, undef, [{Module, init, _, _} | _]} ->
                                                {true, {Module, Options0}};
                                            _ ->
                                                ?LOG_WARNING("Diagnostic by '~s' init exception; "
                                                             "type: ~p; error: ~p; stacktrace: ~p",
                                                             [Module, T, E, S]),
                                                false
                                        end
                                end
                        end, DiagnosticModules0),
    {keep_state, Data0#data{diagnostic_modules = DiagnosticModules1}};

active(internal, {diagnose, Uri, AppId, PostDiagnostic},
       #data{diagnostic_modules = DiagnosticModules}) ->
    case DiagnosticModules of
        [_|_] ->
            {keep_state_and_data,
             [{next_event, internal, {apply_post_diagnostic, Uri, PostDiagnostic}},
              {next_event, internal, {cancel_diagnostic, Uri}},
              {next_event, internal, {run_diagnostic, Uri, AppId}}]};
        [] ->
            keep_state_and_data
    end;

active(internal, {diagnose_dependent, Dependent0}, _) ->
    Dependent1 = sets:filter(fun(U) -> esrv_db:read_text_document(U) =/= [] end, Dependent0),
    {keep_state_and_data,
     sets:fold(fun(U, Acc) ->
                       AppId = esrv_lib:get_app_id(U),
                       [{next_event, internal, {diagnose, U, AppId, false}} | Acc]
               end, [], Dependent1)};

active(internal, {apply_post_diagnostic, Uri, PostDiagnostic0},
       #data{dependencies = Dependencies0} = Data0) ->
    PostDiagnostic1 = PostDiagnostic0 orelse get_post_diagnostic(Uri, Dependencies0),
    Dependencies1 = set_post_diagnostic(Uri, PostDiagnostic1, Dependencies0),
    {keep_state, Data0#data{dependencies = Dependencies1}};

active(internal, {cancel_diagnostic, Uri}, #data{uris = Uris0} = Data0) ->
    case maps:take(Uri, Uris0) of
        {#uri_data{workers = Wokers, token = Token}, Uris1} ->
            ok = esrv_progress_srv:finalize(Token, undefined),
            lists:foreach(fun(Worker) ->
                                  esrv_diagnostic_worker_sup:stop_worker(Worker)
                          end, Wokers),
            {keep_state, Data0#data{uris = Uris1}};
        error ->
            keep_state_and_data
    end;

active(internal, {run_diagnostic, Uri, AppId},
       #data{diagnostic_modules = DiagnosticModules, uris = Uris0} = Data0) ->
    ok = send_notification(Uri, []),
    Workers =
        lists:foldl(fun({Module, Options}, Acc) ->
                            {ok, Worker} = esrv_diagnostic_worker_sup:start_worker(Module, Options),
                            ok = esrv_diagnostic_worker:run(Worker, Uri, AppId),
                            [Worker | Acc]
                    end, [], DiagnosticModules),
    UriData =
        #uri_data{diagnostics = [],
                  notify_required = false,
                  total_workers = length(Workers),
                  completed_workers = 0,
                  workers = Workers},
    Uris1 = Uris0#{Uri => apply_progress(Uri, UriData)},
    {keep_state, Data0#data{uris = Uris1}};

active(cast, {result, Worker, Uri, Diagnostics}, #data{uris = Uris0} = Data0) ->
    {UriData0, Uris1} = maps:take(Uri, Uris0),
    case apply_result(Worker, Diagnostics, UriData0) of
        {ok, UriData1} ->
            UriData2 = apply_notifications(Uri, UriData1),
            UriData3 = apply_progress(Uri, UriData2),
            case UriData3 of
                #uri_data{total_workers = TotalWorkers, completed_workers = CompletedWorkers}
                  when CompletedWorkers < TotalWorkers ->
                    {keep_state, Data0#data{uris = Uris1#{Uri => UriData3}}};
                #uri_data{total_workers = TotalWorkers, completed_workers = TotalWorkers} ->
                    {keep_state, Data0#data{uris = Uris1},
                     [{next_event, internal, {post_diagnostic, Uri}}]}
            end;
        skip ->
            keep_state_and_data
    end;

active(internal, {post_diagnostic, Uri}, #data{dependencies = Dependencies0} = Data0) ->
    case filename:extension(Uri) of
        <<".hrl">> ->
            Dependent = dependent_on_header(Uri),
            {keep_state_and_data, [{next_event, internal, {diagnose_dependent, Dependent}}]};
        <<".erl">> ->
            {ok, Module} = get_module_name(Uri),
            case maps:find(Module, Dependencies0) of
                {ok, #dependency{dependent = Dependent, post_diagnostics = true}} ->
                    Data1 = Data0#data{dependencies = maps:remove(Module, Dependencies0)},
                    {keep_state, Data1, [{next_event, internal, {diagnose_dependent, Dependent}}]};
                _ ->
                    keep_state_and_data
            end;
        _ ->
            keep_state_and_data
    end;

active(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, active, Data).

-spec handle_event('enter', OldState :: term(), State :: term(), Data :: term()) ->
          gen_statem:state_enter_result(term());
                  (gen_statem:event_type(), Msg :: term(), State :: term(), Data :: term()) ->
          gen_statem:event_handler_result(term()).
handle_event(cast, {request, Uri, AppId}, _, _) ->
    {keep_state_and_data, [{next_event, internal, {diagnose, Uri, AppId, true}}]};

handle_event(cast, {add_dependency, ModuleName, Uri}, _,
             #data{dependencies = Dependencies0} = Data0) ->
    Dependencies1 =
        maps:update_with(ModuleName,
                         fun(#dependency{dependent = Dependent0} = Dependency0) ->
                                 Dependent1 = sets:add_element(Uri, Dependent0),
                                 Dependency0#dependency{dependent = Dependent1}
                         end,
                         #dependency{dependent = sets:from_list([Uri]), post_diagnostics = false},
                         Dependencies0),
    Data1 = Data0#data{dependencies = Dependencies1},
    {keep_state, Data1};

handle_event(EventType, EventContent, State, _) ->
    ?LOG_ERROR("Module: ~p; not implemented event in ~p state; type: ~p; content: ~p",
               [?MODULE, State, EventType, EventContent]),
    {stop, not_implemented}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) -> any().
terminate(_Reason, _State, _Data) ->
    void.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  OldState :: term(),
                  OldData :: term(),
                  Extra :: term()) ->
          {ok, NewState :: term(), NewData :: term()} | (Reason :: term()).
code_change(_, OldState, OldData, _) ->
    {ok, OldState, OldData}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec apply_result(Worker :: pid(), Diagnostics :: [diagnostic()], UriData :: #uri_data{}) ->
          {ok, #uri_data{}} | skip.
apply_result(Worker, Diagnostics, #uri_data{diagnostics = Diagnostics0,
                                            completed_workers = CompletedWorkers0,
                                            workers = Wokers0} = UriData0) ->
    case lists:member(Worker, Wokers0) of
        true ->
            {ok, UriData0#uri_data{diagnostics = Diagnostics ++ Diagnostics0,
                                   notify_required = length(Diagnostics) > 0,
                                   completed_workers = CompletedWorkers0 + 1,
                                   workers = lists:delete(Worker, Wokers0)}};
        false ->
            skip
    end.

-spec apply_notifications(Uri :: uri(), UriData :: #uri_data{}) -> #uri_data{}.
apply_notifications(Uri, #uri_data{diagnostics = Diagnostics0,
                                   notify_required = true} = UriData0) ->
    LinesToResolve =
        lists:foldl(fun(#diagnostic{position = {line, Line}}, Acc) ->
                            [Line | Acc];
                       (_, Acc) ->
                            Acc
                    end, [], Diagnostics0),
    Diagnostics1 =
        case LinesToResolve of
            [_|_] ->
                ResolvePositions = resolve_positions(Uri, LinesToResolve),
                lists:map(fun(#diagnostic{position = {line, Line}} = Diagnostic0) ->
                                  Range = maps:get(Line, ResolvePositions, {{Line, 1}, {Line, 1}}),
                                  Diagnostic0#diagnostic{position = {range, Range}};
                             (Diagnostic) ->
                                  Diagnostic
                          end, Diagnostics0);
            [] ->
                Diagnostics0
        end,
    ok = send_notification(Uri, Diagnostics1),
    UriData0#uri_data{notify_required = false};
apply_notifications(_, UriData) ->
    UriData.

-spec resolve_positions(Uri :: uri(), LinesToResolve :: [diagnostic_line()]) ->
          #{diagnostic_line() => diagnostic_range()}.
resolve_positions(Uri, LinesToResolve0) ->
    LinesToResolve1 = lists:usort(LinesToResolve0),
    case esrv_index_mgr:get_current_content(Uri) of
        {ok, CurrentContent} ->
            resolve_lines(1, LinesToResolve1, CurrentContent, #{});
        undefined ->
            #{}
    end.

-spec resolve_lines(NextLine :: diagnostic_line(),
                    ToResolve :: [diagnostic_line()],
                    Left :: binary(),
                    Acc :: #{diagnostic_line() => diagnostic_range()}) ->
          #{diagnostic_line() => diagnostic_range()}.
resolve_lines(_, [], _, Acc) ->
    Acc;
resolve_lines(_, _, <<>>, Acc) ->
    Acc;
resolve_lines(NextLine, [NextLine | T], Left0, Acc) ->
    {LineContent, Left1} =
        case binary:split(Left0, <<"\n">>) of
            [SplittedChunk, Left00] ->
                {SplittedChunk, Left00};
            [LastChunk] ->
                {LastChunk, <<>>}
        end,
    {ColumnFrom, ColumnTo} = resolve_columns(LineContent, 1),
    Range = {{NextLine, ColumnFrom}, {NextLine, ColumnTo}},
    resolve_lines(NextLine + 1, T, Left1, Acc#{NextLine => Range});
resolve_lines(NextLine, ToResolve, Left0, Acc) ->
    [_, Left1] = binary:split(Left0, <<"\n">>),
    resolve_lines(NextLine + 1, ToResolve, Left1, Acc).

-spec resolve_columns(Left :: binary(), NextColumn :: diagnostic_column()) ->
          {diagnostic_column(), diagnostic_column()}.
resolve_columns(<<C, T/binary>>, NextColumn) when C =:= $  orelse C =:= $\t ->
    resolve_columns(T, NextColumn + 1);
resolve_columns(Left, NextColumn) ->
    {NextColumn, NextColumn + byte_size(Left)}.

-spec send_notification(Uri :: uri(), Diagnostics :: [diagnostic()]) -> ok.
send_notification(Uri, Diagnostics0) ->
    Diagnostics1 =
        lists:map(fun(#diagnostic{position = {range, {StartLocation, EndLocation}},
                                  severity = Severity,
                                  source = Source,
                                  message = Message}) ->
                          [{<<"range">>, esrv_lib:format_range(StartLocation, EndLocation)},
                           {<<"severity">>, Severity},
                           {<<"source">>, Source},
                           {<<"message">>, Message}]
                  end, Diagnostics0),
    Notification =
        #notification{method = <<"textDocument/publishDiagnostics">>,
                      params = [{<<"uri">>, Uri},
                                {<<"diagnostics">>, Diagnostics1}]},
    ok = esrv_main_fsm:notification(Notification).

-spec apply_progress(Uri :: uri(), UriData :: #uri_data{}) -> #uri_data{}.
apply_progress(Uri, #uri_data{total_workers = TotalWorkers,
                              completed_workers = CompletedWorkers,
                              token = Token0} = UriData0) ->
    Basename = filename:basename(Uri),
    Message = iolist_to_binary([<<"Diagnostic '">>, Basename, <<"' ">>]),
    Token1 =
        if
            CompletedWorkers =:= 0 ->
                esrv_progress_srv:create_by_server(Message, undefined, undefined);
            CompletedWorkers < TotalWorkers ->
                Percentage = floor(100 * CompletedWorkers / TotalWorkers),
                esrv_progress_srv:report(Token0, undefined, Message, Percentage),
                Token0;
            CompletedWorkers =:= TotalWorkers ->
                esrv_progress_srv:finalize(Token0, Message),
                undefined
        end,
    UriData0#uri_data{token = Token1}.

-spec dependent_on_header(HrlUri :: uri()) -> sets:set(uri()).
dependent_on_header(HrlUri) ->
    Processor =
        fun({Uri, #module_data{include_data = #include_data{resolved = Resolved}}}, Acc) ->
                Acc#{Uri => maps:keys(Resolved)}
        end,
    PerUriIncluded = esrv_req_lib:traverse_proj_modules(Processor, #{}),
    TargetUris = [HrlUri | maps:get(HrlUri, PerUriIncluded, [])],
    maps:fold(fun(Uri, Included, Acc) ->
                      Extension = filename:extension(Uri),
                      case lists:any(fun(U) -> lists:member(U, TargetUris) end, Included) of
                          true when Extension =:= <<".erl">> ->
                              sets:add_element(Uri, Acc);
                          _ ->
                              Acc
                      end
              end, sets:new(), PerUriIncluded).

-spec get_post_diagnostic(Uri :: uri(), Dependencies :: dependencies()) -> boolean().
get_post_diagnostic(Uri, Dependencies) ->
    case get_module_name(Uri) of
        {ok, ModuleName} when is_map_key(ModuleName, Dependencies) ->
            #dependency{post_diagnostics = PostDiagnostic} = maps:get(ModuleName, Dependencies),
            PostDiagnostic;
        _ ->
            false
    end.

-spec set_post_diagnostic(Uri :: uri(),
                          PostDiagnostic :: boolean(),
                          Dependencies :: dependencies()) -> dependencies().
set_post_diagnostic(Uri, PostDiagnostic, Dependencies0) ->
    case get_module_name(Uri) of
        {ok, ModuleName} when is_map_key(ModuleName, Dependencies0) ->
            maps:update_with(ModuleName,
                             fun(Dependency) ->
                                     Dependency#dependency{post_diagnostics = PostDiagnostic}
                             end, Dependencies0);
        _ ->
            Dependencies0
    end.

-spec get_module_name(Uri :: uri()) -> {ok, module()} | undefined.
get_module_name(Uri) ->
    Filename = esrv_lib:uri_to_file(Uri),
    case filename:extension(Filename) of
        ".erl" ->
            Basename = filename:basename(Filename, ".erl"),
            {ok, list_to_atom(Basename)};
        _ ->
            undefined
    end.
