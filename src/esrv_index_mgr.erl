-module(esrv_index_mgr).

-behaviour(gen_statem).

-include("types.hrl").
-include("parser.hrl").
-include("records.hrl").
-include("log.hrl").

%% API
-export([start_link/0,
         process_applications/2,
         document_notification/1,
         get_current_content/1,
         get_current_module_data/1]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([active/3]).

-define(SERVER, ?MODULE).

-record(index_application_request, {uri :: uri(),
                                    app_id :: app_id(),
                                    content :: binary(),
                                    reply_to :: any() | undefined}).

-record(index_persistent_request, {uri :: uri(),
                                   app_id :: app_id() | undefined,
                                   version :: non_neg_integer(),
                                   hash :: hash(),
                                   content :: binary()}).

-record(index_volatile_request, {uri :: uri(),
                                 app_id :: app_id() | undefined,
                                 version :: non_neg_integer(),
                                 content :: binary(),
                                 reply_to :: any()}).

-type index_request() :: #index_application_request{} |
                         #index_persistent_request{} |
                         #index_volatile_request{}.

-record(data, {index_requests :: queue:queue(index_request()),
               workers :: [pid()]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec process_applications(ScannedApps :: [scanned_app()], TargetPaths :: [path()]) -> ok.
process_applications(ScannedApps, TargetPaths) ->
    AppFilesInfo = apps_file_paths(ScannedApps, TargetPaths),
    gen_statem:call(?SERVER, {process_applications, AppFilesInfo}).

-spec document_notification(Notification :: notification()) -> ok.
document_notification(Notification) ->
    gen_statem:cast(?SERVER, {document_notification, Notification}).

-spec get_current_content(Uri :: uri()) -> {ok, binary()} | undefined.
get_current_content(Uri) ->
    gen_statem:call(?SERVER, {get_current_content, Uri}).

-spec get_current_module_data(Uri :: uri()) -> {ok, module_data()} | undefined.
get_current_module_data(Uri) ->
    gen_statem:call(?SERVER, {get_current_module_data, Uri}).

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
    Cores = erlang:system_info(logical_processors_available),
    Workers = [ spawn_link(fun worker/0) || _ <- lists:seq(1, Cores) ],
    {ok, active, #data{index_requests = queue:new(),
                       workers = Workers}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one function like this for each state name.
%% Whenever a gen_statem receives an event, the function
%% with the name of the current state (StateName)
%% is called to handle the event.
%% @end
%%--------------------------------------------------------------------
-spec active('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
            (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
active({call, From}, {process_applications, []}, _) ->
    {keep_state_and_data, [{reply, From, ok}]};
active({call, From}, {process_applications, [{AppId, Path} | T]}, _) ->
    Uri = esrv_lib:path_to_uri(Path),
    {ok, Content} = file:read_file(Path),
    Request0 = #index_application_request{uri = Uri, app_id = AppId, content = Content},
    case T of
        [_|_] ->
            {keep_state_and_data, [{next_event, internal, {add_request, Request0}},
                                   {next_event, {call, From}, {process_applications, T}}]};
        [] ->
            Request1 = Request0#index_application_request{reply_to = From},
            {keep_state_and_data, [{next_event, internal, {add_request, Request1}}]}
    end;

active(cast, {document_notification, #notification{method = Method, params = Params}}, _) ->
    case Method of
        <<"textDocument/didOpen">> ->
            {keep_state_and_data, [{next_event, internal, {did_open, Params}}]};
        <<"textDocument/didChange">> ->
            {keep_state_and_data, [{next_event, internal, {did_change, Params}}]};
        <<"textDocument/didSave">> ->
            {keep_state_and_data, [{next_event, internal, {did_save, Params}}]};
        <<"textDocument/didClose">> ->
            {keep_state_and_data, [{next_event, internal, {did_close, Params}}]}
    end;

active(internal, {did_open, #{<<"textDocument">> := #{<<"uri">> := Uri,
                                                      <<"languageId">> := <<"erlang">>,
                                                      <<"version">> := Version,
                                                      <<"text">> := Text}}}, _) ->
    Fun = fun(_) ->
                  AppId = esrv_lib:get_app_id(Uri),
                  ?LOG_DEBUG("File opened: ~ts ('~s')", [Uri, AppId]),
                  Hash = esrv_lib:hash(Text),
                  TextDocument = #text_document{uri = Uri,
                                                app_id = AppId,
                                                saved_text_hash = Hash,
                                                current_version = Version,
                                                current_content = Text},
                  ok = mnesia:write(TextDocument),
                  Request = #index_persistent_request{uri = Uri,
                                                      app_id = AppId,
                                                      version = Version,
                                                      hash = Hash,
                                                      content = Text},
                  {keep_state_and_data, [{next_event, internal, {add_request, Request}}]}
          end,
    esrv_db:text_document_transaction(Uri, Fun);

active(internal, {did_open, #{<<"textDocument">> := #{<<"uri">> := Uri,
                                                      <<"languageId">> := LanguageId}}}, _) ->
    ?LOG_DEBUG("File opening skipped: ~ts (~ts)", [Uri, LanguageId]),
    keep_state_and_data;

active(internal, {did_change, #{<<"textDocument">> := #{<<"uri">> := Uri,
                                                        <<"version">> := Version},
                                <<"contentChanges">> := [#{<<"text">> := Text}]}}, _) ->
    Fun = fun([TextDocument0]) ->
                  ?LOG_DEBUG("File changed: ~ts (version: ~p)", [Uri, Version]),
                  TextDocument1 = TextDocument0#text_document{current_version = Version,
                                                              current_content = Text,
                                                              current_module_data = undefined},
                  ok = mnesia:write(TextDocument1),
                  keep_state_and_data;
             ([]) ->
                  keep_state_and_data
          end,
    esrv_db:text_document_transaction(Uri, Fun);

active(internal, {did_save, #{<<"textDocument">> := #{<<"uri">> := Uri},
                              <<"text">> := Text}}, _) ->
    Fun = fun([#text_document{app_id = AppId,
                              current_version = CurrentVersion} = TextDocument0]) ->
                  ?LOG_DEBUG("File saved: ~ts", [Uri]),
                  Hash = esrv_lib:hash(Text),
                  TextDocument1 = TextDocument0#text_document{saved_text_hash = Hash,
                                                              current_content = Text,
                                                              current_module_data = undefined},
                  ok = mnesia:write(TextDocument1),
                  Request = #index_persistent_request{uri = Uri,
                                                      app_id = AppId,
                                                      version = CurrentVersion,
                                                      hash = Hash,
                                                      content = Text},
                  {keep_state_and_data, [{next_event, internal, {add_request, Request}}]};
             ([]) ->
                  keep_state_and_data
          end,
    esrv_db:text_document_transaction(Uri, Fun);

active(internal, {did_close, #{<<"textDocument">> := #{<<"uri">> := Uri}}}, _) ->
    Fun = fun(_) ->
                  ?LOG_DEBUG("File closed: ~ts", [Uri]),
                  ok = mnesia:delete(text_document, Uri, write),
                  keep_state_and_data
          end,
    esrv_db:text_document_transaction(Uri, Fun);

active({call, From}, {get_current_content, Uri}, _) ->
    Reply =
        case esrv_db:read_text_document(Uri) of
            [#text_document{current_content = CurrentContent}] ->
                {ok, CurrentContent};
            [] ->
                undefined
        end,
    {keep_state_and_data, [{reply, From, Reply}]};

active({call, From}, {get_current_module_data, Uri}, _) ->
    Fun = fun([#text_document{app_id = AppId,
                              current_version = CurrentVersion,
                              current_content = CurrentContent,
                              current_module_data = undefined}]) ->
                  Request = #index_volatile_request{uri = Uri,
                                                    app_id = AppId,
                                                    version = CurrentVersion,
                                                    content = CurrentContent,
                                                    reply_to = From},
                  {keep_state_and_data, [{next_event, internal, {add_request, Request}}]};
             ([#text_document{current_module_data = CurrentModuleData}]) ->
                  {keep_state_and_data, [{reply, From, {ok, CurrentModuleData}}]};
             ([]) ->
                  {keep_state_and_data, [{reply, From, undefined}]}
          end,
    esrv_db:text_document_transaction(Uri, Fun);

active(internal, {add_request, Request}, #data{index_requests = IndexRequests0} = Data0) ->
    Data1 = Data0#data{index_requests = queue:in(Request, IndexRequests0)},
    {keep_state, Data1, [{next_event, internal, fillup}]};

active(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, active, Data).

-spec handle_event('enter', OldState :: term(), State :: term(), Data :: term()) ->
          gen_statem:state_enter_result(term());
                  (gen_statem:event_type(), Msg :: term(), State :: term(), Data :: term()) ->
          gen_statem:event_handler_result(term()).
handle_event(internal, fillup, _, #data{index_requests = IndexRequests0,
                                        workers = Workers0} = Data0) ->
    case Workers0 of
        [Worker | Workers1] ->
            case queue:out(IndexRequests0) of
                {{value, IndexRequest}, IndexRequests1} ->
                    Worker ! {index, self(), IndexRequest},
                    Data1 = Data0#data{index_requests = IndexRequests1, workers = Workers1},
                    {keep_state, Data1, [{next_event, internal, fillup}]};
                {empty, _} ->
                    keep_state_and_data
            end;
        [] ->
            keep_state_and_data
    end;

handle_event(info, {indexed, Worker}, _, #data{workers = Workers0} = Data0) ->
    Workers1 = [Worker | Workers0],
    Data1 = Data0#data{workers = Workers1},
    {keep_state, Data1, [{next_event, internal, fillup}]};

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
-spec apps_file_paths(ScannedApps :: [scanned_app()], TargetPaths :: [path()]) ->
          [{app_id(), path()}].
apps_file_paths(ScannedApps, TargetPaths) ->
    lists:foldl(fun(#scanned_app{id = Id, type = Type, path = Path}, Acc) ->
                        [ {Id, AFP} || AFP <- app_file_paths(Type, Path, TargetPaths) ] ++ Acc
                end, [], ScannedApps).

-spec app_file_paths(AppType :: app_type(), Path :: path(), TargetPaths :: [path()]) -> [path()].
app_file_paths(AppType, Path, TargetPaths) ->
    ToSkip =
        lists:filter(fun(TargetPath) when TargetPath =:= Path ->
                             false;
                        (TargetPath) ->
                             esrv_lib:is_binary_prefix(Path, TargetPath)
                     end, TargetPaths),
    AppDirs =
        case AppType of
            otp ->
                [Path];
            deps ->
                [Path];
            sub_proj ->
                esrv_lib:get_app_dirs(Path);
            proj ->
                esrv_lib:get_app_dirs(Path)
        end,
    lists:foldl(fun(AppDir, Acc) ->
                        Extensions = [<<".erl">>, <<".hrl">>, <<".escript">>],
                        esrv_lib:scan_deep_files(AppDir, Extensions, ToSkip) ++ Acc
                end, [], AppDirs).

%%%-------------------------------------------------------------------
%%% Worker
%%%-------------------------------------------------------------------
-spec worker() -> no_return().
worker() ->
    receive
        {index, ReplyTo, Request} ->
            case Request of
                #index_application_request{} ->
                    index_application_request(Request);
                #index_persistent_request{} ->
                    index_persistent_request(Request);
                #index_volatile_request{} ->
                    index_volatile_request(Request)
            end,
            ReplyTo ! {indexed, self()},
            worker()
    end.

-spec index_application_request(Request :: #index_application_request{}) -> ok.
index_application_request(#index_application_request{uri = Uri,
                                                     app_id = AppId,
                                                     content = Content,
                                                     reply_to = ReplyTo}) ->
    Hash = crypto:hash(md5, Content),
    {ModuleData, PersistAll} = fetch_module_data(Uri, AppId, Hash, Content),
    ok = persist_module_data(Uri, Hash, AppId, ModuleData, PersistAll),
    case ReplyTo of
        ReplyTo when ReplyTo =/= undefined ->
            ok = gen_statem:reply(ReplyTo, ok);
        undefined ->
            ok
    end.

-spec index_persistent_request(Request :: #index_persistent_request{}) -> ok.
index_persistent_request(#index_persistent_request{uri = Uri,
                                                   app_id = AppId,
                                                   version = Version,
                                                   hash = Hash,
                                                   content = Content}) ->
    {ModuleData, PersistAll} = fetch_module_data(Uri, AppId, Hash, Content),
    Fun = fun([TextDocument]) ->
                  case TextDocument of
                      #text_document{saved_text_hash = Hash} ->
                          ok = persist_module_data(Uri, Hash, AppId, ModuleData, PersistAll),
                          ok = save_module_data(Version, ModuleData, TextDocument);
                      _ ->
                          ok
                  end;
             ([]) ->
                  case file:read_file(Uri) of
                      {ok, Content} ->
                          ok = persist_module_data(Uri, Hash, AppId, ModuleData, PersistAll);
                      _ ->
                          ok
                  end
          end,
    ok = esrv_db:text_document_transaction(Uri, Fun),
    ok = diagnose(Uri, AppId).

-spec diagnose(Uri :: uri(), AppId :: app_id()) -> ok.
diagnose(Uri, AppId) ->
    case filename:extension(Uri) of
        Extension when Extension =:= <<".erl">> orelse
                       Extension =:= <<".hrl">> orelse
                       Extension =:= <<".escript">> ->
            ok = esrv_diagnostics_srv:request(Uri, AppId);
        _ ->
            ok
    end.

-spec index_volatile_request(Request :: #index_volatile_request{}) -> ok.
index_volatile_request(#index_volatile_request{uri = Uri,
                                               app_id = AppId,
                                               version = Version,
                                               content = Content,
                                               reply_to = ReplyTo}) ->
    ?LOG_DEBUG("Parsing: ~ts (volatile)", [Uri]),
    ModuleData = esrv_parser:parse(Uri, AppId, Content),
    gen_statem:reply(ReplyTo, {ok, ModuleData}),
    Fun = fun([TextDocument]) ->
                  ok = save_module_data(Version, ModuleData, TextDocument);
             ([]) ->
                  ok
          end,
    esrv_db:text_document_transaction(Uri, Fun).

-spec fetch_module_data(Uri :: uri(),
                        AppId :: app_id(),
                        Hash :: hash(),
                        Content :: binary()) -> {module_data(), boolean()}.
fetch_module_data(Uri, AppId, Hash, Content) ->
    case esrv_db:read_parsed_module(Uri, false) of
        [#parsed_module{module_data = ModuleData,
                        hash = Hash,
                        parser_version = ?PARSER_VERSION}] ->
            {ModuleData, false};
        _ ->
            ?LOG_DEBUG("Parsing: ~ts (persist)", [Uri]),
            {esrv_parser:parse(Uri, AppId, Content), true}
    end.

-spec persist_module_data(Uri :: uri(),
                          Hash :: hash(),
                          AppId :: app_id() | undefined,
                          ModuleData :: module_data(),
                          PersistAll :: boolean()) -> ok.
persist_module_data(Uri, Hash, AppId, ModuleData, true) ->
    ok = esrv_db:write_parsed_module(Uri, Hash, ModuleData),
    ok = esrv_db:delete_cache({module_data, Uri}),
    persist_module_data(Uri, Hash, AppId, ModuleData, false);
persist_module_data(Uri, Hash, AppId, ModuleData, false) ->
    ok = esrv_db:write_module_meta(Uri, Hash, AppId, ModuleData).

-spec save_module_data(Version :: non_neg_integer(),
                       ModuleData :: module_data(),
                       TextDcument :: #text_document{}) -> ok.
save_module_data(Version, ModuleData, #text_document{current_version = Version} = TextDocument0) ->
    TextDocument1 = TextDocument0#text_document{current_module_data = ModuleData},
    ok = mnesia:write(TextDocument1);
save_module_data(_, _, _) ->
    ok.
