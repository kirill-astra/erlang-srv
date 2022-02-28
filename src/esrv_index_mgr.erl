-module(esrv_index_mgr).

-behaviour(gen_statem).

-include("types.hrl").
-include("parser.hrl").
-include("records.hrl").
-include("log.hrl").

%% API
-export([start_link/0,
         is_active/0,
         document_notification/1,
         get_current_content/1,
         get_current_module_data/1]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([waiting_for_initialized/3,
         initializing/3,
         active/3]).

-define(SERVER, ?MODULE).

-record(index_restore_request, {uri :: uri(),
                                app_path :: path(),
                                content :: binary(),
                                module_type :: module_type()}).

-record(index_persist_request, {uri :: uri(),
                                app_path :: path(),
                                version :: non_neg_integer(),
                                hash :: hash(),
                                content :: binary(),
                                module_type :: module_type()}).

-record(index_volatile_request, {uri :: uri(),
                                 app_path :: path(),
                                 version :: non_neg_integer(),
                                 content :: binary(),
                                 reply_to :: any() | undefined,
                                 module_type :: module_type()}).

-type index_request() :: #index_restore_request{} |
                         #index_persist_request{} |
                         #index_volatile_request{}.

-record(data, {pending :: [gen_statem:action()],
               index_requests :: queue:queue(index_request()),
               workers :: [pid()]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec is_active() -> boolean().
is_active() ->
    gen_statem:call(?SERVER, is_active).

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
    esrv_main_fsm:subscribe(),
    Cores = erlang:system_info(logical_processors_available),
    Workers = [ spawn_link(fun worker/0) || _ <- lists:seq(1, Cores) ],
    {ok, waiting_for_initialized, #data{pending = [],
                                        index_requests = queue:new(),
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
-spec waiting_for_initialized('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
                             (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
waiting_for_initialized(info, initialized, #data{workers = Workers} = Data0) ->
    BunchesData =
        [{<<"Indexing OTP">>, otp, fun otp_files/0},
         {<<"Indexing dependencies">>, deps, fun deps_files/0},
         {<<"Indexing project">>, proj, fun proj_files/0}],
    spawn_link(fun() -> bunch_server(BunchesData, Workers) end),
    ?LOG_INFO("Initial indexing started"),
    Data1 = Data0#data{workers = []},
    {next_state, initializing, Data1};

waiting_for_initialized(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, waiting_for_initialized, Data).

-spec initializing('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
                  (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
initializing(info, {bunches_indexed, Workers}, #data{pending = Pending} = Data0) ->
    ?LOG_INFO("Initial indexing completed"),
    Data1 = Data0#data{pending = [], workers = Workers},
    {next_state, active, Data1, lists:reverse(Pending)};

initializing(cast, {document_notification, Notification}, #data{pending = Pending0} = Data0) ->
    PendingAction = {next_event, cast, {document_notification, Notification}},
    Data1 = Data0#data{pending = [PendingAction | Pending0]},
    {keep_state, Data1};

initializing(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, initializing, Data).

-spec active('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
            (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
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
                  ?LOG_DEBUG("Project file opened: ~ts", [Uri]),
                  {AppPath, ModuleType} = esrv_lib:app_path_and_module_type(Uri),
                  Hash = esrv_lib:hash(Text),
                  TextDocument = #text_document{uri = Uri,
                                                app_path = AppPath,
                                                module_type = ModuleType,
                                                saved_text_hash = Hash,
                                                current_version = Version,
                                                current_content = Text},
                  ok = mnesia:write(TextDocument),
                  Request = #index_persist_request{uri = Uri,
                                                   app_path = AppPath,
                                                   version = Version,
                                                   hash = Hash,
                                                   content = Text,
                                                   module_type = ModuleType},
                  {keep_state_and_data, [{next_event, internal, {add_request, Request}}]}
          end,
    esrv_db:text_document_transaction(Uri, Fun);

active(internal, {did_open, #{<<"textDocument">> := #{<<"uri">> := Uri,
                                                      <<"languageId">> := LanguageId}}}, _) ->
    ?LOG_DEBUG("Project file opening skipped: ~ts (~ts)", [Uri, LanguageId]),
    keep_state_and_data;

active(internal, {did_change, #{<<"textDocument">> := #{<<"uri">> := Uri,
                                                        <<"version">> := Version},
                                <<"contentChanges">> := [#{<<"text">> := Text}]}}, _) ->
    Fun = fun([TextDocument0]) ->
                  ?LOG_DEBUG("Project file changed: ~ts (version: ~p)", [Uri, Version]),
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
    Fun = fun([#text_document{app_path = AppPath,
                              module_type = ModuleType,
                              current_version = CurrentVersion} = TextDocument0]) ->
                  ?LOG_DEBUG("Project file saved: ~ts", [Uri]),
                  Hash = esrv_lib:hash(Text),
                  TextDocument1 = TextDocument0#text_document{saved_text_hash = Hash,
                                                              current_content = Text,
                                                              current_module_data = undefined},
                  ok = mnesia:write(TextDocument1),
                  Request = #index_persist_request{uri = Uri,
                                                   app_path = AppPath,
                                                   version = CurrentVersion,
                                                   hash = Hash,
                                                   content = Text,
                                                   module_type = ModuleType},
                  {keep_state_and_data, [{next_event, internal, {add_request, Request}}]};
             ([]) ->
                  keep_state_and_data
          end,
    esrv_db:text_document_transaction(Uri, Fun);

active(internal, {did_close, #{<<"textDocument">> := #{<<"uri">> := Uri}}}, _) ->
    Fun = fun(_) ->
                  ?LOG_DEBUG("Project file closed: ~ts", [Uri]),
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
    Fun = fun([#text_document{app_path = AppPath,
                              module_type = ModuleType,
                              current_version = CurrentVersion,
                              current_content = CurrentContent,
                              current_module_data = undefined}]) ->
                  Request = #index_volatile_request{uri = Uri,
                                                    app_path = AppPath,
                                                    version = CurrentVersion,
                                                    content = CurrentContent,
                                                    reply_to = From,
                                                    module_type = ModuleType},
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

handle_event({call, From}, is_active, State, _) ->
    {keep_state_and_data, [{reply, From, State =:= active}]};

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

%%%-------------------------------------------------------------------
%%% Bunch server
%%%-------------------------------------------------------------------
-type app_file() :: {path(), path()}.
-type bunch_data() :: {binary(), module_type(), fun(() -> [app_file()])}.

-record(bunch, {title :: binary(),
                token :: lp_token() | undefined,
                module_type :: module_type(),
                total :: integer(),
                ready :: integer(),
                indexing :: integer(),
                app_files_left :: [app_file()],
                percentage :: 0 .. 100,
                step :: float()}).

-record(bs_data, {bunches_data :: [bunch_data()],
                  current_bunch :: #bunch{} | undefined,
                  workers :: [pid()]}).

-spec bunch_server(BunchesData :: [bunch_data()], Workers :: [pid()]) -> any().
bunch_server(BunchesData, Workers) ->
    BsData = #bs_data{bunches_data = BunchesData, workers = Workers},
    bs_loop(BsData).

-spec bs_loop(BsData :: #bs_data{}) -> any().
bs_loop(BsData0) ->
    BsData1 = bs_check_current_bunch(BsData0),
    BsData2 = bs_fillup_workers(BsData1),
    case BsData2 of
        #bs_data{bunches_data = [], current_bunch = undefined, workers = Workers} ->
            ?SERVER ! {bunches_indexed, Workers};
        _ ->
            receive
                {indexed, Worker} ->
                    BsData3 = bs_file_indexed(Worker, BsData2),
                    bs_loop(BsData3)
            end
    end.

-spec bs_check_current_bunch(BsData :: #bs_data{}) -> NewBsData :: #bs_data{}.
bs_check_current_bunch(#bs_data{bunches_data = [BunchData | T],
                                current_bunch = undefined} = BsData0) ->
    BsData1 = BsData0#bs_data{bunches_data = T,
                              current_bunch = bs_bunch_new(BunchData)},
    bs_check_current_bunch(BsData1);
bs_check_current_bunch(BsData) ->
    BsData.

-spec bs_fillup_workers(BsData :: #bs_data{}) -> NewBsData :: #bs_data{}.
bs_fillup_workers(#bs_data{current_bunch = CurrentBunch0,
                           workers = [Worker | WorkersT]} = BsData0) ->
    case CurrentBunch0 of
        #bunch{module_type = ModuleType,
               indexing = Indexing0,
               app_files_left = [{AppPath, File} | AppFilesLeftT]} ->
            {ok, Content} = file:read_file(File),
            Request = #index_restore_request{uri = esrv_lib:path_to_uri(File),
                                             app_path = AppPath,
                                             content = Content,
                                             module_type = ModuleType},
            Worker ! {index, self(), Request},
            CurrentBunch1 = CurrentBunch0#bunch{indexing = Indexing0 + 1,
                                                app_files_left = AppFilesLeftT},
            BsData1 = BsData0#bs_data{current_bunch = CurrentBunch1, workers = WorkersT},
            bs_fillup_workers(BsData1);
        _ ->
            BsData0
    end;
bs_fillup_workers(BsData) ->
    BsData.

-spec bs_file_indexed(Worker :: pid(), BsData :: #bs_data{}) -> NewBsData :: #bs_data{}.
bs_file_indexed(Worker, #bs_data{current_bunch = CurrentBunch0, workers = Workers0} = BsData0) ->
    CurrentBunch1 = bs_bunch_file_indexed(CurrentBunch0),
    CurrentBunch2 = bs_bunch_report_progress(CurrentBunch1),
    CurrentBunch3 =
        case CurrentBunch2 of
            #bunch{indexing = 0, app_files_left = []} ->
                undefined;
            _ ->
                CurrentBunch2
        end,
    BsData0#bs_data{current_bunch = CurrentBunch3, workers = [Worker | Workers0]}.

-spec bs_bunch_new(BunchData :: bunch_data()) -> #bunch{} | undefined.
bs_bunch_new({Title, ModuleType, GetAppFilesFun}) ->
    AppFiles = GetAppFilesFun(),
    Total = length(AppFiles),
    if
        Total > 0 ->
            ?LOG_INFO("~s (total files: ~p)", [Title, Total]),
            #bunch{title = Title,
                   token = esrv_progress_srv:create_by_server(Title, undefined, undefined),
                   module_type = ModuleType,
                   total = Total,
                   ready = 0,
                   indexing = 0,
                   app_files_left = AppFiles,
                   percentage = 0,
                   step = 100 / Total};
        Total =:= 0 ->
            ?LOG_INFO("~s skipped: no files found", [Title]),
            undefined
    end.

-spec bs_bunch_file_indexed(Bunch :: #bunch{}) -> NewBunch :: #bunch{}.
bs_bunch_file_indexed(#bunch{ready = Ready0, indexing = Indexing0} = Bunch0) ->
    Bunch0#bunch{ready = Ready0 + 1, indexing = Indexing0 - 1}.

-spec bs_bunch_report_progress(Bunch :: #bunch{}) -> NewBunch :: #bunch{}.
bs_bunch_report_progress(#bunch{token = Token,
                                total = Total,
                                ready = Ready,
                                percentage = Percentage0,
                                step = Step} = Bunch0) ->
    Percentage = floor(Ready * Step),
    if
        Ready =:= Total ->
            esrv_progress_srv:finalize(Token, <<"completed">>),
            Bunch0#bunch{percentage = 100};
        Percentage > Percentage0 ->
            Message = iolist_to_binary(io_lib:format("~p / ~p", [Ready, Total])),
            esrv_progress_srv:report(Token, undefined, Message, Percentage),
            Bunch0#bunch{percentage = Percentage};
        true ->
            Bunch0
    end.

-spec otp_files() -> [app_file()].
otp_files() ->
    {ok, ProjPath} = esrv_config:get_value(proj_path),
    AppPaths = esrv_lib:otp_app_paths(ProjPath),
    app_files([<<".">>], AppPaths, [ProjPath | esrv_lib:deps_paths(ProjPath)]).

-spec deps_files() -> [app_file()].
deps_files() ->
    {ok, ProjPath} = esrv_config:get_value(proj_path),
    {ok, AppsDirs} = esrv_config:get_value(apps_dirs),
    AppPaths = esrv_lib:deps_app_paths(ProjPath),
    app_files(AppsDirs, AppPaths, [esrv_lib:otp_path(ProjPath), ProjPath]).

-spec proj_files() -> [app_file()].
proj_files() ->
    {ok, ProjPath} = esrv_config:get_value(proj_path),
    {ok, AppsDirs} = esrv_config:get_value(apps_dirs),
    app_files(AppsDirs, [ProjPath], [esrv_lib:otp_path(ProjPath) | esrv_lib:deps_paths(ProjPath)]).

-spec app_files(AppsDirs :: [binary()],
                AppPaths :: [path()],
                ExcludePaths :: [path()]) -> [app_file()].
app_files(AppsDirs, AppPaths, ExcludePaths) ->
    lists:foldl(fun(AppPath, Acc0) ->
                        Patterns = [ [AppPath, AppsDir] || AppsDir <- AppsDirs ],
                        TargetDirs = esrv_lib:patterns_to_dirs(Patterns),
                        lists:foldl(fun(File, Acc00) ->
                                            [{AppPath, File} | Acc00]
                                    end, Acc0, esrv_lib:dirs_to_files(TargetDirs, ExcludePaths))
                end, [], AppPaths).

%%%-------------------------------------------------------------------
%%% Worker
%%%-------------------------------------------------------------------
-spec worker() -> no_return().
worker() ->
    receive
        {index, ReplyTo, Request} ->
            case Request of
                #index_restore_request{} ->
                    index_restore_request(Request);
                #index_persist_request{} ->
                    index_persist_request(Request);
                #index_volatile_request{} ->
                    index_volatile_request(Request)
            end,
            ReplyTo ! {indexed, self()},
            worker()
    end.

-spec index_restore_request(Request :: #index_restore_request{}) -> ok.
index_restore_request(#index_restore_request{uri = Uri,
                                             app_path = AppPath,
                                             content = Content,
                                             module_type = ModuleType}) ->
    Hash = crypto:hash(md5, Content),
    {ModuleData, PersistAll} = fetch_module_data(Uri, AppPath, ModuleType, Hash, Content),
    ok = persist_module_data(Uri, Hash, ModuleType, ModuleData, PersistAll).

-spec index_persist_request(Request :: #index_persist_request{}) -> ok.
index_persist_request(#index_persist_request{uri = Uri,
                                             app_path = AppPath,
                                             version = Version,
                                             hash = Hash,
                                             content = Content,
                                             module_type = ModuleType}) ->
    {ModuleData, PersistAll} = fetch_module_data(Uri, AppPath, ModuleType, Hash, Content),
    Fun = fun([TextDocument]) ->
                  case TextDocument of
                      #text_document{saved_text_hash = Hash} ->
                          ok = persist_module_data(Uri, Hash, ModuleType, ModuleData, PersistAll),
                          ok = save_module_data(Version, ModuleData, TextDocument);
                      _ ->
                          ok
                  end;
             ([]) ->
                  case file:read_file(Uri) of
                      {ok, Content} ->
                          ok = persist_module_data(Uri, Hash, ModuleType, ModuleData, PersistAll);
                      _ ->
                          ok
                  end
          end,
    ok = esrv_db:text_document_transaction(Uri, Fun),
    ok = diagnose(Uri, AppPath, ModuleType).

-spec diagnose(Uri :: uri(), AppPath :: path(), ModuleType :: module_type()) -> ok.
diagnose(Uri, AppPath, ModuleType) ->
    case filename:extension(Uri) of
        Extension when Extension =:= <<".erl">> orelse
                       Extension =:= <<".hrl">> orelse
                       Extension =:= <<".escript">> ->
            ok = esrv_diagnostics_srv:request(Uri, AppPath, ModuleType);
        _ ->
            ok
    end.

-spec index_volatile_request(Request :: #index_volatile_request{}) -> ok.
index_volatile_request(#index_volatile_request{uri = Uri,
                                               app_path = AppPath,
                                               version = Version,
                                               content = Content,
                                               reply_to = ReplyTo,
                                               module_type = ModuleType}) ->
    ?LOG_DEBUG("Parsing: ~ts (volatile)", [Uri]),
    ModuleData = esrv_parser:parse(Uri, AppPath, ModuleType, Content),
    gen_statem:reply(ReplyTo, {ok, ModuleData}),
    Fun = fun([TextDocument]) ->
                  ok = save_module_data(Version, ModuleData, TextDocument);
             ([]) ->
                  ok
          end,
    esrv_db:text_document_transaction(Uri, Fun).

-spec fetch_module_data(Uri :: uri(),
                        AppPath :: path(),
                        ModuleType :: module_type(),
                        Hash :: hash(),
                        Content :: binary()) -> {module_data(), boolean()}.
fetch_module_data(Uri, AppPath, ModuleType, Hash, Content) ->
    case esrv_db:read_parsed_module(Uri) of
        [#parsed_module{module_data = ModuleData,
                        hash = Hash,
                        parser_version = ?PARSER_VERSION}] ->
            {ModuleData, false};
        _ ->
            ?LOG_DEBUG("Parsing: ~ts (persist)", [Uri]),
            {esrv_parser:parse(Uri, AppPath, ModuleType, Content), true}
    end.

-spec persist_module_data(Uri :: uri(),
                          Hash :: hash(),
                          ModuleType :: module_type(),
                          ModuleData :: module_data(),
                          PersistAll :: boolean()) -> ok.
persist_module_data(Uri, Hash, ModuleType, ModuleData, true) ->
    ok = esrv_db:write_parsed_module(Uri, Hash, ModuleData),
    ok = esrv_db:delete_cache({module_data, Uri}),
    persist_module_data(Uri, Hash, ModuleType, ModuleData, false);
persist_module_data(Uri, Hash, ModuleType, ModuleData, false) ->
    ok = esrv_db:write_module_meta(Uri, Hash, ModuleType, ModuleData).

-spec save_module_data(Version :: non_neg_integer(),
                       ModuleData :: module_data(),
                       TextDcument :: #text_document{}) -> ok.
save_module_data(Version, ModuleData, #text_document{current_version = Version} = TextDocument0) ->
    TextDocument1 = TextDocument0#text_document{current_module_data = ModuleData},
    ok = mnesia:write(TextDocument1);
save_module_data(_, _, _) ->
    ok.
