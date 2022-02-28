-module(esrv_progress_srv).

-behaviour(gen_server).

-include("types.hrl").
-include("records.hrl").
-include("log.hrl").

%% API
-export([start_link/0,
         create_by_server/3,
         create_by_client/4,
         report/4,
         finalize/2,
         cancel/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2, terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(progress, {title :: binary(),
                   cancel_callback :: pid() | undefined,
                   is_created :: boolean(),
                   to_notify :: [notification()],
                   is_finalized :: boolean()}).

-record(state, {progress_data :: #{lp_token() => #progress{}},
                next_id :: integer()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec create_by_server(Title :: binary(),
                       CancelCallback :: pid() | undefined,
                       Message :: binary() | undefined) -> lp_token() | undefined.
create_by_server(Title, CancelCallback, Message) ->
    case esrv_lib:get_client_capability([<<"window">>, <<"workDoneProgress">>]) of
        true ->
            gen_server:call(?SERVER, {create_by_server, Title, CancelCallback, Message});
        _ ->
            undefined
    end.

-spec create_by_client(Token :: lp_token() | undefined,
                       Title :: binary(),
                       CancelCallback :: pid() | undefined,
                       Message :: binary() | undefined) -> ok.
create_by_client(undefined, _, _, _) -> ok;
create_by_client(Token, Title, CancelCallback, Message) ->
    gen_server:cast(?SERVER, {create_by_client, Token, Title, CancelCallback, Message}).

-spec report(Token :: lp_token() | undefined,
             CancelCallback :: pid() | undefined,
             Message :: binary() | undefined,
             Percentage :: integer() | undefined) -> ok.
report(undefined, _, _, _) -> ok;
report(Token, CancelCallback, Message, Percentage) ->
    gen_server:cast(?SERVER, {report, Token, CancelCallback, Message, Percentage}).

-spec finalize(Token :: lp_token() | undefined, Message :: binary() | undefined) -> ok.
finalize(undefined, _) -> ok;
finalize(Token, Message) ->
    gen_server:cast(?SERVER, {finalize, Token, Message}).

-spec cancel(Token :: lp_token()) -> ok.
cancel(Token) ->
    gen_server:cast(?SERVER, {cancel, Token}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
          {ok, State :: term(), Timeout :: timeout()} |
          {ok, State :: term(), hibernate} |
          {ok, State :: term(), {continue, Continue :: term()}} |
          {stop, Reason :: term()} |
          ignore.
init([]) ->
    State = #state{progress_data = #{}, next_id = 1},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
          {reply, Reply :: term(), NewState :: term()} |
          {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
          {reply, Reply :: term(), NewState :: term(), hibernate} |
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
          {stop, Reason :: term(), NewState :: term()}.
handle_call({create_by_server, Title, CancelCallback, Message}, From,
            #state{progress_data = ProgressData0, next_id = NextId} = State0) ->
    Token = list_to_binary("server_token_" ++ integer_to_list(NextId)),
    gen_server:reply(From, Token),
    spawn_link(fun() ->
                       Request = #request{id = esrv_main_fsm:next_id(),
                                          method = <<"window/workDoneProgress/create">>,
                                          params = [{<<"token">>, Token}]},
                       Result =
                           case esrv_main_fsm:request(Request, 5000) of
                               {ok, #response{error = undefined}} ->
                                   ok;
                               {ok, #response{error = Error}} ->
                                   Error;
                               error ->
                                   error
                           end,
                       ?SERVER ! {create_result, Token, Result}
               end),
    Notification =
        #notification{method = <<"$/progress">>,
                      params = [{<<"token">>, Token},
                                {<<"value">>, [{<<"kind">>, 'begin'},
                                               {<<"title">>, Title},
                                               {<<"cancellable">>, is_pid(CancelCallback)},
                                               {<<"message">>, Message},
                                               {<<"percentage">>, 0}]}]},
    Progress = #progress{title = Title,
                         cancel_callback = CancelCallback,
                         is_created = false,
                         to_notify = [Notification],
                         is_finalized = false},
    State1 = State0#state{progress_data = store_progress(Token, Progress, ProgressData0),
                          next_id = NextId + 1},
    {noreply, State1};

handle_call(Call, _From, State) ->
    ?LOG_ERROR("Module: ~p; not implemented call: ~p", [?MODULE, Call]),
    {stop, not_implemented, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), NewState :: term()}.
handle_cast({create_by_client, Token, Title, CancelCallback, Message},
            #state{progress_data = ProgressData0} = State0) ->
    Notification =
        #notification{method = <<"$/progress">>,
                      params = [{<<"token">>, Token},
                                {<<"value">>, [{<<"kind">>, 'begin'},
                                               {<<"title">>, Title},
                                               {<<"cancellable">>, is_pid(CancelCallback)},
                                               {<<"message">>, Message},
                                               {<<"percentage">>, 0}]}]},
    Progress0 = #progress{title = Title,
                          cancel_callback = CancelCallback,
                          is_created = true,
                          to_notify = [Notification],
                          is_finalized = false},
    Progress1 = send_notifications(Progress0),
    State1 = State0#state{progress_data = store_progress(Token, Progress1, ProgressData0)},
    {noreply, State1};

handle_cast({report, Token, CancelCallback, Message, Percentage},
            #state{progress_data = ProgressData0} = State0) ->
    Progress0 = maps:get(Token, ProgressData0, undefined),
    case Progress0 of
        #progress{to_notify = ToNotify0} ->
            Notification =
                #notification{method = <<"$/progress">>,
                              params = [{<<"token">>, Token},
                                        {<<"value">>, [{<<"kind">>, report},
                                                       {<<"cancellable">>, is_pid(CancelCallback)},
                                                       {<<"message">>, Message},
                                                       {<<"percentage">>, Percentage}]}]},
            Progress1 = Progress0#progress{cancel_callback = CancelCallback,
                                           to_notify = [Notification | ToNotify0]},
            Progress2 = send_notifications(Progress1),
            ProgressData1 = store_progress(Token, Progress2, ProgressData0),
            State1 = State0#state{progress_data = ProgressData1},
            {noreply, State1};
        undefined ->
            {noreply, State0}
    end;

handle_cast({finalize, Token, Message}, #state{progress_data = ProgressData0} = State0) ->
    Progress0 = maps:get(Token, ProgressData0, undefined),
    case Progress0 of
        #progress{to_notify = ToNotify0} ->
            Notification =
                #notification{method = <<"$/progress">>,
                              params = [{<<"token">>, Token},
                                        {<<"value">>, [{<<"kind">>, 'end'},
                                                       {<<"message">>, Message}]}]},
            Progress1 = Progress0#progress{cancel_callback = undefined,
                                           to_notify = [Notification | ToNotify0],
                                           is_finalized = true},
            Progress2 = send_notifications(Progress1),
            ProgressData1 = store_progress(Token, Progress2, ProgressData0),
            State1 = State0#state{progress_data = ProgressData1},
            {noreply, State1};
        undefined ->
            {noreply, State0}
    end;

handle_cast({cancel, Token}, #state{progress_data = ProgressData0} = State0) ->
    Progress0 = maps:get(Token, ProgressData0, undefined),
    case Progress0 of
        #progress{cancel_callback = CancelCallback} ->
            if is_pid(CancelCallback) -> CancelCallback ! cancel_progress; true -> ok end,
            ProgressData1 = maps:remove(Token, ProgressData0),
            State1 = State0#state{progress_data = ProgressData1},
            {noreply, State1};
        undefined ->
            {noreply, State0}
    end;

handle_cast(Cast, State) ->
    ?LOG_ERROR("Module: ~p; not implemented cast: ~p", [?MODULE, Cast]),
    {stop, not_implemented, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: normal | term(), NewState :: term()}.
handle_info({create_result, Token, Result}, #state{progress_data = ProgressData0} = State0) ->
    Progress0 = maps:get(Token, ProgressData0, undefined),
    case Progress0 of
        #progress{} ->
            ProgressData1 =
                case Result of
                    ok ->
                        Progress1 = Progress0#progress{is_created = true},
                        Progress2 = send_notifications(Progress1),
                        store_progress(Token, Progress2, ProgressData0);
                    _ ->
                        ?LOG_WARNING("Unable to create work done progress; "
                                     "token: ~p; result: ~tp", [Token, Result]),
                        maps:remove(Token, ProgressData0)
                end,
            State1 = State0#state{progress_data = ProgressData1},
            {noreply, State1};
        undefined ->
            {noreply, State0}
    end;

handle_info(Info, State) ->
    ?LOG_ERROR("Module: ~p; not implemented info: ~p", [?MODULE, Info]),
    {stop, not_implemented, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_continue(Continue :: term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {noreply, NewState :: term(), {continue, Continue :: term()}} |
          {stop, Reason :: term(), NewState :: term()}.
handle_continue(Continue, State) ->
    ?LOG_ERROR("Module: ~p; not implemented continue: ~p", [?MODULE, Continue]),
    {stop, not_implemented, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: {down, term()} | term(),
                  State :: term(),
                  Extra :: term()) ->
          {ok, NewState :: term()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec send_notifications(Progress :: #progress{}) -> NewProgress :: #progress{}.
send_notifications(#progress{is_created = true, to_notify = ToNotify} = Progress0) ->
    lists:foreach(fun(Notification) ->
                          esrv_main_fsm:notification(Notification)
                  end, lists:reverse(ToNotify)),
    Progress0#progress{to_notify = []};
send_notifications(Progress) ->
    Progress.

-spec store_progress(Token :: lp_token(),
                     Progress :: #progress{},
                     ProgressData :: #{lp_token() => #progress{}}) -> #{lp_token() => #progress{}}.
store_progress(Token, Progress, ProgressData0) ->
    case Progress of
        #progress{is_created = true, to_notify = [], is_finalized = true} ->
            maps:remove(Token, ProgressData0);
        _ ->
            maps:put(Token, Progress, ProgressData0)
    end.
