-module(esrv_otp_node_request_srv).

-behaviour(gen_server).

-include("records.hrl").
-include("log.hrl").

%% API
-export([start_link/0,
         ensure_indexed/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2, terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {app_ids :: [app_id()]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec ensure_indexed(ScannedApps :: [scanned_app()]) -> ok.
ensure_indexed(ScannedApps) ->
    gen_server:call(?SERVER, {ensure_indexed, ScannedApps}, infinity).

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
    State = #state{app_ids = []},
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
handle_call({ensure_indexed, ScannedApps0}, _From, #state{app_ids = AppIds0} = State0) ->
    ScannedApps1 = lists:filter(fun(#scanned_app{id = Id}) ->
                                        not lists:member(Id, AppIds0)
                                end, ScannedApps0),
    case ScannedApps1 of
        [_|_] ->
            ok = lists:foreach(fun mnesia:dirty_write/1, ScannedApps1),
            ok = esrv_index_mgr:process_applications(ScannedApps1, []),
            AppIds1 =
                lists:foldl(fun(#scanned_app{id = Id}, AppIds00) ->
                                    [Id | AppIds00]
                            end, AppIds0, ScannedApps1),
            {reply, ok, State0#state{app_ids = AppIds1}};
        [] ->
            {reply, ok, State0}
    end;

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
