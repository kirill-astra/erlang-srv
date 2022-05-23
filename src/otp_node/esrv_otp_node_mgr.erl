-module(esrv_otp_node_mgr).

-behaviour(gen_server).

-include("log.hrl").

%% API
-export([start_link/0,
         register_node/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2, terminate/2, code_change/3, format_status/2]).

-define(STOP_TIMEOUT, 60000).
-define(SERVER, ?MODULE).

-record(state, {is_singleton :: boolean(),
                registered_nodes :: [node()],
                terminate_tref :: timer:tref() | undefined}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec register_node(Node :: node()) -> ok.
register_node(Node) ->
    gen_server:call(?SERVER, {register_node, Node}).

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
    IsSingleton = esrv_config:get_value(singleton_otp_node, false),
    State0 = #state{is_singleton = IsSingleton,
                    registered_nodes = []},
    State1 = check_stop_timer(State0),
    {ok, State1}.

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
handle_call({register_node, Node}, _From, State0) ->
    State1 = add_node(Node, State0),
    State2 = cancel_stop_timer(State1),
    ?LOG_INFO("Node '~s' registered", [Node]),
    {reply, ok, State2};

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
handle_cast(stop, State) ->
    ?LOG_INFO("No registered nodes, terminating..."),
    {stop, normal, State};

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
handle_info({nodedown, Node}, State0) ->
    ?LOG_INFO("Node '~s' unregistered", [Node]),
    State1 = delete_node(Node, State0),
    State2 = check_stop_timer(State1),
    {noreply, State2};

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
-spec add_node(Node :: node(), State :: #state{}) -> #state{}.
add_node(Node, #state{registered_nodes = RegisteredNodes0} = State0) ->
    monitor_node(Node, true),
    State0#state{registered_nodes = [Node | RegisteredNodes0]}.

-spec delete_node(Node :: node(), State :: #state{}) -> #state{}.
delete_node(Node, #state{registered_nodes = RegisteredNodes0} = State0) ->
    State0#state{registered_nodes = lists:delete(Node, RegisteredNodes0)}.

-spec check_stop_timer(State :: #state{}) -> #state{}.
check_stop_timer(#state{is_singleton = false,
                        registered_nodes = [],
                        terminate_tref = undefined} = State0) ->
    ?LOG_INFO("Start terminate timer"),
    {ok, TRef} = timer:apply_after(?STOP_TIMEOUT, gen_server, cast, [?SERVER, stop]),
    State0#state{terminate_tref = TRef};
check_stop_timer(State) ->
    State.

-spec cancel_stop_timer(State :: #state{}) -> #state{}.
cancel_stop_timer(#state{terminate_tref = TRef} = State0) when TRef =/= undefined ->
    ?LOG_INFO("Stop terminate timer"),
    timer:cancel(TRef),
    State0#state{terminate_tref = undefined};
cancel_stop_timer(State) ->
    State.
