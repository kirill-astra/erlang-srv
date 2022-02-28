-module(esrv_client_gw).

-behaviour(gen_server).

-include("records.hrl").
-include("log.hrl").

%% API
-export([start_link/0,
         set_connection/1,
         message_in/1,
         message_out/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2, terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-type transport() :: esrv_stdio | esrv_tcp.

-record(state, {transport :: transport(),
                listener :: pid() | undefined,
                connection :: esrv_transport:connection() | undefined}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec set_connection(Connection :: esrv_transport:connection()) -> ok.
set_connection(Connection) ->
    gen_server:cast(?SERVER, {set_connection, Connection}).

-spec message_in(Message :: message()) -> ok.
message_in(Message) ->
    gen_server:cast(?SERVER, {message_in, Message}).

-spec message_out(Message :: message()) -> ok.
message_out(Message) ->
    gen_server:cast(?SERVER, {message_out, Message}).

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
    {ok, Transport} = application:get_env(erlang_srv, transport),
    State = #state{transport = Transport},
    {ok, State, {continue, start_listener}}.

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
handle_cast({set_connection, Connection}, State0) ->
    {noreply, State0#state{connection = Connection}};

handle_cast({message_in, Message}, State) ->
    esrv_main_fsm:message_in(Message),
    {noreply, State};

handle_cast({message_out, Message}, #state{transport = Transport,
                                           connection = Connection} = State) ->
    Payload = esrv_msg_coder:encode(Message),
    ok = Transport:send(Connection, Payload),
    {noreply, State};

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
handle_continue(start_listener, #state{transport = Transport} = State0) ->
    {ok, Listener} = Transport:start_listener(),
    {noreply, State0#state{listener = Listener}};

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
