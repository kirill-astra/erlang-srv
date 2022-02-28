-module(esrv_elvis_mgr).

-behaviour(gen_server).

-include("types.hrl").
-include("log.hrl").

%% API
-export([start_link/0,
         ensure_config/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2, terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-type result() :: {ok, elvis_config:configs()} | {error, binary()}.

-record(state, {hash :: hash() | undefined,
                config_path :: path() | undefined,
                result :: result() | undefined}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec ensure_config(Hash :: hash(), ConfigPath :: path()) -> result().
ensure_config(Hash, ConfigPath) ->
    gen_server:call(?SERVER, {ensure_config, Hash, ConfigPath}, infinity).

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
    {ok, #state{}}.

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
handle_call({ensure_config, Hash, ConfigPath}, _, State0) ->
    case State0 of
        #state{hash = Hash, config_path = ConfigPath, result = Result} ->
            {reply, Result, State0};
        _ ->
            Result = load_config(ConfigPath),
            State1 = State0#state{hash = Hash, config_path = ConfigPath, result = Result},
            {reply, Result, State1}
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
-spec load_config(ConfigPath :: path()) -> result().
load_config(ConfigPath) ->
    try
        ConfigFile = esrv_lib:path_to_file(ConfigPath),
        Config = elvis_config:from_file(ConfigFile),
        ?LOG_INFO("Elvis config loaded: ~ts", [ConfigPath]),
        ok = application:set_env(elvis_core, verbose, false),
        ok = application:set_env(elvis_core, no_output, true),
        {ok, Config}
    catch
        _:_ ->
            {error, <<"Loading ", ConfigPath/binary, " exception">>}
    end.
