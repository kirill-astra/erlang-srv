-module(esrv_otp_node_controller).

-behaviour(gen_server).

-include("log.hrl").

%% API
-export([start_link/0,
         initialize/0,
         get_otp_node/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2, terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {otp_node :: node() | undefined}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec initialize() -> ok.
initialize() ->
    gen_server:call(?SERVER, initialize, infinity).

-spec get_otp_node() -> {ok, node()} | undefined.
get_otp_node() ->
    case whereis(?SERVER) =/= undefined of
        true ->
            gen_server:call(?SERVER, get_otp_node, infinity);
        false ->
            undefined
    end.

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
    State = #state{},
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
handle_call(initialize, _From, State0) ->
    State1 =
        case esrv_config:get_value(dedicated_otp_node, true) of
            true ->
                OtpNode = esrv_distributed:get_otp_node_name(),
                ok = initialize_otp_node(OtpNode),
                ok = rpc:call(OtpNode, esrv_otp_node_mgr, register_node, [node()]),
                ?LOG_INFO("Dedicated OTP node attached: '~s'", [OtpNode]),
                monitor_node(OtpNode, true),
                State0#state{otp_node = OtpNode};
            _ ->
                ?LOG_INFO("No dedicated OTP node", []),
                State0
        end,
    {reply, ok, State1};

handle_call(get_otp_node, _From, #state{otp_node = OtpNode} = State) ->
    Reply =
        case OtpNode of
            OtpNode when OtpNode =/= undefined ->
                {ok, OtpNode};
            undefined ->
                undefined
        end,
    {reply, Reply, State};

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
handle_info({nodedown, OtpNode}, #state{otp_node = OtpNode} = State) ->
    ?LOG_ERROR("Dedicated OTP node '~s' down", [OtpNode]),
    {stop, otp_node_down, State};

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
-spec initialize_otp_node(OtpNode :: node()) -> ok.
initialize_otp_node(OtpNode) ->
    ok = maybe_start_otp_node(OtpNode),
    true = global:set_lock({init, node()}, [OtpNode]),
    ok = maybe_init_logger(OtpNode),
    ok = maybe_transfer_modules(OtpNode),
    ok = maybe_start_application(OtpNode),
    true = global:del_lock({init, node()}, [OtpNode]),
    ok.

-spec maybe_start_otp_node(OtpNode :: node()) -> ok.
maybe_start_otp_node(OtpNode) ->
    case net_adm:ping(OtpNode) of
        pong ->
            ok;
        pang ->
            NameArg =
                case esrv_lib:get_distr_mode() of
                    longnames ->
                        "-name " ++ atom_to_list(OtpNode);
                    shortnames ->
                        "-sname " ++ atom_to_list(OtpNode)
                end,
            Cmd = "erl " ++ NameArg ++ " -hidden -detached",
            os:cmd(Cmd),
            ?LOG_INFO("Exec 'start otp node' command: ~s", [Cmd]),
            ok = wait_for_otp_node(OtpNode)
    end.

-spec wait_for_otp_node(OtpNode :: node()) -> ok.
wait_for_otp_node(OtpNode) ->
    wait_for_otp_node(OtpNode, 50).

-spec wait_for_otp_node(OtpNode :: node(), AttemptsLeft :: pos_integer()) -> ok.
wait_for_otp_node(OtpNode, AttemptsLeft) when AttemptsLeft > 0 ->
    case net_adm:ping(OtpNode) of
        pong ->
            ok;
        pang ->
            timer:sleep(100),
            wait_for_otp_node(OtpNode, AttemptsLeft - 1)
    end.

-spec maybe_init_logger(OtpNode :: node()) -> ok.
maybe_init_logger(OtpNode) ->
    case rpc:call(OtpNode, application, get_env, [otp_node, logger_ready]) of
        {ok, true} ->
            ok;
        _ ->
            ok = rpc:call(OtpNode, logger, set_primary_config, [level, ?OTP_NODE_LOG_LEVEL]),
            lists:foreach(fun(HandlerId) ->
                                  ok = rpc:call(OtpNode, logger, remove_handler, [HandlerId])
                          end, rpc:call(OtpNode, logger, get_handler_ids, [])),
            LoggerConfig = erlang_srv:get_otp_node_logger_config(),
            ok = rpc:call(OtpNode, logger, add_handler, [server, logger_disk_log_h, LoggerConfig]),
            ok = rpc:call(OtpNode, application, set_env, [otp_node, logger_ready, true])
    end.

-spec maybe_transfer_modules(OtpNode :: node()) -> ok.
maybe_transfer_modules(OtpNode) ->
    case rpc:call(OtpNode, code, is_loaded, [erlang_srv]) of
        {file, _} ->
            ok;
        _ ->
            lists:foreach(fun(Module) ->
                                  {module, _} = code:ensure_loaded(Module),
                                  ok = transfer_module(Module, OtpNode)
                          end, [erlang_srv,
                                esrv_otp_node_app,
                                esrv_otp_node_sup,
                                esrv_otp_node_mgr,
                                esrv_otp_node_request_srv,
                                esrv_otp_node_controller,
                                esrv_cache_mgr,
                                esrv_index_mgr,
                                esrv_config,
                                esrv_db,
                                esrv_lib,
                                esrv_parser,
                                esrv_parser_lib,
                                esrv_tokens_parser,
                                esrv_forms_parser,
                                esrv_module_parser,
                                esrv_zone_parser])
    end.

-spec transfer_module(Module :: module(), OtpNode :: node()) -> ok.
transfer_module(Module, OtpNode) ->
    {Module, Binary, Filename} = code:get_object_code(Module),
    {module, Module} = rpc:call(OtpNode, code, load_binary, [Module, Filename, Binary]),
    ?LOG_DEBUG("Module '~s' transfered to '~s'", [Module, OtpNode]).

-spec maybe_start_application(OtpNode :: node()) -> ok.
maybe_start_application(OtpNode) ->
    Applications = rpc:call(OtpNode, application, which_applications, []),
    case lists:keymember(otp_node, 1, Applications) of
        false ->
            ok = rpc:call(OtpNode, esrv_config, start, []),
            ok = rpc:call(OtpNode, erlang_srv, start_otp_node_application, []);
        true ->
            ok
    end.
