-module(esrv_man_mgr).

-behaviour(gen_statem).

-include("log.hrl").

%% API
-export([start_link/0,
         initialize/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([waiting_for_initialize/3,
         syncing/3,
         idle/3]).

-define(RESYNC_INTERVAL, 60000).
-define(OTP_DOWNLOADS_PAGE, "http://erlang.org/download/").
-define(GET(Url), httpc:request(get, {Url, []}, [], [{body_format, binary}])).
-define(SERVER, ?MODULE).

-record(data, {}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec initialize() -> ok.
initialize() ->
    gen_statem:cast(?SERVER, initialize).

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
    {ok, waiting_for_initialize, #data{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one function like this for each state name.
%% Whenever a gen_statem receives an event, the function
%% with the name of the current state (StateName)
%% is called to handle the event.
%% @end
%%--------------------------------------------------------------------
-spec waiting_for_initialize('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
                            (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
waiting_for_initialize(cast, initialize, Data) ->
    case esrv_config:get_value(sync_otp_man, true) of
        true ->
            {next_state, syncing, Data, [{state_timeout, 0, sync}]};
        _ ->
            {next_state, idle, Data}
    end;

waiting_for_initialize(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, waiting_for_initialize, Data).

-spec syncing('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
             (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
syncing(state_timeout, sync, Data) ->
    case fetch_man_tarball() of
        ok ->
            {next_state, idle, Data};
        error ->
            {keep_state_and_data, [{state_timeout, ?RESYNC_INTERVAL, sync}]}
    end;

syncing(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, syncing, Data).

-spec idle('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
          (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
idle(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, idle, Data).

-spec handle_event('enter', OldState :: term(), State :: term(), Data :: term()) ->
          gen_statem:state_enter_result(term());
                  (gen_statem:event_type(), Msg :: term(), State :: term(), Data :: term()) ->
          gen_statem:event_handler_result(term()).
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
-spec fetch_man_tarball() -> ok | error.
fetch_man_tarball() ->
    try
        {ok, {_, _, Body}} = ?GET(?OTP_DOWNLOADS_PAGE),
        RegExp = [<<"<a href=\"(otp_doc_man_">>,
                  erlang:system_info(otp_release),
                  <<".[0-9]*?.tar.gz)\">">>],
        {match, Elements} = re:run(Body, RegExp, [{capture, all_but_first, list}, global]),
        [TarballName] = lists:last(Elements),
        TarballPath = filename:join([esrv_lib:bdir_user_cache(), "otp_man", TarballName]),
        case filelib:is_regular(TarballPath) of
            false ->
                {ok, {_, _, Tarball}} = ?GET(?OTP_DOWNLOADS_PAGE ++ TarballName),
                ToExtractPath = filename:rootname(TarballPath, ".tar.gz"),
                file:make_dir(ToExtractPath),
                ok = erl_tar:extract({binary, Tarball}, [{cwd, ToExtractPath}, compressed]),
                ok = file:write_file(TarballPath, Tarball),
                ?LOG_INFO("Got OTP man pages: ~s", [TarballPath]);
            true ->
                ok
        end
    catch
        T:E:S ->
            ?LOG_ERROR("Unable to fetch OTP man pages; "
                       "type: ~p; error: ~p; stacktrace: ~p", [{T, E, S}]),
            error
    end.
