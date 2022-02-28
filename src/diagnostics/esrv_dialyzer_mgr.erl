-module(esrv_dialyzer_mgr).

-behaviour(gen_statem).

-include("types.hrl").
-include("log.hrl").

%% API
-export([start_link/0,
         get_plts/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([inactive/3,
         preparing/3,
         idle/3,
         error/3]).

-define(SERVER, ?MODULE).

-record(data, {checkers :: [pid()] | undefined,
               plts :: [path()],
               error :: binary() | undefined}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_plts() -> {ok, [path()]} | not_ready | {error, binary()}.
get_plts() ->
    gen_statem:call(?SERVER, get_plts).

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
    {ok, inactive, #data{plts = []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one function like this for each state name.
%% Whenever a gen_statem receives an event, the function
%% with the name of the current state (StateName)
%% is called to handle the event.
%% @end
%%--------------------------------------------------------------------
-spec inactive('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
              (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
inactive({call, From}, get_plts, Data0) ->
    Data1 = Data0#data{checkers = [spawn_link(fun() -> otp_plt_check() end),
                                   spawn_link(fun() -> deps_plt_check() end)]},
    {next_state, preparing, Data1, [{reply, From, not_ready}]};

inactive(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, inactive, Data).

-spec preparing('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
               (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
preparing({call, From}, get_plts, _) ->
    {keep_state_and_data, [{reply, From, not_ready}]};

preparing(info, {plt, Checker, Plt}, #data{checkers = Checkers0, plts = Plts0} = Data0) ->
    Data1 = Data0#data{checkers = lists:delete(Checker, Checkers0), plts = [Plt | Plts0]},
    case Data1 of
        #data{checkers = []} ->
            ?LOG_INFO("Plt files ready"),
            {next_state, idle, Data1};
        _ ->
            {keep_state, Data1}
    end;

preparing(info, {error, Error0}, Data0) ->
    Error1 = <<Error0/binary, " (see log file for details)">>,
    Data1 = Data0#data{checkers = undefined, error = Error1},
    {next_state, error, Data1};

preparing(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, preparing, Data).

-spec idle('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
          (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
idle({call, From}, get_plts, #data{plts = Plts}) ->
    {keep_state_and_data, [{reply, From, {ok, Plts}}]};

idle(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, idle, Data).

-spec error('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
           (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
error({call, From}, get_plts, #data{error = Error}) ->
    {keep_state_and_data, [{reply, From, {error, Error}}]};

error(_, _, _) ->
    keep_state_and_data.

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
-spec otp_plt_check() -> any().
otp_plt_check() ->
    OtpPltPath = otp_plt_path(),
    filelib:ensure_dir(OtpPltPath),
    Result =
        try
            CheckCmd = [<<"dialyzer --check_plt --plt ">>, OtpPltPath],
            ok = exec(CheckCmd),
            {plt, self(), OtpPltPath}
        catch
            _:_ ->
                OtpPltTmpPath = otp_plt_tmp_path(),
                filelib:ensure_dir(OtpPltTmpPath),
                ?LOG_INFO("Building otp plt: ~s", [OtpPltPath]),
                BuildCmd = [<<"dialyzer --build_plt --apps ">>,
                            lists:join(" ", otp_app_names()),
                            <<" --output_plt ">>,
                            OtpPltTmpPath],
                case exec(BuildCmd) of
                    ok ->
                        file:rename(OtpPltTmpPath, OtpPltPath),
                        {plt, self(), OtpPltPath};
                    {unexpected, Unexpected} ->
                        ?LOG_ERROR("Building otp plt failed; cmd: ~s; result: ~s",
                                   [BuildCmd, Unexpected]),
                        {error, <<"building otp plt failed">>}
                end
        end,
    ?SERVER ! Result.

-spec otp_plt_path() -> path().
otp_plt_path() ->
    filename:join([esrv_lib:bdir_user_cache(),
                   <<"dialyzer">>,
                   <<"otp">>,
                   otp_plt_name()]).

-spec otp_plt_tmp_path() -> path().
otp_plt_tmp_path() ->
    {ok, CurrentDir} = file:get_cwd(),
    CurrentDirName = filename:basename(CurrentDir),
    filename:join([esrv_lib:bdir_user_cache(),
                   <<"dialyzer">>,
                   CurrentDirName,
                   otp_plt_name()]).

-spec otp_plt_name() -> binary().
otp_plt_name() ->
    OtpVersionFilename = filename:join([code:root_dir(),
                                        <<"releases">>,
                                        erlang:system_info(otp_release),
                                        <<"OTP_VERSION">>]),
    case filelib:is_regular(OtpVersionFilename) of
        true ->
            {ok, Version0} = file:read_file(OtpVersionFilename),
            Version1 = binary:replace(Version0, <<"\n">>, <<"">>),
            iolist_to_binary([<<"otp_">>, Version1, <<".plt">>]);
        false ->
            <<"otp.plt">>
    end.

-spec otp_app_names() -> [binary()].
otp_app_names() ->
    {ok, ProjPath} = esrv_config:get_value(proj_path),
    OtpPath = esrv_lib:otp_path(ProjPath),
    lists:filtermap(fun(Dir) ->
                            case re:run(Dir, ".*/(.*)-", [{capture, all_but_first, binary}]) of
                                {match, [AppName]} ->
                                    {true, AppName};
                                nomatch ->
                                    false
                            end
                    end, esrv_lib:pattern_to_dirs([OtpPath, <<"lib">>, "*"])).

-spec deps_plt_check() -> any().
deps_plt_check() ->
    DepsPltPath = deps_plt_path(),
    filelib:ensure_dir(DepsPltPath),
    Result =
        try
            DepsPltFilename = esrv_lib:path_to_file(DepsPltPath),
            {ok, Info} = dialyzer:plt_info(DepsPltFilename),
            PltPaths = [ esrv_lib:file_to_path(F) || F <- proplists:get_value(files, Info) ],
            {ToAdd, ToDelete} = deps_plt_sync_data(PltPaths),
            AddCmd = [<<"dialyzer --add_to_plt -r ">>,
                      lists:join(" ", ToAdd),
                      <<" --plt ">>,
                      DepsPltPath],
            ok = exec(AddCmd),
            DeleteCmd = [<<"dialyzer --remove_from_plt -r ">>,
                         lists:join(" ", ToDelete),
                         <<" --plt ">>,
                         DepsPltPath],
            ok = exec(DeleteCmd),
            CheckCmd = [<<"dialyzer --check_plt --plt ">>, DepsPltPath],
            ok = exec(CheckCmd),
            {plt, self(), DepsPltPath}
        catch
            _:_ ->
                ?LOG_INFO("Building deps plt: ~s", [DepsPltPath]),
                BuildCmd = [<<"dialyzer --build_plt -r ">>,
                            lists:join(" ", [ Path || {_, Path} <- deps_plt_paths() ]),
                            <<" --output_plt ">>,
                            DepsPltPath],
                case exec(BuildCmd) of
                    ok ->
                        {plt, self(), DepsPltPath};
                    {unexpected, Unexpected} ->
                        ?LOG_ERROR("Building deps plt failed; cmd: ~s; result: ~s",
                                   [BuildCmd, Unexpected]),
                        {error, <<"building deps plt failed">>}
                end
        end,
    ?SERVER ! Result.

-spec deps_plt_sync_data(PltPaths :: [path()]) -> {[path()], [path()]}.
deps_plt_sync_data(PltPaths) ->
    PltNames = [ filename:basename(P) || P <- PltPaths ],
    ToAdd = lists:filtermap(fun({Name, Path}) ->
                                    case not lists:member(Name, PltNames) of
                                        true ->
                                            {true, Path};
                                        false ->
                                            false
                                    end
                            end, deps_plt_paths()),
    ToDelete = lists:filter(fun(Path) ->
                                    not filelib:is_regular(Path)
                            end, PltPaths),
    {ToAdd, ToDelete}.

-spec deps_plt_paths() -> [{binary(), path()}].
deps_plt_paths() ->
    {ok, ProjPath} = esrv_config:get_value(proj_path),
    PltPaths =
        lists:foldl(fun(DepsAppPath, Acc0) ->
                            filelib:fold_files(DepsAppPath, "\.beam", true,
                                               fun(File, Acc00) ->
                                                       [{filename:basename(File), File} | Acc00]
                                               end, Acc0)
                    end, [], esrv_lib:deps_app_paths(ProjPath)),
    lists:ukeysort(1, PltPaths).

-spec deps_plt_path() -> binary().
deps_plt_path() ->
    {ok, CurrentDir} = file:get_cwd(),
    CurrentDirName = filename:basename(CurrentDir),
    filename:join([esrv_lib:bdir_user_cache(),
                   <<"dialyzer">>,
                   CurrentDirName,
                   <<"deps.plt">>]).

-spec exec(Cmd :: iolist()) -> ok | {unexpected, string()}.
exec(Cmd0) ->
    Cmd1 = iolist_to_binary(Cmd0),
    Cmd2 = binary_to_list(Cmd1),
    Result0= os:cmd(Cmd2),
    case string:find(Result0, "done (passed successfully)") of
        nomatch ->
            Result1 = re:replace(Result0, "\n", " ", [{return, list}, global]),
            Result2 = re:replace(Result1, " +", " ", [{return, list}, global]),
            {unexpected, Result2};
        _ ->
            ok
    end.
