-module(erlang_srv).

-export([main/1]).

-export([get_otp_node_logger_config/0,
         start_otp_node_application/0]).

-include("log.hrl").

-spec main(Args :: [any()]) -> ok.
main(Args) ->
    ok = esrv_config:create(),
    ok = parse_args(Args),
    case esrv_config:get_value(distr_mode) of
        undefined ->
            ok = start_language_server();
        _ ->
            ok = start_otp_node()
    end,
    receive _ -> ok end.

-spec parse_args(Args :: [string()]) -> ok.
parse_args(Args) ->
    application:load(erlang_srv),
    application:load(otp_node),
    OptSpec =
        [{version, $v, "version", undefined,
          "Print the current version of Erlang Server and exit"},
         {transport, $t, "transport", {string, "stdio"},
          "Specify the transport the server will use for "
          "the connection with the client, either 'stdio' or 'tcp'"},
         {port, $p, "port", {integer, 10000},
          "Specify the port used for 'tcp' transport"},
         {log_dir, $d, "log-dir", {string, default_log_dir()},
          "Specify the logs directory"},
         {log_level, $l, "log-level", {string, "info"},
          "Specify the log level. Possible values: "
          "'emergency', 'alert', 'critical', 'error', "
          "'warning', 'notice', 'info', 'debug' "
          "and two special values 'all' and 'none'"},
         {log_otp, $o, "log-otp", {string, "stop"},
          "Specify the filter action for otp/sasl log events, "
          "either 'log' or 'stop'"},
         {otp_node, undefined, "otp-node", string,
          "Claim to start OTP node instead of language server node, "
          "either 'longnames' or 'shortnames'"}],
    try
        {ok, {ParsedArgs, _}} = getopt:parse(OptSpec, Args),
        lists:foreach(fun(version) ->
                              io:format("Version: ~s~n", [esrv_lib:version()]),
                              halt(0);
                         ({transport, "stdio"}) ->
                              application:set_env(erlang_srv, transport, esrv_stdio);
                         ({transport, "tcp"}) ->
                              application:set_env(erlang_srv, transport, esrv_tcp);
                         ({port, Port}) ->
                              application:set_env(erlang_srv, port, Port);
                         ({log_dir, LogDir}) ->
                              esrv_config:set_value(log_dir, LogDir);
                         ({log_level, LogLevel}) when LogLevel =:= "emergency";
                                                      LogLevel =:= "alert";
                                                      LogLevel =:= "critical";
                                                      LogLevel =:= "error";
                                                      LogLevel =:= "warning";
                                                      LogLevel =:= "notice";
                                                      LogLevel =:= "info";
                                                      LogLevel =:= "debug";
                                                      LogLevel =:= "all";
                                                      LogLevel =:= "none" ->
                              esrv_config:set_value(log_level, list_to_atom(LogLevel));
                         ({log_otp, LogOtp}) when LogOtp =:= "log";
                                                  LogOtp =:= "stop" ->
                              esrv_config:set_value(log_otp, list_to_atom(LogOtp));
                         ({otp_node, OtpNode}) when OtpNode =:= "longnames";
                                                    OtpNode =:= "shortnames" ->
                              ok = esrv_config:set_value(singleton_otp_node, true),
                              ok = esrv_config:set_value(distr_mode, list_to_binary(OtpNode))
                      end, ParsedArgs)
    catch
        _:_ ->
            getopt:usage(OptSpec, "erlang_srv"),
            halt(1)
    end.

-spec default_log_dir() -> file:filename().
default_log_dir() ->
    UserLogDir = esrv_lib:bdir_user_log(),
    binary_to_list(UserLogDir).

%%%===================================================================
%%% Language server
%%%===================================================================
-spec start_language_server() -> ok.
start_language_server() ->
    ok = init_ls_logger(),
    {ok, _} = application:ensure_all_started(erlang_srv, permanent),
    ?LOG_INFO("Language server started", []).

-spec init_ls_logger() -> ok.
init_ls_logger() ->
    {ok, LogLevel} = esrv_config:get_value(log_level),
    ok = logger:set_primary_config(level, LogLevel),
    [ ok = logger:remove_handler(HId) || HId <- logger:get_handler_ids() ],
    CheckTraceFilter =
        fun(#{meta := Meta} = LogEvent, [Expected]) ->
                case maps:get(trace, Meta, false) of
                    Expected -> LogEvent;
                    _ -> stop
                end
        end,
    ok = logger:add_handler(server, logger_disk_log_h,
                            get_ls_server_logger_config(LogLevel, CheckTraceFilter)),
    ok = logger:add_handler(traces, logger_disk_log_h,
                            get_ls_traces_logger_config(CheckTraceFilter)).

-spec get_ls_server_logger_config(LogLevel :: logger:level(), CheckTraceFilter :: fun()) ->
          logger:handler_config().
get_ls_server_logger_config(LogLevel, CheckTraceFilter) ->
    {ok, LogOtp} = esrv_config:get_value(log_otp),
    MessageTemplate =
        if
            LogLevel =:= debug orelse LogLevel =:= all ->
                [time, " [", level, "] ", pid, " ", mfa, ":", line, " ", msg, "\n"];
            true ->
                [time, " [", level, "] ", pid, " ", msg, "\n"]
        end,
    #{config => #{file => get_ls_log_file("server.log"),
                  max_no_files => 5,
                  max_no_bytes => 8388608,
                  filesync_repeat_interval => 3000},
      filters => [{check_trace, {CheckTraceFilter, [false]}},
                  {otp, {fun logger_filters:domain/2,
                         {LogOtp, sub, [otp, sasl]}}}],
      formatter => {logger_formatter, #{template => MessageTemplate,
                                        time_designator => $ }}}.

-spec get_ls_traces_logger_config(CheckTraceFilter :: fun()) -> logger:handler_config().
get_ls_traces_logger_config(CheckTraceFilter) ->
    TraceTemplate = [time, " [trace] [", type, "] ", msg, "\n"],
    #{config => #{file => get_ls_log_file("traces.log"),
                  max_no_files => 5,
                  max_no_bytes => 8388608,
                  filesync_repeat_interval => 3000},
      filters => [{check_trace, {CheckTraceFilter, [true]}}],
      formatter => {logger_formatter, #{single_line => false,
                                        template => TraceTemplate,
                                        time_designator => $ }}}.

-spec get_ls_log_file(Name :: string()) -> file:filename().
get_ls_log_file(Name) ->
    {ok, LogDir} = esrv_config:get_value(log_dir),
    {ok, CurrentDir} = file:get_cwd(),
    CurrentDirName = filename:basename(CurrentDir),
    filename:join([LogDir, CurrentDirName, Name]).

%%%===================================================================
%%% OTP node
%%%===================================================================
-spec start_otp_node() -> ok.
start_otp_node() ->
    ok = esrv_distributed:initialize_otp_node(),
    true = global:set_lock({init, node()}, [node()]),
    ok = init_otp_node_logger(),
    ok = start_otp_node_application(),
    true = global:del_lock({init, node()}, [node()]),
    ?LOG_INFO("OTP node started", []).

-spec init_otp_node_logger() -> ok.
init_otp_node_logger() ->
    case application:get_env(otp_node, logger_ready) of
        {ok, true} ->
            ok;
        _ ->
            {ok, LogDir} = esrv_config:get_value(log_dir),
            {ok, LogLevel} = esrv_config:get_value(log_level),
            {ok, LogOtp} = esrv_config:get_value(log_otp),
            ok = logger:set_primary_config(level, LogLevel),
            [ ok = logger:remove_handler(HId) || HId <- logger:get_handler_ids() ],
            ok = logger:add_handler(server, logger_disk_log_h,
                                    get_otp_node_logger_config(LogDir, LogLevel, LogOtp)),
            ok = application:set_env(otp_node, logger_ready, true)
    end.

-spec get_otp_node_logger_config() -> logger:handler_config().
get_otp_node_logger_config() ->
    LogDir = default_log_dir(),
    get_otp_node_logger_config(LogDir, ?OTP_NODE_LOG_LEVEL, stop).

-spec get_otp_node_logger_config(LogDir :: file:filename(),
                                 LogLevel :: logger:level(),
                                 LogOtp :: log | stop) -> logger:handler_config().
get_otp_node_logger_config(LogDir, LogLevel, LogOtp) ->
    MessageTemplate =
        if
            LogLevel =:= debug orelse LogLevel =:= all ->
                [time, " [", level, "] ",
                 pid, " ", mfa, ":", line, " ", msg, "\n"];
            true ->
                [time, " [", level, "] ",
                 pid, " ", msg, "\n"]
        end,
    #{config => #{file => get_otp_node_log_dir(LogDir),
                  max_no_files => 5,
                  max_no_bytes => 8388608,
                  filesync_repeat_interval => 3000},
      filters => [{otp, {fun logger_filters:domain/2,
                         {LogOtp, sub, [otp, sasl]}}}],
      formatter => {logger_formatter,
                    #{template => MessageTemplate,
                      time_designator => $ }}}.

-spec get_otp_node_log_dir(LogDir :: file:filename()) -> file:filename().
get_otp_node_log_dir(LogDir) ->
    OtpName = esrv_lib:append_release_and_distr_mode("otp"),
    filename:join([LogDir, OtpName, "server.log"]).

-spec start_otp_node_application() -> ok.
start_otp_node_application() ->
    ok = esrv_db:initialize(),
    ok = generate_otp_node_app_file(),
    {ok, _} = application:ensure_all_started(otp_node, permanent),
    ok.

-spec generate_otp_node_app_file() -> ok.
generate_otp_node_app_file() ->
    DirPath = filename:join([esrv_lib:bdir_user_cache(), "otp_node", node()]),
    DirName = esrv_lib:path_to_file(DirPath),
    AppTerm =
        {application, otp_node,
         [
          {description, "OTP modules handler"},
          {vsn, "1.0.0"},
          {registered, []},
          {mod, {esrv_otp_node_app, []}},
          {applications, [kernel,
                          stdlib]},
          {env, []},
          {modules, []},
          {maintainers, []},
          {licenses, ["Apache 2.0"]},
          {links, []}
         ]},
    AppFile = filename:join(DirName, "otp_node.app"),
    ok = filelib:ensure_dir(AppFile),
    ok = file:write_file(AppFile, io_lib:format("~p.", [AppTerm])),
    true = code:add_path(DirName),
    ok.
