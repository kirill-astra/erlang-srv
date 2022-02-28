-module(erlang_srv).

-export([main/1]).

-include("log.hrl").

-spec main(Args :: [any()]) -> ok.
main(Args) ->
    ok = parse_args(Args),
    ok = init_logger(),
    ok = esrv_config:create(),
    {ok, _} = application:ensure_all_started(erlang_srv, permanent),
    ?LOG_INFO("Server started", []),
    receive _ -> ok end.

-spec parse_args(Args :: [string()]) -> ok.
parse_args(Args) ->
    application:load(erlang_srv),
    OptSpec =
        [{version, $v, "version", undefined,
          "Print the current version of Erlang Server and exit"},
         {transport, $t, "transport", {string, "stdio"},
          "Specifies the transport the server will use for "
          "the connection with the client, either 'stdio' or 'tcp'"},
         {port, $p, "port", {integer, 10000},
          "Specifies the port used for 'tcp' transport"},
         {log_dir, $d, "log-dir", {string, default_log_dir()},
          "Specifies the logs directory"},
         {log_level, $l, "log-level", {string, "info"},
          "Specifies the log level. Possible values: "
          "'emergency', 'alert', 'critical', 'error', "
          "'warning', 'notice', 'info', 'debug' "
          "and two special values 'all' and 'none'"},
         {log_otp, $o, "log-otp", {string, "stop"},
          "Specifies the filter action for otp/sasl log events, "
          "either 'log' or 'stop'"}],
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
                              application:set_env(erlang_srv, log_dir, LogDir);
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
                              application:set_env(erlang_srv, log_level, list_to_atom(LogLevel));
                         ({log_otp, LogOtp}) when LogOtp =:= "log";
                                                  LogOtp =:= "stop" ->
                              application:set_env(erlang_srv, log_otp, list_to_atom(LogOtp))
                      end, ParsedArgs)
    catch
        _:_ ->
            getopt:usage(OptSpec, "erlang_srv"),
            halt(1)
    end.

-spec default_log_dir() -> file:filename().
default_log_dir() ->
    UserLogDir = esrv_lib:bdir_user_log(),
    {ok, CurrentDir} = file:get_cwd(),
    CurrentDirName = filename:basename(CurrentDir),
    binary_to_list(filename:join(UserLogDir, CurrentDirName)).

-spec init_logger() -> ok.
init_logger() ->
    {ok, LogDir} = application:get_env(erlang_srv, log_dir),
    {ok, LogLevel} = application:get_env(erlang_srv, log_level),
    {ok, LogOtp} = application:get_env(erlang_srv, log_otp),
    ok = logger:set_primary_config(level, LogLevel),
    [ ok = logger:remove_handler(HId) || HId <- logger:get_handler_ids() ],
    CheckTraceFilter =
        fun(#{meta := Meta} = LogEvent, [Expected]) ->
                case maps:get(trace, Meta, false) of
                    Expected -> LogEvent;
                    _ -> stop
                end
        end,
    MessageTemplate =
        if
            LogLevel =:= debug orelse LogLevel =:= all ->
                [time, " [", level, "] ", pid, " ", mfa, ":", line, " ", msg, "\n"];
            true ->
                [time, " [", level, "] ", pid, " ", msg, "\n"]
        end,
    logger:add_handler(server, logger_disk_log_h,
                       #{config => #{file => filename:join(LogDir, "server.log"),
                                     max_no_files => 5,
                                     max_no_bytes => 8388608,
                                     filesync_repeat_interval => 3000},
                         filters => [{check_trace, {CheckTraceFilter, [false]}},
                                     {otp, {fun logger_filters:domain/2,
                                            {LogOtp, sub, [otp, sasl]}}}],
                         formatter => {logger_formatter, #{template => MessageTemplate,
                                                           time_designator => $ }}}),
    TraceTemplate = [time, " [trace] [", type, "] ", msg, "\n"],
    logger:add_handler(traces, logger_disk_log_h,
                       #{config => #{file => filename:join(LogDir, "traces.log"),
                                     max_no_files => 5,
                                     max_no_bytes => 8388608,
                                     filesync_repeat_interval => 3000},
                         filters => [{check_trace, {CheckTraceFilter, [true]}}],
                         formatter => {logger_formatter, #{single_line => false,
                                                           template => TraceTemplate,
                                                           time_designator => $ }}}).
