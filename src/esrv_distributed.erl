-module(esrv_distributed).

-include("log.hrl").

%% API
-export([initialize/0]).

-type distr_mode() :: shortnames | longnames.

%%==============================================================================
%% API
%%==============================================================================
-spec initialize() -> ok.
initialize() ->
    DistrMode = get_distr_mode(),
    Node = get_node(DistrMode),
    spawn_link(fun() -> os:cmd("epmd -daemon") end),
    {ok, _} = erl_epmd:names(),
    {ok, Pid} = net_kernel:start([Node, DistrMode]),
    ?LOG_INFO("Distribution enabled: ~p; mode: ~p; node: ~p", [Pid, DistrMode, Node]).

-spec get_distr_mode() -> distr_mode().
get_distr_mode() ->
    {ok, DistrMode} = esrv_config:get_value(distr_mode),
    if
        DistrMode =:= <<"shortnames">> orelse DistrMode =:= <<"longnames">> ->
            binary_to_atom(DistrMode, utf8);
        true ->
            ?LOG_WARNING("Incorrect distribution mode: ~ts; assuming 'shortnames'", [DistrMode]),
            shortnames
    end.

-spec get_node(DistrMode :: distr_mode()) -> node().
get_node(DistrMode) ->
    {ok, ProjPath} = esrv_config:get_value(proj_path),
    ProjName = filename:basename(ProjPath),
    <<HashHead:32, _/binary>> = crypto:hash(sha, ProjPath),
    NodeName = iolist_to_binary(io_lib:format("esrv_~s_~8.16.0b", [ProjName, HashHead])),
    NodeHost = case DistrMode of longnames -> <<"@127.0.0.1">>; shortnames -> <<>> end,
    binary_to_atom(<<NodeName/binary, NodeHost/binary>>, utf8).
