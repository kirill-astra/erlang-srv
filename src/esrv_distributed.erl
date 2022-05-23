-module(esrv_distributed).

-include("types.hrl").
-include("log.hrl").

%% API
-export([initialize_ls/0,
         initialize_otp_node/0,
         get_ls_name/0,
         get_otp_node_name/0]).

%%==============================================================================
%% API
%%==============================================================================
-spec initialize_ls() -> ok.
initialize_ls() ->
    Node = get_ls_name(),
    ok = start_distributed(Node).

-spec initialize_otp_node() -> ok.
initialize_otp_node() ->
    Node = get_otp_node_name(),
    ok = start_distributed(Node).

-spec start_distributed(Node :: node()) -> ok.
start_distributed(Node) ->
    DistrMode = esrv_lib:get_distr_mode(),
    os:cmd("epmd -daemon"),
    {ok, _} = erl_epmd:names(),
    {ok, Pid} = net_kernel:start([Node, DistrMode]),
    ?LOG_INFO("Distribution enabled: ~p; mode: ~p; node: '~s'", [Pid, DistrMode, Node]).

-spec get_ls_name() -> node().
get_ls_name() ->
    {ok, ProjPath} = esrv_config:get_value(proj_path),
    ProjName = filename:basename(ProjPath),
    <<HashHead:32, _/binary>> = crypto:hash(sha, ProjPath),
    NodeName = iolist_to_binary(io_lib:format("esrv_~s_~8.16.0b", [ProjName, HashHead])),
    get_name(NodeName).

-spec get_otp_node_name() -> node().
get_otp_node_name() ->
    NodeName = list_to_binary(esrv_lib:append_release_and_distr_mode("otp_node")),
    get_name(NodeName).

-spec get_name(NodeName :: binary()) -> node().
get_name(NodeName) ->
    NodeHost =
        case esrv_lib:get_distr_mode() of
            longnames ->
                <<"@127.0.0.1">>;
            shortnames ->
                case is_alive() of
                    true ->
                        case binary:split(atom_to_binary(node(), utf8), <<"@">>) of
                            [_, Hostname] ->
                                <<"@", Hostname/binary>>;
                            _ ->
                                <<>>
                        end;
                    false ->
                        <<>>
                end
        end,
    binary_to_atom(<<NodeName/binary, NodeHost/binary>>, utf8).
