-module(erlang_srv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: [any()]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
                  #{id => esrv_man_mgr,
                    start => {esrv_man_mgr, start_link, []}},
                  #{id => esrv_cache_mgr,
                    start => {esrv_cache_mgr, start_link, []}},
                  #{id => esrv_progress_srv,
                    start => {esrv_progress_srv, start_link, []}},
                  #{id => esrv_dynamic_sup,
                    start => {esrv_dynamic_sup, start_link, []},
                    type => supervisor},
                  #{id => esrv_diagnostics_sup,
                    start => {esrv_diagnostics_sup, start_link, []},
                    type => supervisor},
                  #{id => esrv_main_fsm,
                    start => {esrv_main_fsm, start_link, []}},
                  #{id => esrv_index_mgr,
                    start => {esrv_index_mgr, start_link, []}},
                  #{id => esrv_client_gw,
                    start => {esrv_client_gw, start_link, []}}
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
