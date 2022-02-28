-module(esrv_diagnostic_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_worker/2,
         stop_worker/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_worker(Module :: module(), Options :: map()) -> {ok, pid()}.
start_worker(Module, Options) ->
    supervisor:start_child(?SERVER, [Module, Options]).

-spec stop_worker(Worker :: pid()) -> ok.
stop_worker(Worker) ->
    supervisor:terminate_child(?SERVER, Worker).

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
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
                  #{id => id,
                    start => {esrv_diagnostic_worker, start_link, []},
                    restart => transient,
                    shutdown => brutal_kill}
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
