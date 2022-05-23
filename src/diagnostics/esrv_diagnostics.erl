-module(esrv_diagnostics).

-include("types.hrl").
-include("diagnostics.hrl").

-callback init(Options :: map()) -> NewOptions :: map().

-callback run(TargetUri :: uri(),
              AppId :: app_id() | undefined,
              Options :: map()) -> [diagnostic()].

-optional_callbacks([init/1]).
