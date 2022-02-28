-module(esrv_diagnostics).

-include("types.hrl").
-include("diagnostics.hrl").

-callback init(Options :: map()) -> NewOptions :: map().

-callback run(TargetUri :: uri(),
              AppPath :: path(),
              ModuleType :: module_type(),
              Options :: map()) -> [diagnostic()].

-optional_callbacks([init/1]).
