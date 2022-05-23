-module(esrv_dialyzer_diagnostics).

%% API
-export([init/1,
         run/3]).

-behaviour(esrv_diagnostics).

-include("types.hrl").
-include("records.hrl").
-include("diagnostics.hrl").

-define(SOURCE, <<"dialyzer">>).

-spec init(Options :: map()) -> NewOptions :: map().
init(Options) ->
    ChildSpec = #{id => esrv_dialyzer_mgr,
                  start => {esrv_dialyzer_mgr, start_link, []}},
    {ok, _} = esrv_dynamic_sup:start_child(ChildSpec),
    Options.

-spec run(Uri :: uri(), AppId :: app_id() | undefined, Options :: map()) -> [diagnostic()].
run(_, undefined, _) ->
    [];
run(Uri, AppId, Options) ->
    case esrv_lib:get_app_type(AppId) of
        {ok, AppType} when AppType =:= proj orelse AppType =:= sub_proj ->
            do_run(Uri, AppId, Options);
        _ ->
            []
    end.

-spec do_run(Uri :: uri(), AppId :: app_id() | undefined, Options :: map()) -> [diagnostic()].
do_run(Uri, AppId, Options) ->
    Filename = esrv_lib:uri_to_file(Uri),
    case filename:extension(Filename) of
        ".erl" ->
            case get_plts(Options) of
                {ok, Plts} ->
                    {_, TargetUris} = get_target_uris(Uri, [], true, {sets:new(), []}),
                    Info =
                        [{files, lists:map(fun esrv_lib:uri_to_file/1, TargetUris)},
                         {from, src_code},
                         {include_dirs, esrv_lib:includes(AppId)},
                         {plts, lists:map(fun esrv_lib:path_to_file/1, Plts)},
                         {defines, esrv_lib:defines()}],
                    try
                        Warnings = dialyzer:run(Info),
                        lists:map(fun({_, {_, Line}, _} = Warning) ->
                                          #diagnostic{position = {line, Line},
                                                      severity = ?DIAGNOSTIC_WARNING,
                                                      source = ?SOURCE,
                                                      message = format_message(Warning)}
                                  end, Warnings)
                    catch
                        _:_ ->
                            []
                    end;
                not_ready ->
                    [];
                {error, Error} ->
                    [#diagnostic{position = {line, 1},
                                 severity = ?DIAGNOSTIC_ERROR,
                                 source = ?SOURCE,
                                 message = Error}]
            end;
        _ ->
            []
    end.

-spec get_plts(Options :: map()) -> {ok, [path()]} | not_ready | {error, binary()}.
get_plts(#{<<"plts">> := Plts}) ->
    {ok, Plts};
get_plts(_) ->
    esrv_dialyzer_mgr:get_plts().

-type target_uris_acc() :: {sets:set(uri()), [uri()]}.

-spec get_target_uris(Uri :: uri(),
                      DepsChain :: [uri()],
                      IsTarget :: boolean(),
                      Acc :: target_uris_acc()) -> target_uris_acc().
get_target_uris(Uri, DepsChain, true, {IncludedSet, TargetUris0}) ->
    get_target_uris(Uri, DepsChain, false, {IncludedSet, [Uri | TargetUris0]});
get_target_uris(Uri, DepsChain, false, Acc0) ->
    case lists:member(Uri, DepsChain) of
        false ->
            case esrv_db:get_module_data(Uri) of
                {ok, #module_data{behaviors = Behaviors,
                                  include_data = #include_data{resolved = Resolved}}} ->
                    BehaviorModules = maps:keys(Behaviors),
                    Acc1 = process_behavior_modules(BehaviorModules, [Uri | DepsChain], Acc0),
                    IncludedUris = maps:keys(Resolved),
                    process_included_uris(IncludedUris, [Uri | DepsChain], Acc1);
                undefined ->
                    Acc0
            end;
        true ->
            Acc0
    end.

-spec process_behavior_modules(Modules :: [module()],
                               DepsChain :: [uri()],
                               Acc :: target_uris_acc()) -> target_uris_acc().
process_behavior_modules([], _, Acc) ->
    Acc;
process_behavior_modules([Module | T], DepsChain, Acc0) ->
    Acc1 =
        case esrv_db:read_module_meta_by_name(Module) of
            [#module_meta{uri = Uri, app_type = AppType} | _] when AppType =/= otp ->
                get_target_uris(Uri, DepsChain, true, Acc0);
            _ ->
                Acc0
        end,
    process_behavior_modules(T, DepsChain, Acc1).

-spec process_included_uris(IncludedUris :: [uri()],
                            DepsChain :: [uri()],
                            Acc :: target_uris_acc()) -> target_uris_acc().
process_included_uris([], _, Acc) ->
    Acc;
process_included_uris([IncludedUri | T], DepsChain, {IncludedSet0, TargetUris} = Acc0) ->
    case sets:is_element(IncludedUri, IncludedSet0) of
        false ->
            Acc1 = {sets:add_element(IncludedUri, IncludedSet0), TargetUris},
            get_target_uris(IncludedUri, DepsChain, false, Acc1);
        true ->
            process_included_uris(T, DepsChain, Acc0)
    end.

-spec format_message(Warning :: tuple()) -> binary().
format_message(Warning) ->
    Message = dialyzer:format_warning(Warning),
    re:replace(Message, <<"^.*?: ">>, <<"">>, [{return, binary}]).
