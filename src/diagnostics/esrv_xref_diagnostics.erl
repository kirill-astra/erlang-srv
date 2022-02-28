-module(esrv_xref_diagnostics).

-behaviour(esrv_diagnostics).

%% API
-export([run/4]).

-include("types.hrl").
-include("records.hrl").
-include("diagnostics.hrl").

-define(SOURCE, <<"xref">>).

-spec run(Uri :: uri(), AppPath :: path(), ModuleType :: module_type(), Options :: map()) ->
          [diagnostic()].
run(_, _, otp, _) ->
    [];
run(Uri, _, _, Options) ->
    case esrv_lib:get_module_data(Uri) of
        {ok, ModuleData} ->
            #module_data{types = Types, functions = Functions, zones = Zones} = ModuleData,
            {Diagnostics, Dependencies} =
                lists:foldl(fun({_, #poi{data = Data, start_location = StartLocation}},
                                {DiagnosticsAcc, DependenciesAcc}) ->
                                    case Data of
                                        {local_type, NameArity}
                                          when not is_map_key(NameArity, Types) ->
                                            {local_type(NameArity, StartLocation,
                                                        Zones, DiagnosticsAcc),
                                             DependenciesAcc};
                                        {remote_type, ModuleName, NameArity} ->
                                            {remote_type(ModuleName, NameArity, StartLocation,
                                                         Options, DiagnosticsAcc),
                                             sets:add_element(ModuleName, DependenciesAcc)};
                                        {local_function, NameArity}
                                          when not is_map_key(NameArity, Functions) ->
                                            {local_function(NameArity, StartLocation,
                                                            Zones, DiagnosticsAcc),
                                             DependenciesAcc};
                                        {Tag, ModuleName, NameArity}
                                          when Tag =:= remote_spec orelse Tag =:= remote_function ->
                                            {remote_function(ModuleName, NameArity, StartLocation,
                                                             Options, DiagnosticsAcc),
                                             sets:add_element(ModuleName, DependenciesAcc)};
                                        _ ->
                                            {DiagnosticsAcc, DependenciesAcc}
                                    end
                            end, {[], sets:new()}, esrv_req_lib:collect_pois(Uri, ModuleData)),
            TargetModuleName = get_target_module_name(Uri, ModuleData),
            lists:foreach(fun(ModuleName) when ModuleName =/= TargetModuleName ->
                                  case esrv_db:read_module_meta_by_name(ModuleName) of
                                      [#module_meta{module_type = proj}] ->
                                          esrv_diagnostics_srv:add_dependency(ModuleName, Uri);
                                      _ ->
                                          ok
                                  end;
                             (_) ->
                                  ok
                          end, sets:to_list(Dependencies)),
            Diagnostics;
        undefined ->
            []
    end.

-spec get_target_module_name(Uri :: uri(), ModuleData :: module_data()) -> module() | undefined.
get_target_module_name(Uri, ModuleData) ->
     case esrv_req_lib:get_module_name(Uri, ModuleData) of
         {ok, {_, {ModuleName, _}}} ->
             ModuleName;
         undefined ->
             undefined
     end.

-spec local_type(NameArity :: name_arity(),
                 Location :: location(),
                 Zones :: [zone()],
                 Acc :: [diagnostic()]) -> [diagnostic()].
local_type({Name, Arity}, {Line, _} = Location, Zones, Acc) ->
    case esrv_req_lib:get_zone(Location, Zones) of
        {ok, #zone{type = export_type}} ->
            MessageData = io_lib:format("no definition for ~p/~p type",
                                        [Name, Arity]),
            [diagnostic(Line, MessageData) | Acc];
        _ ->
            Acc
    end.

-spec remote_type(ModuleName :: module(),
                  NameArity :: name_arity(),
                  Location :: location(),
                  Options :: map(),
                  Acc :: [diagnostic()]) -> [diagnostic()].
remote_type(ModuleName, {Name, Arity}, {Line, _}, Options, Acc) ->
    case esrv_req_lib:get_remote_type(ModuleName, {Name, Arity}) of
        {ok, _} ->
            Acc;
        undefined ->
            case is_type_to_skip(ModuleName, Name, Arity, Options) of
                true ->
                    Acc;
                false ->
                    MessageData = io_lib:format("no definition for ~p:~p/~p type",
                                                [ModuleName, Name, Arity]),
                    [diagnostic(Line, MessageData) | Acc]
            end
    end.

-spec local_function(NameArity :: name_arity(),
                     Location :: location(),
                     Zones :: [zone()],
                     Acc :: [diagnostic()]) -> [diagnostic()].
local_function({Name, Arity}, {Line, _} = Location, Zones, Acc) ->
    case esrv_req_lib:get_zone(Location, Zones) of
        {ok, #zone{type = export}} ->
            MessageData = io_lib:format("no definition for ~p/~p function",
                                        [Name, Arity]),
            [diagnostic(Line, MessageData) | Acc];
        _ ->
            Acc
    end.

-spec remote_function(ModuleName :: module(),
                      NameArity :: name_arity(),
                      Location :: location(),
                      Options :: map(),
                      Acc :: [diagnostic()]) -> [diagnostic()].
remote_function(erlang, {module_info, 0}, _, _, Acc) ->
    Acc;
remote_function(erlang, {module_info, 1}, _, _, Acc) ->
    Acc;
remote_function(ModuleName, {Name, Arity}, {Line, _}, Options, Acc) ->
    case esrv_req_lib:get_remote_function(ModuleName, {Name, Arity}) of
        {ok, _} ->
            Acc;
        undefined ->
            case is_function_to_skip(ModuleName, Name, Arity, Options) of
                true ->
                    Acc;
                false ->
                    MessageData = io_lib:format("no definition for ~p:~p/~p function",
                                                [ModuleName, Name, Arity]),
                    [diagnostic(Line, MessageData) | Acc]
            end
    end.

-spec is_type_to_skip(ModuleName :: module(),
                      Name :: name(),
                      Arity :: arity(),
                      Options :: map()) -> boolean().
is_type_to_skip(ModuleName, Name, Arity, #{<<"toSkip">> := ToSkip}) ->
    is_object_to_skip(<<"types">>, ModuleName, Name, Arity, ToSkip).

-spec is_function_to_skip(ModuleName :: module(),
                          Name :: name(),
                          Arity :: arity(),
                          Options :: map()) -> boolean().
is_function_to_skip(ModuleName, Name, Arity, #{<<"toSkip">> := ToSkip}) ->
    is_object_to_skip(<<"functions">>, ModuleName, Name, Arity, ToSkip).

-spec is_object_to_skip(ObjectsTag :: binary(),
                        ModuleName :: module(),
                        Name :: name(),
                        Arity :: arity(),
                        ToSkip :: [map()]) -> boolean().
is_object_to_skip(ObjectsTag, ModuleName, Name, Arity, ToSkip) ->
    Target = atom_to_binary(ModuleName, utf8),
    RegExp = io_lib:format("^~p(/~p)?$", [Name, Arity]),
    lists:any(fun(#{<<"module">> := Module, ObjectsTag := Objects}) when Module =:= Target ->
                      lists:any(fun(Object) -> re:run(Object, RegExp) =/= nomatch end, Objects);
                 (#{<<"module">> := Module}) when Module =:= Target ->
                      true;
                 (_) ->
                      false
              end, ToSkip).

-spec diagnostic(Line :: line(), MessageData :: iodata()) -> diagnostic().
diagnostic(Line, MessageData) ->
    #diagnostic{position = {line, Line},
                severity = ?DIAGNOSTIC_ERROR,
                source = ?SOURCE,
                message = iolist_to_binary(MessageData)}.
