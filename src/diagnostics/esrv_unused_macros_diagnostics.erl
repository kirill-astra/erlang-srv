-module(esrv_unused_macros_diagnostics).

%% API
-export([run/3]).

-behaviour(esrv_diagnostics).

-include("types.hrl").
-include("records.hrl").
-include("diagnostics.hrl").

-define(SOURCE, <<"unused_macros">>).

-record(macro_info, {location :: location(),
                     used :: boolean()}).

-spec run(Uri :: uri(), AppId :: app_id() | undefined, Options :: map()) -> [diagnostic()].
run(_, undefined, _) ->
    [];
run(Uri, AppId, Options) ->
    case esrv_lib:get_app_type(AppId) of
        {ok, AppType} when AppType =:= proj orelse AppType =:= sub_proj ->
            do_run(Uri, Options);
        _ ->
            []
    end.

-spec do_run(Uri :: uri(), Options :: map()) -> [diagnostic()].
do_run(Uri, Options) ->
    {ModuleName, Pois} = get_uri_data(Uri),
    MacrosInfo =
        lists:foldl(fun(#poi{data = {macro, MacroId},
                             start_location = Location,
                             definition = {local, Location}}, Acc) ->
                            Acc#{MacroId => #macro_info{location = Location, used = false}};
                       (#poi{data = {macro, MacroId}}, Acc)
                          when is_map_key(MacroId, Acc) ->
                            maps:update(MacroId,
                                        fun(MacroInfo) ->
                                                MacroInfo#macro_info{used = true}
                                        end, Acc);
                       (_, Acc) ->
                            Acc
                    end, #{}, Pois),
    lists:filtermap(fun({MacroId, #macro_info{location = {Line, _}, used = false}}) ->
                            FormattedMacroId = esrv_req_lib:format_macro_id(MacroId),
                            case is_macro_to_skip(ModuleName, FormattedMacroId, Options) of
                                true ->
                                    false;
                                false ->
                                    {true, #diagnostic{position = {line, Line},
                                                       severity = ?DIAGNOSTIC_WARNING,
                                                       source = ?SOURCE,
                                                       message = iolist_to_binary([<<"macro ">>,
                                                                                   FormattedMacroId,
                                                                                   <<" unused">>])}}
                            end;
                       (_) ->
                            false
                    end, maps:to_list(MacrosInfo)).

-spec get_uri_data(Uri :: uri()) -> {module() | undefined, [poi()]}.
get_uri_data(Uri) ->
    case filename:extension(Uri) of
        <<".erl">> ->
            case esrv_db:get_module_data(Uri) of
                {ok, #module_data{module_name = ModuleName, pois = {GbTree, _}}} ->
                    {case ModuleName of {Module, _} -> Module; undefined -> undefined end,
                     lists:map(fun({_, Poi}) -> Poi end, gb_trees:to_list(GbTree))};
                undefined ->
                    {undefined, []}
            end;
        _ ->
            {undefined, []}
    end.

-spec is_macro_to_skip(ModuleName :: module() | undefined,
                       FormattedMacroId :: binary(),
                       Options :: map()) -> boolean().
is_macro_to_skip(ModuleName, FormattedMacroId, #{<<"toSkip">> := ToSkip})
  when ModuleName =/= undefined ->
    Target = atom_to_binary(ModuleName, utf8),
    lists:any(fun(#{<<"module">> := Module, <<"macros">> := Macros}) when Module =:= Target ->
                      lists:member(FormattedMacroId, Macros);
                 (#{<<"module">> := Module}) when Module =:= Target ->
                      true;
                 (_) ->
                      false
              end, ToSkip);
is_macro_to_skip(_, _, _) ->
    false.
