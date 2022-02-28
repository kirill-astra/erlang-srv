-module(esrv_req_td_document_highlight).

-behaviour(esrv_request_processor).

%% API
-export([process/1]).

-include("types.hrl").
-include("parser.hrl").
-include("protocol.hrl").
-include("records.hrl").

-type item() :: [{location(), poi()}].

-spec process(Request :: request()) -> {response, #response{}}.
process(#request{id = Id, params = #{<<"position">> := #{<<"line">> := Line,
                                                         <<"character">> := Character},
                                     <<"textDocument">> := #{<<"uri">> := Uri}}}) ->
    {ok, ModuleData} = esrv_index_mgr:get_current_module_data(Uri),
    Items0 =
        case esrv_req_lib:get_poi({Line + 1, Character + 1}, Uri, ModuleData) of
            {ok, #poi{data = PoiData, definition = Definition}}
              when element(1, PoiData) =:= macro orelse
                   element(1, PoiData) =:= module orelse
                   element(1, PoiData) =:= variable orelse
                   element(1, PoiData) =:= record orelse
                   element(1, PoiData) =:= field orelse
                   element(1, PoiData) =:= local_type orelse
                   element(1, PoiData) =:= local_type_name orelse
                   element(1, PoiData) =:= remote_type orelse
                   element(1, PoiData) =:= remote_type_name orelse
                   element(1, PoiData) =:= local_spec orelse
                   element(1, PoiData) =:= remote_spec orelse
                   element(1, PoiData) =:= function_clause orelse
                   element(1, PoiData) =:= local_function orelse
                   element(1, PoiData) =:= local_function_name orelse
                   element(1, PoiData) =:= remote_function orelse
                   element(1, PoiData) =:= remote_function_name orelse
                   element(1, PoiData) =:= local_type_or_function orelse
                   element(1, PoiData) =:= local_type_or_function_name orelse
                   element(1, PoiData) =:= remote_type_or_function orelse
                   element(1, PoiData) =:= remote_type_or_function_name ->
                lists:foldl(fun({EndLocation, Poi}, Acc) ->
                                    case check_poi(Definition, PoiData, Poi) of
                                        true ->
                                            [{EndLocation, Poi} | Acc];
                                        false ->
                                            Acc
                                    end
                            end, [], esrv_req_lib:collect_pois(Uri, ModuleData));
            _ ->
                []
        end,
    Items1 = lists:usort(Items0),
    {response, #response{id = Id, result = format_items(Items1)}}.

-spec check_poi(TargetDefinition :: poi_definition(), PoiData :: poi_data(), Poi :: poi()) ->
          boolean().
check_poi(TargetDefinition, PoiData, #poi{data = PoiData, definition = Definition})
  when element(1, PoiData) =:= variable  ->
    Definition =:= TargetDefinition;
check_poi(_, PoiData, Poi) ->
    check_poi(PoiData, Poi).

-spec check_poi(TargetPoiData :: poi_data(), Poi :: poi()) -> boolean().
check_poi({local_type_name, Name}, #poi{data = {local_type, {Name, _}}}) ->
    true;
check_poi({remote_type_name, ModuleName, Name},
          #poi{data = {remote_type, ModuleName, {Name, _}}}) ->
    true;
check_poi({local_spec, NameArity}, #poi{data = {Tag, NameArity}})
  when Tag =:= local_function orelse Tag =:= function_clause ->
    true;
check_poi({local_function, NameArity}, #poi{data = {Tag, NameArity}})
  when Tag =:= local_spec orelse Tag =:= function_clause ->
    true;
check_poi({function_clause, NameArity}, #poi{data = {Tag, NameArity}})
  when Tag =:= local_spec orelse Tag =:= local_function ->
    true;
check_poi({local_function_name, Name}, #poi{data = {Tag, {Name, _}}})
  when Tag =:= local_spec orelse Tag =:= local_function orelse Tag =:= function_clause ->
    true;
check_poi({remote_function_name, ModuleName, Name},
          #poi{data = {remote_function, ModuleName, {Name, _}}}) ->
    true;
check_poi({local_type_or_function, NameArity}, #poi{data = {Tag, NameArity}})
  when Tag =:= local_type orelse
       Tag =:= local_spec orelse
       Tag =:= local_function orelse
       Tag =:= function_clause ->
    true;
check_poi({local_type_or_function_name, Name}, #poi{data = {Tag, {Name, _}}})
  when Tag =:= local_type orelse
       Tag =:= local_spec orelse
       Tag =:= local_function orelse
       Tag =:= function_clause ->
    true;
check_poi({remote_type_or_function, ModuleName, NameArity},
          #poi{data = {Tag, ModuleName, NameArity}})
  when Tag =:= remote_type orelse Tag =:= remote_function ->
    true;
check_poi({remote_type_or_function_name, ModuleName, {Name, _}},
          #poi{data = {Tag, ModuleName, {Name, _}}})
  when Tag =:= remote_type orelse Tag =:= remote_function ->
    true;
check_poi(PoiData, #poi{data = PoiData}) ->
    true;
check_poi(_, _) ->
    false.

-spec format_items(Items :: [item()]) -> jsx:json_term().
format_items([]) ->
    null;
format_items(Items) ->
    lists:map(fun({EndLocation, #poi{start_location = StartLocation} = Poi}) ->
                      [{<<"range">>, esrv_lib:format_range(StartLocation, EndLocation)},
                       {<<"kind">>, poi_kind(Poi)}]
              end, Items).

-spec poi_kind(Poi :: poi()) -> integer().
poi_kind(#poi{data = PoiData, start_location = Location, definition = {local, Location}})
  when element(1, PoiData) =:= module orelse
       element(1, PoiData) =:= variable orelse
       element(1, PoiData) =:= record orelse
       element(1, PoiData) =:= field orelse
       element(1, PoiData) =:= local_type orelse
       element(1, PoiData) =:= local_function ->
    ?HIGHLIGHT_KIND_WRITE;
poi_kind(#poi{data = PoiData})
  when element(1, PoiData) =:= local_spec orelse
       element(1, PoiData) =:= function_clause ->
    ?HIGHLIGHT_KIND_WRITE;
poi_kind(#poi{data = PoiData})
  when element(1, PoiData) =:= macro orelse
       element(1, PoiData) =:= module orelse
       element(1, PoiData) =:= variable orelse
       element(1, PoiData) =:= record orelse
       element(1, PoiData) =:= field orelse
       element(1, PoiData) =:= local_type orelse
       element(1, PoiData) =:= local_type_name orelse
       element(1, PoiData) =:= remote_type orelse
       element(1, PoiData) =:= remote_type_name orelse
       element(1, PoiData) =:= local_function orelse
       element(1, PoiData) =:= local_function_name orelse
       element(1, PoiData) =:= remote_function orelse
       element(1, PoiData) =:= remote_function_name orelse
       element(1, PoiData) =:= local_type_or_function orelse
       element(1, PoiData) =:= local_type_or_function_name orelse
       element(1, PoiData) =:= remote_type_or_function orelse
       element(1, PoiData) =:= remote_type_or_function_name ->
    ?HIGHLIGHT_KIND_READ;
poi_kind(_) ->
    ?HIGHLIGHT_KIND_TEXT.
