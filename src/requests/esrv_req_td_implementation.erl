-module(esrv_req_td_implementation).

-behaviour(esrv_request_processor).

%% API
-export([process/1]).

-include("types.hrl").
-include("parser.hrl").
-include("records.hrl").

-spec process(Request :: request()) -> {response, #response{}}.
process(#request{id = Id, params = #{<<"position">> := #{<<"line">> := Line,
                                                         <<"character">> := Character},
                                     <<"textDocument">> := #{<<"uri">> := Uri}}}) ->
    {ok, ModuleData} = esrv_index_mgr:get_current_module_data(Uri),
    Result =
        case esrv_req_lib:get_poi({Line + 1, Character + 1}, Uri, ModuleData) of
            {ok, #poi{data = {remote_function, ModuleName, NameArity}}} ->
                case target_name_arity(ModuleName, NameArity) of
                    {ok, TargetNameArity} ->
                        case target_poi(TargetNameArity, Uri, ModuleData) of
                            {ok, Endlocation, #poi{start_location = StartLocation}} ->
                                esrv_req_lib:format_location(Uri, StartLocation, Endlocation);
                            undefined ->
                                null
                        end;
                    undefined ->
                        null
                end;
            _ ->
                null
        end,
    {response, #response{id = Id, result = Result}}.

-spec target_name_arity(ModuleName :: module(), NameArity :: name_arity()) ->
          {ok, name_arity()} | undefined.
target_name_arity(gen_event, NameArity) ->
    case NameArity of
        {add_handler, 3} -> {ok, {init, 1}};
        {add_sup_handler, 3} -> {ok, {init, 1}};
        {call, 3} -> {ok, {handle_call, 2}};
        {call, 4} -> {ok, {handle_call, 2}};
        {delete_handler, 1} -> {ok, {terminate, 2}};
        {notify, 2} -> {ok, {handle_event, 2}};
        {sync_notify, 2} -> {ok, {handle_event, 2}};
        {stop, 1} -> {ok, {terminate, 2}};
        {stop, 3} -> {ok, {terminate, 2}};
        _ -> undefined
    end;
target_name_arity(gen_server, NameArity) ->
    case NameArity of
        {abcast, 2} -> {ok, {handle_cast, 2}};
        {abcast, 3} -> {ok, {handle_cast, 2}};
        {call, 2} -> {ok, {handle_call, 3}};
        {call, 3} -> {ok, {handle_call, 3}};
        {cast, 2} -> {ok, {handle_cast, 2}};
        {multi_call, 2} -> {ok, {handle_call, 3}};
        {multi_call, 3} -> {ok, {handle_call, 3}};
        {multi_call, 4} -> {ok, {handle_call, 3}};
        {start, 3} -> {ok, {init, 1}};
        {start, 4} -> {ok, {init, 1}};
        {start_link, 3} -> {ok, {init, 1}};
        {start_link, 4} -> {ok, {init, 1}};
        {stop, 1} -> {ok, {terminate, 2}};
        {stop, 3} -> {ok, {terminate, 2}};
        _ -> undefined
    end;
target_name_arity(gen_statem, NameArity) ->
    case NameArity of
        {call, 2} -> {ok, {handle_event, 4}};
        {call, 3} -> {ok, {handle_event, 4}};
        {cast, 2} -> {ok, {handle_event, 4}};
        {start, 3} -> {ok, {init, 1}};
        {start, 4} -> {ok, {init, 1}};
        {start_link, 3} -> {ok, {init, 1}};
        {start_link, 4} -> {ok, {init, 1}};
        {stop, 1} -> {ok, {terminate, 3}};
        {stop, 3} -> {ok, {terminate, 3}};
        _ -> undefined
    end;
target_name_arity(supervisor, NameArity) ->
    case NameArity of
        {start_link, 2} -> {ok, {init, 1}};
        {start_link, 3} -> {ok, {init, 1}};
        _ -> undefined
    end;
target_name_arity(_, _) ->
    undefined.

-spec target_poi(NameArity :: name_arity(), Uri :: uri(), ModuleData :: module_data()) ->
          {ok, location(), poi()} | undefined.
target_poi(NameArity, Uri, ModuleData) ->
    Collected = esrv_req_lib:collect_pois(Uri, ModuleData),
    target_poi(NameArity, Collected).

-spec target_poi(NameArity :: name_arity(), Collected :: [{location(), poi()}]) ->
          {ok, location(), poi()} | undefined.
target_poi(_, []) ->
    undefined;
target_poi(NameArity, [{Endlocation, #poi{data = {local_function, NameArity},
                                          start_location = StartLocation,
                                          definition = {local, StartLocation}} = Poi} | _]) ->
    {ok, Endlocation, Poi};
target_poi(NameArity, [_ | T]) ->
    target_poi(NameArity, T).
