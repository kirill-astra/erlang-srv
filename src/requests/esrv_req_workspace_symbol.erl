-module(esrv_req_workspace_symbol).

-behaviour(esrv_request_processor).

%% API
-export([process/1]).

-include("types.hrl").
-include("parser.hrl").
-include("protocol.hrl").
-include("records.hrl").

-type kind() :: 1..26.
-type item() :: {binary(), uri(), location(), kind()}.

-spec process(Request :: request()) -> {response, response()}.
process(#request{id = Id, params = #{<<"query">> := Query0}}) ->
    Query1 = string:trim(Query0),
    case re:compile(Query1) of
        {ok, Compiled} ->
            Items =
                lists:foldl(fun({Uri, PersistentModuleData}, Acc) ->
                                    case esrv_index_mgr:get_current_module_data(Uri) of
                                        {ok, VolatileModuleData} ->
                                            process_module(Uri, VolatileModuleData, Compiled, Acc);
                                        undefined ->
                                            process_module(Uri, PersistentModuleData, Compiled, Acc)
                                    end
                            end, [], esrv_db:get_all_proj_module_data()),
            Result =
                lists:map(fun({Name, Uri, Location, Kind}) ->
                                  [{<<"name">>, Name},
                                   {<<"kind">>, Kind},
                                   {<<"location">>, esrv_req_lib:format_location(Uri, Location)}]
                          end, Items),
            {response, #response{id = Id, result = Result}};
        {error, {Error, Position}} ->
            Message = io_lib:format("Incorrect query: ~p; position: ~p", [Error, Position]),
            ResponseError = #response_error{code = ?REC_INVALID_PARAMS,
                                            message = iolist_to_binary(Message)},
            {response, #response{id = Id, error = ResponseError}}
    end.

-spec process_module(Uri :: uri(),
                     ModuleData :: module_data(),
                     Compiled :: any(),
                     Acc :: [item()]) -> [item()].
process_module(Uri, ModuleData, Compiled, Acc0) ->
    lists:foldl(fun({_, #poi{data = {macro, {Name, Arity}}, start_location = Location}}, Acc00) ->
                        Symbol = esrv_req_lib:format_macro_name_arity(Name, Arity),
                        maybe_add_symbol(Symbol, Uri, Location, ?SYMBOL_KIND_CONSTANT,
                                         Compiled, Acc00);
                   ({_, #poi{data = {module, Name}, start_location = Location}}, Acc00) ->
                        Symbol = esrv_req_lib:format_atom(Name),
                        maybe_add_symbol(Symbol, Uri, Location, ?SYMBOL_KIND_MODULE,
                                         Compiled, Acc00);
                   ({_, #poi{data = {behavior, Name}, start_location = Location}}, Acc00) ->
                        Symbol = esrv_req_lib:format_atom(Name),
                        maybe_add_symbol(Symbol, Uri, Location, ?SYMBOL_KIND_INTERFACE,
                                         Compiled, Acc00);
                   ({_, #poi{data = {record, Name}, start_location = Location}}, Acc00) ->
                        Symbol = esrv_req_lib:format_atom(Name),
                        maybe_add_symbol(Symbol, Uri, Location, ?SYMBOL_KIND_STRUCT,
                                         Compiled, Acc00);
                   ({_, #poi{data = {local_type, {Name, Arity}},
                             start_location = Location}}, Acc00) ->
                        Symbol = esrv_req_lib:format_name_arity(Name, Arity),
                        maybe_add_symbol(Symbol, Uri, Location, ?SYMBOL_KIND_TYPE_PARAMETER,
                                         Compiled, Acc00);
                   ({_, #poi{data = {local_type_name, Name}, start_location = Location}}, Acc00) ->
                        Symbol = esrv_req_lib:format_atom(Name),
                        maybe_add_symbol(Symbol, Uri, Location, ?SYMBOL_KIND_TYPE_PARAMETER,
                                         Compiled, Acc00);
                   ({_, #poi{data = {remote_type, _, {Name, Arity}},
                             start_location = Location}}, Acc00) ->
                        Symbol = esrv_req_lib:format_name_arity(Name, Arity),
                        maybe_add_symbol(Symbol, Uri, Location, ?SYMBOL_KIND_TYPE_PARAMETER,
                                         Compiled, Acc00);
                   ({_, #poi{data = {remote_type_name, _, Name},
                             start_location = Location}}, Acc00) ->
                        Symbol = esrv_req_lib:format_atom(Name),
                        maybe_add_symbol(Symbol, Uri, Location, ?SYMBOL_KIND_TYPE_PARAMETER,
                                         Compiled, Acc00);
                   ({_, #poi{data = {local_spec, {Name, Arity}},
                             start_location = Location}}, Acc00) ->
                        Symbol = esrv_req_lib:format_name_arity(Name, Arity),
                        maybe_add_symbol(Symbol, Uri, Location, ?SYMBOL_KIND_FUNCTION,
                                         Compiled, Acc00);
                   ({_, #poi{data = {remote_spec, _, {Name, Arity}},
                             start_location = Location}}, Acc00) ->
                        Symbol = esrv_req_lib:format_name_arity(Name, Arity),
                        maybe_add_symbol(Symbol, Uri, Location, ?SYMBOL_KIND_FUNCTION,
                                         Compiled, Acc00);
                   ({_, #poi{data = {function_clause, {Name, Arity}},
                             start_location = Location}}, Acc00) ->
                        Symbol = esrv_req_lib:format_name_arity(Name, Arity),
                        maybe_add_symbol(Symbol, Uri, Location, ?SYMBOL_KIND_FUNCTION,
                                         Compiled, Acc00);
                   ({_, #poi{data = {local_function, {Name, Arity}},
                             start_location = Location}}, Acc00) ->
                        Symbol = esrv_req_lib:format_name_arity(Name, Arity),
                        maybe_add_symbol(Symbol, Uri, Location, ?SYMBOL_KIND_FUNCTION,
                                         Compiled, Acc00);
                   ({_, #poi{data = {local_function_name, Name},
                             start_location = Location}}, Acc00) ->
                        Symbol = esrv_req_lib:format_atom(Name),
                        maybe_add_symbol(Symbol, Uri, Location, ?SYMBOL_KIND_FUNCTION,
                                         Compiled, Acc00);
                   ({_, #poi{data = {remote_function, _, {Name, Arity}},
                             start_location = Location}}, Acc00) ->
                        Symbol = esrv_req_lib:format_name_arity(Name, Arity),
                        maybe_add_symbol(Symbol, Uri, Location, ?SYMBOL_KIND_FUNCTION,
                                         Compiled, Acc00);
                   ({_, #poi{data = {remote_function_name, _, Name},
                             start_location = Location}}, Acc00) ->
                        Symbol = esrv_req_lib:format_atom(Name),
                        maybe_add_symbol(Symbol, Uri, Location, ?SYMBOL_KIND_FUNCTION,
                                         Compiled, Acc00);
                   (_, Acc00) ->
                        Acc00
                end, Acc0, esrv_req_lib:collect_pois(Uri, ModuleData)).

-spec maybe_add_symbol(Symbol :: binary(),
                       Uri :: uri(),
                       Location :: location(),
                       Kind :: kind(),
                       Compiled :: any(),
                       Acc :: [item()]) -> [item()].
maybe_add_symbol(Symbol, Uri, Location, Kind, Compiled, Acc) ->
    case re:run(Symbol, Compiled) of
        {match, _} ->
            [{Symbol, Uri, Location, Kind} | Acc];
        nomatch ->
            Acc
    end.
