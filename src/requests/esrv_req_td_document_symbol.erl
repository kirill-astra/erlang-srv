-module(esrv_req_td_document_symbol).

-behaviour(esrv_request_processor).

%% API
-export([process/1]).

-include("parser.hrl").
-include("protocol.hrl").
-include("records.hrl").

-type kind() :: 1..26.
-type item() :: {binary(), location(), kind()}.

-spec process(Request :: request()) -> {response, #response{}}.
process(#request{id = Id, params = #{<<"textDocument">> := #{<<"uri">> := Uri}}}) ->
    Result =
        case esrv_index_mgr:get_current_module_data(Uri) of
            {ok, #module_data{behaviors = Behaviors,
                              macros = Macros,
                              types = Types,
                              records = Records,
                              functions = Functions}} ->
                Items =
                    behavior_items(Behaviors) ++
                    macro_items(Macros) ++
                    type_items(Types) ++
                    record_items(Records) ++
                    function_items(Functions),
                lists:map(fun({Name, Location, Kind}) ->
                                  [{<<"name">>, Name},
                                   {<<"kind">>, Kind},
                                   {<<"location">>, esrv_req_lib:format_location(Uri, Location)}]
                          end, lists:keysort(2, Items));
            undefined ->
                null
        end,
    {response, #response{id = Id, result = Result}}.

-spec behavior_items(Behaviors :: #{name() => location()}) -> [item()].
behavior_items(Behaviors) ->
    maps:fold(fun(Name, Location, Acc) ->
                      [{atom_to_binary(Name, utf8),
                        Location,
                        ?SYMBOL_KIND_INTERFACE} | Acc]
              end, [], Behaviors).

-spec macro_items(Macros :: macros()) -> [item()].
macro_items(Macros) ->
    maps:fold(fun({Name, Arity}, #macro_definition{location = Location}, Acc) ->
                      [{esrv_req_lib:format_macro_name_arity(Name, Arity),
                        Location,
                        ?SYMBOL_KIND_CONSTANT} | Acc]
                  end, [], Macros).

-spec type_items(Types :: #{name_arity() => location()}) -> [item()].
type_items(Types) ->
    maps:fold(fun({Name, Arity}, Location, Acc) ->
                      [{esrv_req_lib:format_name_arity(Name, Arity),
                        Location,
                        ?SYMBOL_KIND_TYPE_PARAMETER} | Acc]
                  end, [], Types).

-spec record_items(Records :: #{name() => record_data()}) -> [item()].
record_items(Records) ->
    maps:fold(fun(Name, #record_data{location = Location}, Acc) ->
                      [{atom_to_binary(Name, utf8),
                        Location,
                        ?SYMBOL_KIND_STRUCT} | Acc]
              end, [], Records).

-spec function_items(Functions :: #{name_arity() => function_data()}) -> [item()].
function_items(Functions) ->
    maps:fold(fun({Name, Arity}, #function_data{location = Location}, Acc) ->
                      [{esrv_req_lib:format_name_arity(Name, Arity),
                        Location,
                        ?SYMBOL_KIND_FUNCTION} | Acc]
              end, [], Functions).
