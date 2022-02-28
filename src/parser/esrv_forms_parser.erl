-module(esrv_forms_parser).

-include("types.hrl").
-include("parser.hrl").
-include("records.hrl").

%% API
-export([parse/1]).

%%%===================================================================
%%% API
%%%===================================================================
-spec parse(TokensInfo :: tokens_info()) -> forms_info().
parse(#tokens_info{scanned = Scanned}) ->
    lists:foldr(fun(#tokens_data{start_location = StartLocation,
                                 end_location = EndLocation,
                                 tokens = Tokens,
                                 zones = Zones},
                    #forms_info{forms = Forms0} = FormsInfo0) ->
                        case get_form_data(Tokens) of
                            {ok, FormData} ->
                                FormsInfo0#forms_info{forms = [#form{start_location = StartLocation,
                                                                     end_location = EndLocation,
                                                                     data = FormData,
                                                                     zones = Zones} | Forms0]};
                            skip ->
                                FormsInfo0
                        end
                end, #forms_info{forms = []}, Scanned).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_form_data(Tokens :: [erl_scan:token()]) -> {ok, form_data()} | skip.
get_form_data([]) ->
    {ok, {tokens, []}};
get_form_data(Tokens) ->
    case erl_parse:parse_form(Tokens) of
        {ok, AbstractForm} ->
            {ok, {abstract_form, AbstractForm}};
        {error, _} ->
            case maybe_skip_tokens(Tokens) of
                false ->
                    {ok, {tokens, Tokens}};
                true ->
                    skip
            end
    end.

-spec maybe_skip_tokens(Tokens :: [erl_scan:token()]) -> boolean().
maybe_skip_tokens([{'#', _}, {'#', _}, {atom, _, Atom} | _]) ->
    case atom_to_list(Atom) of
        "mod" ++ _ -> true;
        "cod" ++ _ -> true;
        "dfa" ++ _ -> true;
        "act" ++ _ -> true;
        _ -> false
    end;
maybe_skip_tokens(_) ->
    false.
