-module(esrv_req_td_folding_range).

-behaviour(esrv_request_processor).

%% API
-export([process/1]).

-include("types.hrl").
-include("records.hrl").

-spec process(Request :: request()) -> {response, #response{}}.
process(#request{id = Id, params = #{<<"textDocument">> := #{<<"uri">> := Uri}}}) ->
    Result =
        case esrv_index_mgr:get_current_module_data(Uri) of
            {ok, #module_data{folding_ranges = FoldingRanges}} ->
                lists:map(fun({StartLine, EndLine}) ->
                                  [{<<"startLine">>, StartLine - 1},
                                   {<<"endLine">>, EndLine - 1},
                                   {<<"kind">>, <<"region">>}]
                          end, FoldingRanges);
            undefined ->
                null
        end,
    {response, #response{id = Id, result = Result}}.
