-module(esrv_request_processor).

%% API
-export([dispatch/1]).

-include("protocol.hrl").
-include("records.hrl").
-include("log.hrl").

-callback process(request()) -> {response, response()} | {async, pid()}.

%%%===================================================================
%%% API
%%%===================================================================
-spec dispatch(Request :: request()) -> {response, response()} | {async, pid()}.
dispatch(#request{method = <<"initialize">>} = Request) ->
    esrv_req_initialize:process(Request);
dispatch(#request{id = Id, method = Method} = Request) ->
    try
        {Time, Result} = timer:tc(fun() -> do_dispatch(Request) end),
        ?LOG_INFO("Request '~s' processed in ~p ms", [Method, Time div 1000]),
        Result
    catch
        T:E:S ->
            ?LOG_ERROR("Processing '~s' request internal error; "
                       "type: ~p; error: ~p; stacktrace: ~p", [Method, T, E, S]),
            InternalError =
                #response_error{code = ?REC_INTERNAL_ERROR,
                                message = <<"Internal error">>},
            {response, #response{id = Id, error = InternalError}}
    end.

-spec do_dispatch(Request :: request()) -> {response, response()} | {async, pid()}.
do_dispatch(#request{id = Id, method = Method} = Request) ->
    case Method of
        <<"textDocument/completion">> ->
            esrv_req_td_completion:process(Request);
        <<"completionItem/resolve">> ->
            esrv_req_completion_resolve:process(Request);
        <<"textDocument/definition">> ->
            esrv_req_td_definition:process(Request);
        <<"textDocument/references">> ->
            esrv_req_td_references:process(Request);
        <<"textDocument/hover">> ->
            esrv_req_td_hover:process(Request);
        <<"textDocument/documentSymbol">> ->
            esrv_req_td_document_symbol:process(Request);
        <<"textDocument/documentHighlight">> ->
            esrv_req_td_document_highlight:process(Request);
        <<"textDocument/implementation">> ->
            esrv_req_td_implementation:process(Request);
        <<"textDocument/prepareRename">> ->
            esrv_req_td_prepare_rename:process(Request);
        <<"textDocument/rename">> ->
            esrv_req_td_rename:process(Request);
        <<"textDocument/foldingRange">> ->
            esrv_req_td_folding_range:process(Request);
        <<"workspace/symbol">> ->
            esrv_req_workspace_symbol:process(Request);
        _ ->
            MethodNotFoundError =
                #response_error{code = ?REC_METHOD_NOT_FOUND,
                                message = <<"Method not found">>},
            {response, #response{id = Id, error = MethodNotFoundError}}
    end.
