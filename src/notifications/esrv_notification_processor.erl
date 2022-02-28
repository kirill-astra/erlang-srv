-module(esrv_notification_processor).

%% API
-export([dispatch/1]).

-include("records.hrl").
-include("log.hrl").

-callback process(#notification{}) -> ok.

%%%===================================================================
%%% API
%%%===================================================================
-spec dispatch(Notification :: #notification{}) -> ok.
dispatch(#notification{method = Method} = Notification) ->
    case Method of
        <<"textDocument/", Value/binary>> when Value =:= <<"didOpen">> orelse
                                               Value =:= <<"didChange">> orelse
                                               Value =:= <<"didSave">> orelse
                                               Value =:= <<"didClose">> ->
            esrv_index_mgr:document_notification(Notification);
        <<"exit">> ->
            esrv_lib:gentle_exit(1);
        _ ->
            ?LOG_INFO("Ignoring '~s' notification", [Method])
    end.
