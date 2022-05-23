-module(esrv_main_fsm).

-behaviour(gen_statem).

-include("protocol.hrl").
-include("types.hrl").
-include("records.hrl").
-include("log.hrl").

%% API
-export([start_link/0,
         initialized/1,
         message_in/1,
         next_id/0,
         request/1,
         request/2,
         response/1,
         notification/1]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([waiting_for_initialize/3,
         initializing/3,
         waiting_for_initialized/3,
         active/3,
         waiting_for_exit/3]).

-define(REQUEST_TIMEOUT, 15000).
-define(EXIT_TIMEOUT, 10000).
-define(SERVER, ?MODULE).

-record(data, {in_requests :: #{msg_id() => pid()},
               out_requests :: #{msg_id() => from()},
               next_id :: msg_id()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec initialized(Response :: response()) -> ok.
initialized(Response) ->
    gen_statem:cast(?SERVER, {initialized, Response}).

-spec message_in(Message :: message()) -> ok.
message_in(Message) ->
    gen_statem:cast(?SERVER, {in, Message}).

-spec next_id() -> msg_id().
next_id() ->
    gen_statem:call(?SERVER, next_id, infinity).

-spec request(Request :: request()) -> {ok, response()} | error.
request(Request) ->
    request(Request, ?REQUEST_TIMEOUT).

-spec request(Request :: request(), Timeout :: timeout()) -> {ok, response()} | error.
request(Request, Timeout) ->
    gen_statem:call(?SERVER, {request, Request, Timeout}, infinity).

-spec response(Response :: response()) -> ok.
response(Response) ->
    gen_statem:cast(?SERVER, {response, Response}).

-spec notification(Notification :: notification()) -> ok.
notification(Notification) ->
    gen_statem:cast(?SERVER, {notification, Notification}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> gen_statem:init_result(atom()).
init([]) ->
    {ok, waiting_for_initialize, #data{in_requests = #{},
                                       out_requests = #{},
                                       next_id = 1}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one function like this for each state name.
%% Whenever a gen_statem receives an event, the function
%% with the name of the current state (StateName)
%% is called to handle the event.
%% @end
%%--------------------------------------------------------------------
-spec waiting_for_initialize('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
                            (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
waiting_for_initialize(internal, {in, Message}, Data) ->
    case Message of
        #request{method = <<"initialize">>} ->
            ?LOG_INFO("Initialization started"),
            {next_state, initializing, Data, [{next_event, internal, {dispatch, Message}}]};
        #request{id = Id, method = Method} ->
            ?LOG_WARNING("Got '~s' request before initialization", [Method]),
            Error = #response_error{code = ?REC_SERVER_NOT_INITIALIZED,
                                    message = <<"No message expected before 'initialize'">>},
            Response = #response{id = Id, error = Error},
            {keep_state_and_data, [{next_event, internal, {out, Response}}]};
        #notification{method = <<"exit">>} ->
            ?LOG_INFO("Got 'exit' notification during initialization"),
            esrv_lib:gentle_exit(1);
        _ ->
            keep_state_and_data
    end;

waiting_for_initialize(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, waiting_for_initialize, Data).

-spec initializing('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
                  (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
initializing(cast, {initialized, Response}, Data) ->
    {next_state, waiting_for_initialized, Data, [{next_event, cast, {response, Response}}]};

initializing(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, initializing, Data).

-spec waiting_for_initialized('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
                            (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
waiting_for_initialized(internal, {in, #notification{method = <<"initialized">>}}, Data) ->
    ?LOG_INFO("Initialization completed"),
    {next_state, active, Data};

waiting_for_initialized(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, waiting_for_initialized, Data).

-spec active('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
            (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
active(internal, {in, Message}, Data) ->
    case Message of
        #request{id = Id, method = <<"shutdown">>} ->
            ?LOG_INFO("Shutdown started"),
            Response = #response{id = Id, result = null},
            {next_state, waiting_for_exit, Data, [{next_event, internal, {out, Response}},
                                                  {state_timeout, ?EXIT_TIMEOUT, force_exit}]};
        _ ->
            {keep_state_and_data, [{next_event, internal, {dispatch, Message}}]}
    end;

active({call, From}, {request, Request, Timeout}, _) ->
    {keep_state_and_data, [{next_event, internal, {out, Request}},
                           {next_event, internal, {state_out_request, Request, Timeout, From}}]};

active(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, active, Data).

-spec waiting_for_exit('enter', OldState :: atom(), Data :: term()) ->
          gen_statem:state_enter_result(atom());
                      (gen_statem:event_type(), Msg :: term(), Data :: term()) ->
          gen_statem:event_handler_result(atom()).
waiting_for_exit(internal, {in, Message}, _) ->
    case Message of
        #request{id = Id, method = Method} ->
            ?LOG_WARNING("Got '~s' request after shutdown", [Method]),
            Error = #response_error{code = ?REC_INVALID_REQUEST,
                                    message = <<"No message expected after 'shutdown'">>},
            Response = #response{id = Id, error = Error},
            {keep_state_and_data, [{next_event, internal, {out, Response}}]};
        #notification{method = <<"exit">>} ->
            ?LOG_INFO("Shutdown completed"),
            esrv_lib:gentle_exit(0);
        _ ->
            keep_state_and_data
    end;

waiting_for_exit({call, From}, {request, _, _}, _) ->
    {keep_state_and_data, [{reply, From, error}]};

waiting_for_exit(state_timeout, force_exit, _) ->
    ?LOG_WARNING("No 'exit' notification received"),
    esrv_lib:gentle_exit(2);

waiting_for_exit(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, waiting_for_exit, Data).

-spec handle_event('enter', OldState :: term(), State :: term(), Data :: term()) ->
          gen_statem:state_enter_result(term());
                  (gen_statem:event_type(), Msg :: term(), State :: term(), Data :: term()) ->
          gen_statem:event_handler_result(term()).

%% Receiveing incoming messages
handle_event(cast, {in, #request{} = Request}, _, _) ->
    ?LOG_DEBUG(format_request(Request), #{trace => true, type => request_in}),
    {keep_state_and_data, [{next_event, internal, {in, Request}}]};

handle_event(cast, {in, #response{} = Response}, _, _) ->
    ?LOG_DEBUG(format_response(Response), #{trace => true, type => response_in}),
    {keep_state_and_data, [{next_event, internal, {in, Response}}]};

handle_event(cast, {in, #notification{} = Notification}, _, _) ->
    ?LOG_DEBUG(format_notification(Notification), #{trace => true, type => notification_in}),
    {keep_state_and_data, [{next_event, internal, {in, Notification}}]};

%% Dispatching incoming messages
handle_event(internal, {dispatch, #request{} = Request}, _, _) ->
    case esrv_request_processor:dispatch(Request) of
        {response, Response} ->
            {keep_state_and_data, [{next_event, internal, {out, Response}}]};
        {async, Pid} ->
            {keep_state_and_data, [{next_event, internal, {state_in_request, Request, Pid}}]}
    end;

handle_event(internal, {dispatch, #response{} = Response}, _, _) ->
    {keep_state_and_data, [{next_event, internal, {state_in_response, Response}}]};

handle_event(internal, {dispatch, #notification{} = Notification}, _, _) ->
    ok = esrv_notification_processor:dispatch(Notification),
    keep_state_and_data;

%% Sending outcoing messages
handle_event({call, From}, next_id, _, #data{next_id = NextId} = Data0) ->
    {keep_state, Data0#data{next_id = NextId + 1}, [{reply, From, NextId}]};

handle_event(internal, {out, #request{} = Request}, _, _) ->
    ?LOG_DEBUG(format_request(Request), #{trace => true, type => request_out}),
    esrv_client_gw:message_out(Request),
    keep_state_and_data;

handle_event(cast, {response, Response}, _, _) ->
    {keep_state_and_data, [{next_event, internal, {out, Response}},
                           {next_event, internal, {state_out_response, Response}}]};

handle_event(internal, {out, #response{} = Response}, _, _) ->
    ?LOG_DEBUG(format_response(Response), #{trace => true, type => response_out}),
    esrv_client_gw:message_out(Response),
    keep_state_and_data;

handle_event(cast, {notification, Notification}, _, _) ->
    {keep_state_and_data, [{next_event, internal, {out, Notification}}]};

handle_event(internal, {out, #notification{} = Notification}, _, _) ->
    ?LOG_DEBUG(format_notification(Notification), #{trace => true, type => notification_out}),
    esrv_client_gw:message_out(Notification),
    keep_state_and_data;

%% Storing request/response info in the server state
handle_event(internal, {state_in_request, #request{id = Id}, Pid}, _,
             #data{in_requests = InRequests0} = Data0) ->
    Data1 = Data0#data{in_requests = InRequests0#{Id => Pid}},
    {keep_state, Data1};

handle_event(internal, {state_out_response, #response{id = Id}}, _,
             #data{in_requests = InRequests0} = Data0) ->
    Data1 = Data0#data{in_requests = maps:remove(Id, InRequests0)},
    {keep_state, Data1};

handle_event(internal, {state_out_request, #request{id = Id}, Timeout, From}, _,
             #data{out_requests = OutRequests0} = Data0) ->
    Data1 = Data0#data{out_requests = OutRequests0#{Id => From}},
    {keep_state, Data1, [{{timeout, Id}, Timeout, discard_request}]};

handle_event(internal, {state_in_response, #response{id = Id} = Response}, _,
             #data{out_requests = OutRequests0} = Data0) ->
    case maps:get(Id, OutRequests0, undefined) of
        From when From =/= undefined ->
            Data1 = Data0#data{out_requests = maps:remove(Id, OutRequests0)},
            {keep_state, Data1, [{reply, From, {ok, Response}},
                                 {{timeout, Id}, infinity, discard_request}]};
        undefined ->
            keep_state_and_data
    end;

handle_event({timeout, Id}, discard_request, _, #data{out_requests = OutRequests0} = Data0) ->
    {From, OutRequests1} = maps:take(Id, OutRequests0),
    Data1 = Data0#data{out_requests = OutRequests1},
    {keep_state, Data1, [{reply, From, error}]};

handle_event(EventType, EventContent, State, _) ->
    ?LOG_ERROR("Module: ~p; not implemented event in ~p state; type: ~p; content: ~p",
               [?MODULE, State, EventType, EventContent]),
    {stop, not_implemented}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) -> any().
terminate(_Reason, _State, _Data) ->
    void.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  OldState :: term(),
                  OldData :: term(),
                  Extra :: term()) ->
          {ok, NewState :: term(), NewData :: term()} | (Reason :: term()).
code_change(_, OldState, OldData, _) ->
    {ok, OldState, OldData}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec format_request(Request :: request()) -> iodata().
format_request(#request{id = Id, method = Method, params = Params}) ->
    io_lib:format("[id: ~p] [method: ~s]~s", [Id, Method, format_json(<<"params">>, Params)]).

-spec format_response(Response :: response()) -> iodata().
format_response(#response{id = Id, result = Result, error = Error}) ->
    io_lib:format("[id: ~p]~s~s", [Id, format_json(<<"result">>, Result),
                                   format_error(Error)]).

-spec format_notification(Notification :: notification()) -> iodata().
format_notification(#notification{method = Method, params = Params}) ->
    io_lib:format("[method: ~s]~s", [Method, format_json(<<"params">>, Params)]).

-spec format_error(ResponseRrror :: #response_error{} | undefined) -> iodata().
format_error(undefined) ->
    <<>>;
format_error(#response_error{code = Code,
                             message = Message,
                             data = Data}) ->
    format_json(<<"error">>, [{<<"code">>, Code},
                              {<<"message">>, Message},
                              {<<"data">>, Data}]).

-spec format_json(Tag :: binary(), JsonTerm :: jsx:json_term() | undefined) -> iodata().
format_json(_, undefined) ->
    <<>>;
format_json(Tag, JsonTerm) ->
    JsonText = esrv_msg_coder:encode_json_term(JsonTerm),
    Prettified = jsx:prettify(JsonText),
    Shifted = re:replace(<<"  ", Prettified/binary>>, <<"\n">>, <<"\n  ">>, [global]),
    [<<"\n">>, Tag, <<":\n">>, Shifted].
