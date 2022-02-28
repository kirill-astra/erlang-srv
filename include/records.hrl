-ifndef(erlang_srv_records).
-define(erlang_srv_records, true).

-include("types.hrl").
-include("parser.hrl").

%% LSP
-record(request, {id :: msg_id(),
                  method :: binary(),
                  params :: jsx:json_term() | undefined}).
-type request() :: #request{}.

-record(response, {id :: msg_id() | null,
                   result :: jsx:json_term() | undefined,
                   error :: response_error() | undefined}).
-type response() :: #response{}.

-record(response_error, {code :: integer(),
                         message :: binary(),
                         data :: jsx:json_term() | undefined}).
-type response_error() :: #response_error{}.

-record(notification, {method :: binary(),
                       params :: jsx:json_term() | undefined}).
-type notification() :: #notification{}.

-type message() :: request() | response() | notification().

%% mnesia
-record(parsed_module, {uri :: uri(),
                        hash :: hash(),
                        parser_version :: integer(),
                        module_data :: module_data()}).
-type parsed_module() :: #parsed_module{}.

-record(module_meta, {uri :: uri(),
                      hash :: hash(),
                      module :: module() | undefined,
                      module_type :: module_type()}).
-type module_meta() :: #module_meta{}.

-record(proj_module, {uri :: uri(),
                      module :: module() | undefined,
                      module_data :: module_data()}).
-type proj_module() :: #proj_module{}.

-record(text_document, {uri :: uri(),
                        app_path :: path(),
                        module_type :: module_type(),
                        saved_text_hash :: hash(),
                        current_version :: non_neg_integer(),
                        current_content :: binary(),
                        current_module_data :: module_data() | undefined}).
-type text_document() :: #text_document{}.

-record(loaded_module, {uri :: uri(),
                        hash :: hash()}).
-type loaded_module() :: #loaded_module{}.

-record(cache, {key :: any(),
                value :: any(),
                start_timestamp :: integer(),
                expired_after :: integer() | infinity}).
-type cache() :: #cache{}.

-endif.
