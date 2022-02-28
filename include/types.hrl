-ifndef(erlang_srv_types).
-define(erlang_srv_types, true).

-type msg_id() :: integer() | binary().
-type path() :: binary().
-type pattern() :: [string() | binary()].
-type uri() :: binary().
-type from() :: any().
-type table() :: atom().
-type module_type() :: otp | deps | proj.
-type lp_token() :: integer() | binary().
-type hash() :: binary().
-type version() :: integer().
-type markup_kind() :: plaintext | markdown.

-endif.
