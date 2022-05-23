-ifndef(erlang_srv_types).
-define(erlang_srv_types, true).

-type msg_id() :: integer() | binary().
-type path() :: binary().
-type uri() :: binary().
-type from() :: any().
-type table() :: atom().
-type app_id() :: atom().
-type app_type() :: passive_app_type() | active_app_type().
-type passive_app_type() :: otp | deps.
-type active_app_type() :: sub_proj | proj.
-type lp_token() :: integer() | binary().
-type hash() :: binary().
-type version() :: integer().
-type markup_kind() :: plaintext | markdown.
-type distr_mode() :: shortnames | longnames.
-type scan_fs_pattern() :: file:filename_all() | [file:name_all()].
-type scan_fs_filter() :: file | dir | both.
-type scan_fs_options() :: #{filter => scan_fs_filter(),
                             recursive => boolean(),
                             to_skip => [path()],
                             extensions => [binary()]}.

-endif.
