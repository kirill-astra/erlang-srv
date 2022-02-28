-ifndef(erlang_srv_diagnostics).
-define(erlang_srv_diagnostics, true).

-define(DIAGNOSTIC_ERROR, 1).
-define(DIAGNOSTIC_WARNING, 2).
-define(DIAGNOSTIC_INFO, 3).
-define(DIAGNOSTIC_HINT, 4).

-type diagnostic_line() :: pos_integer().
-type diagnostic_column() :: non_neg_integer().
-type diagnostic_location() :: {diagnostic_line(), diagnostic_column()}.
-type diagnostic_range() :: {diagnostic_location(), diagnostic_location()}.

-record(diagnostic, {position :: {line, diagnostic_line()} |
                                 {range, diagnostic_range()},
                     severity :: ?DIAGNOSTIC_ERROR |
                                 ?DIAGNOSTIC_WARNING |
                                 ?DIAGNOSTIC_INFO |
                                 ?DIAGNOSTIC_HINT,
                     source :: binary(),
                     message :: binary()}).
-type diagnostic() :: #diagnostic{}.

-endif.
