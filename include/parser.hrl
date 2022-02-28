-ifndef(erlang_srv_parser).
-define(erlang_srv_parser, true).

-include("types.hrl").

-define(PARSER_VERSION, 1).

-type line() :: integer().
-type line_range() :: {line(), line()}.
-type column() :: integer().
-type location() :: {line(), column()}.
-type location_range() :: {location(), location()}.
-type text() :: string().
-type name() :: atom().
-type name_arity() :: {name(), arity()}.
-type signature() :: [name() | none].
-type export_kind() :: function | type.
-type import() :: module() | {module(), name_arity()}.
-type parsed_name_arity() :: {name_arity(), erl_scan:token()}.
-type token() :: erl_scan:token() | unresolved_macro_token().

%% Macros
-define(UNRESOLVED_MACRO_TOKEN(V), {atom, V, '?unresolved_macro?'}).

-type marcro_arg_tokens() :: [erl_scan:token()].
-type unresolved_macro_token() :: ?UNRESOLVED_MACRO_TOKEN([marcro_arg_tokens()]).

-type md_args() :: #{atom() => integer()} | undefined.
-type mu_args() :: #{integer() => marcro_arg_tokens()} | undefined.

-type macro_arity() :: arity() | undefined.
-type macro_id() :: {name(), macro_arity()}.
-type macro_token() :: token() | {placeholder, integer()}.

-record(macro_definition, {tokens :: [macro_token()],
                           location :: location(),
                           args :: md_args()}).
-type macro_definition() :: #macro_definition{}.

-type macros() :: #{macro_id() => macro_definition()}.

%% Zones
-type zone_type() :: behavior |
                     optional_callbacks |
                     export |
                     export_type |
                     {import, module()} |
                     type |
                     record_name |
                     record_body |
                     spec_name |
                     spec_body |
                     {function, name_arity()} |
                     outer.

-record(opened_zone, {type :: zone_type(),
                      start_location :: location()}).
-type opened_zone() :: #opened_zone{}.

-record(zone, {type :: zone_type(),
               start_location :: location(),
               end_location :: location()}).
-type zone() :: #zone{}.

%% Point of interest
-type poi_data() :: {macro, macro_id()} |
                    {include, string()} |
                    {include_lib, string()} |
                    {module, module()} |
                    {behavior, name()} |
                    {callback, name_arity()} |
                    {variable, name()} |
                    {record, name()} |
                    {field, name(), name()} |
                    {local_type, name_arity()} |
                    {local_type_name, name()} |
                    {remote_type, module(), name_arity()} |
                    {remote_type_name, module(), name()} |
                    {local_spec, name_arity()} |
                    {remote_spec, module(), name_arity()} |
                    {function_clause, name_arity()} |
                    {local_function, name_arity()} |
                    {local_function_name, name()} |
                    {remote_function, module(), name_arity()} |
                    {remote_function_name, module(), name()} |
                    {local_type_or_function, name_arity()} |
                    {local_type_or_function_name, name()} |
                    {remote_type_or_function, module(), name_arity()} |
                    {remote_type_or_function_name, module(), name()}.

-type poi_definition() :: {local, location()} |
                          {remote, uri(), location()}.

-record(poi, {data :: poi_data(),
              start_location :: location(),
              definition :: poi_definition() | undefined}).
-type poi() :: #poi{}.

-type pois() :: {gb_trees:tree(location(), poi()), sets:set(location())}.

%% Tokens info
-record(resolved_include, {line :: line()}).
-type resolved_include() :: #resolved_include{}.

-record(include_data, {resolved :: #{uri() => resolved_include()},
                       unresolved :: [string()],
                       unresolved_libs :: [string()]}).
-type include_data() :: #include_data{}.

-record(tokens_data, {start_location :: location(),
                      end_location :: location(),
                      tokens :: [token()],
                      zones :: [zone()]}).
-type tokens_data() :: #tokens_data{}.

-record(tokens_info, {scanned :: [tokens_data()],
                      macros :: macros(),
                      include_data :: include_data(),
                      export :: [name_arity()],
                      export_type :: [name_arity()],
                      import :: [import()],
                      grey_ranges :: [line_range()],
                      pois :: pois()}).
-type tokens_info() :: #tokens_info{}.

%% Forms info
-type form_data() :: {abstract_form, erl_parse:abstract_form()} |
                     {tokens, [token()]}.

-record(form, {start_location :: location(),
               end_location :: location(),
               data :: form_data(),
               zones :: [zone()]}).
-type form() :: #form{}.

-record(forms_info, {forms :: [form()]}).
-type forms_info() :: #forms_info{}.

%% Module info
-record(record_data, {location :: location(),
                      record_fields :: #{name() => location()}}).
-type record_data() :: #record_data{}.

-record(clause, {variables :: [name()]}).
-type clause() :: #clause{}.

-record(spec_data, {location :: location(),
                    signatures :: [signature()]}).
-type spec_data() :: #spec_data{}.

-record(function_data, {location :: location(),
                        clauses :: [clause()],
                        signatures :: [signature()]}).
-type function_data() :: #function_data{}.

-record(module_info, {uri :: uri(),
                      module_name :: {module(), location()} | undefined,
                      behaviors :: #{name() => location()},
                      callback :: [name_arity()],
                      parse_transform :: [atom()],
                      export :: [name_arity()],
                      export_type :: [name_arity()],
                      export_all :: boolean(),
                      import :: [import()],
                      types :: #{name_arity() => location()},
                      records :: #{name() => record_data()},
                      local_specs :: #{name_arity() => spec_data()},
                      remote_specs :: #{module() => #{name_arity() => spec_data()}},
                      functions :: #{name_arity() => function_data()},
                      folding_ranges :: [line_range()],
                      zones :: [zone()],
                      pois :: pois()}).
-type module_info() :: #module_info{}.

%% Parsed module
-record(module_data, {module_name :: {module(), location()} | undefined,
                      behaviors :: #{name() => location()},
                      callback :: [name_arity()],
                      parse_transform :: [module()],
                      macros :: macros(),
                      include_data :: include_data(),
                      export :: [name_arity()],
                      export_type :: [name_arity()],
                      export_all :: boolean(),
                      import :: [import()],
                      types :: #{name_arity() => location()},
                      records :: #{name() => record_data()},
                      local_specs :: #{name_arity() => spec_data()},
                      remote_specs :: #{module() => #{name_arity() => spec_data()}},
                      functions :: #{name_arity() => function_data()},
                      grey_ranges :: [line_range()],
                      folding_ranges :: [line_range()],
                      zones :: [zone()],
                      pois :: pois()}).
-type module_data() :: #module_data{}.

-endif.
