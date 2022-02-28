# Erlang SRV
The Erlang Language Server Protocol Implementation.

## Command-line Arguments

These are the command-line arguments that can be provided to the
`erlang_srv` escript:

```
Usage: erlang_srv [-v] [-t [<transport>]] [-p [<port>]] [-d [<log_dir>]]
                  [-l [<log_level>]] [-o [<log_otp>]]

  -v, --version    Print the current version of Erlang Server and exit
  -t, --transport  Specifies the transport the server will use for the
                   connection with the client, either 'stdio' or 'tcp'
                   [default: stdio]
  -p, --port       Specifies the port used for 'tcp' transport [default:
                   10000]
  -d, --log-dir    Specifies the logs directory [default: value returned by
                   filename:basedir(user_log, "erlang_srv") and appended by current
                   directory name ]
  -l, --log-level  Specifies the log level. Possible values: 'emergency',
                   'alert', 'critical', 'error', 'warning', 'notice',
                   'info', 'debug' and two special values 'all' and 'none'
                   [default: info]
  -o, --log-otp    Specifies the filter action for otp/sasl log events, either
                   'log' or 'stop' [default: stop]
```

## Configuration

Erlang SRV looks for configuration parameters in the following sources (decreasing priority level):
- configuration file
- `"initializationOptions"` value in the `initialize` request according to LSP
- default values

Check language server logs to detect an actual source of certain configuration parameter.

### Configuration file

The configuration file format is `yaml`.

Erlang SRV looks for configuration file `erlang_srv.config` in the following locations:
- root directory of a given project
- directory passed in the `"initializationOptions"."configPath"` value in scope of `initialize` request according to LSP
- directory returned by `filename:basedir(user_config, "erlang_srv")`

Check language server logs to detect actually applied configuration file.

Consult example [erlang_srv.config](examples/erlang_srv.config.example)

### Initialization options

Configuration parameters can be set via passing `"initializationOptions"` in the `initialize` request according to LSP.

Consult example [initializationOptions.json](examples/initializationOptions.json.example)

| Parameter | Description | Default |
| :- | :- | :- |
| `otpAppsExclude` | List of OTP applications to to exclude from analysis. | |
| `appsDirs` | Subdirectories to recursively analyze. Used both for project and dependency applications. It supports wildcards. | `["src", "test", "include"]` |
| `depsDirs` | List of directories containing dependencies. It supports wildcards. | `["deps", "_checkouts", "_build/default/lib"]` |
| `includeDirs` | List of directories used by diagnostic modules as include dirs. It supports wildcards. | `["include"]` |
| `extraPaths` | List of directories to add into erlang code server by `code:add_path/1` | |
| `distrMode` | Erlang distribution mode. Either `"shortnames"` or `"longnames"` | `"shortnames"` |
| `macros` | Predefined macros used during analysis. | |
| `syncOtpMan` | Whether or not to fetch automatically [OTP man pages](http://erlang.org/download/) | `true` |
| `diagnostics` | List of diagnostic modules with their options | `[{"name": "compiler"}, {"name": "xref"}, {"name": "elvis"}, {"name": "unused_macros"}]` |

## Diagnostics

When a file is open or saved, a list of diagnostics are run in the background, reporting eventual issues to the editor. Diagnostics list is configured via `diagnostics` parameter.

Each diagnostics list element consists of the following fields:

| Parameter | Description | Default |
| :- | :- | :- |
| `name` | Diagnostic name matches erlang module name implementing `esrv_diagnostics` behavior. | |
| `enabled` | Whether or not diagnostic is enabled | `true` |
| `options` | Options passed to the diagnostic module at init | `[]` |

Erlang SRV provides some diagnostic modules out of box:

| Name | Description | Enabled by default |
| :- | :- | :- |
| `compiler` | Reports warnings and errors from erlang [compiler](https://www.erlang.org/doc/man/compile.html) | `true` |
| `xref` | Looks for undefined external function calls and undefined external type usage | `true` |
| `elvis` | Use [elvis](https://github.com/inaka/elvis) to review the code style | `true` |
| `unused_macros` | Looks for macros which are defined but not utilized | `true` |
| `dialyzer` | Use the [dialyzer](https://www.erlang.org/doc/apps/dialyzer/dialyzer_chapter.html) static analysis tool to find discrepancies in your code | `false` |

Also it is possible to implement your own diagnostic module. Such module must implement `esrv_diagnostics` behavior. Use `extraPaths` configuration parameter to add directory containing diagnostic module.

Check language server logs to detect actually activated diagnostic modules.

### Diagnostic `compiler`

This module provides:

- Compilation of `.erl` and `.escript` files
- Parsing of `.hrl` files
- Optional deployment compiled modules to target erlang node

Supported options:

| Option | Description | Example |
| :- | :- | :- |
| `extra_options` | String containing erlang list with additional compiler options | `"[nowarn_unused_vars, {d, 'DEBUG'}]"` |
| `deploy_node` | Erlang node name to deploy compiled modules to. Target deployment node must be started in the same name mode: either both short names or both long names. Use `distrMode` configuration parameter to setup Erlang SRV name mode  | `"target@127.0.0.1"` |
| `deploy_cookie` | [Magic cookie](https://www.erlang.org/doc/reference_manual/distributed.html#security) used by distributed erlang. Target deployment node must have the same value | `"MySuperSecret"` |

### Diagnostic `xref`

This module looks for undefined external function calls and undefined external type usage.

It is possible to disable reporting errors for certain functions, types or even for whole modules. Such disabling may be handy in case of [parse transform](https://www.erlang.org/doc/man/erl_id_trans.html#parse-transformations) (the brightest example is [lager](https://github.com/erlang-lager/lager) application).

Supported options:

| Option | Description | Example |
| :- | :- | :- |
| `to_skip` | Objects to disable diagnostic for | `[{"module": "fully_disabled_module_name"}, {"module": "partially_disabled_module_name", "functions": ["any_arities_function", "certain_arity_function/1"], "types": ["any_arities_type", "certain_arity_type/1"]}]` |

### Diagnostic `elvis`

This module provides code style analysis according to the `elvis.config` file.

Erlang SRV looks for elvis configuration according to the following algorithm:

- `elvis.config` file in the root directory of a given project
- file name passed in the `"config"` diagnostic option
- `elvis.config` file in the directory returned by `filename:basedir(user_config, "erlang_srv")`

Elvis diagnostic disabled if no configuration file found.

Supported options:

| Option | Description |
| :- | :- |
| `config` | Elvis configuration file to use if no project configuration file provided |

### Diagnostic `unused_macros`

This module looks for macros which are defined but not utilized. It is possible to disable reporting warnings for certain macros.

Supported options:

| Option | Description | Example |
| :- | :- | :- |
| `to_skip` | Objects to disable diagnostic for | `[{"module": "fully_disabled_module_name"}, {"module": "partially_disabled_module_name", "macros": ["MACRO_NAME"]}]` |

### Diagnostic `dialyzer`

This module runs [dialyzer](https://www.erlang.org/doc/apps/dialyzer/dialyzer_chapter.html) against `.erl` files.

Erlang SRV automatically generates plt files if no `plts` option is found among diagnostic options.

**WARNING**: dialyzer diagnostics may take too much time (especially for large projects) and cause bad user experience. This diagnostic module is disabled by default. Please use it carefully.

Supported options:

| Option | Description | Example |
| :- | :- | :- |
| `plts` | Explicit list of plt files | `["/path/to/plt/file_1", "/path/to/plt/file_2"]` |
