-module(esrv_parser).

-include("types.hrl").
-include("parser.hrl").

%% API
-export([parse/3,
         parse/4]).

%%%===================================================================
%%% API
%%%===================================================================
-spec parse(Uri :: uri(),
            AppId :: app_id() | undefined,
            Data :: binary()) -> module_data().
parse(Uri, AppId, Data) ->
    parse(Uri, AppId, Data, []).

-spec parse(Uri :: uri(),
            AppId :: app_id() | undefined,
            Data :: binary(),
            IncludeChain :: [uri()]) -> module_data().
parse(Uri, AppId, Data, IncludeChain) ->
    TokensInfo = esrv_tokens_parser:parse(Uri, AppId, Data, IncludeChain),
    FormsInfo = esrv_forms_parser:parse(TokensInfo),
    ModuleInfo = esrv_module_parser:parse(Uri, TokensInfo, FormsInfo),
    compose_parsed_module(TokensInfo, ModuleInfo).

-spec compose_parsed_module(TokensInfo :: tokens_info(),
                            ModuleInfo :: module_info()) -> module_data().
compose_parsed_module(#tokens_info{macros = Macros,
                                   include_data = IncludeData,
                                   export = TokensExport,
                                   export_type = TokensExportType,
                                   import = TokensImport,
                                   grey_ranges = GreyRanges},
                      #module_info{module_name = ModuleName,
                                   behaviors = Behaviors,
                                   callback = Callback,
                                   parse_transform = ParseTransform,
                                   export = ModuleExport,
                                   export_type = ModuleExportType,
                                   export_all = ExportAll,
                                   import = ModuleImport,
                                   types = Types,
                                   records = Records,
                                   local_specs = LocalSpecs,
                                   remote_specs = RemoteSpecs,
                                   functions = Functions,
                                   folding_ranges = FoldingRanges,
                                   zones = Zones,
                                   pois = Pois}) ->
    #module_data{module_name = ModuleName,
                 behaviors = Behaviors,
                 callback = Callback,
                 parse_transform = ParseTransform,
                 macros = Macros,
                 include_data = IncludeData,
                 export = ModuleExport ++ TokensExport,
                 export_type = ModuleExportType ++ TokensExportType,
                 export_all = ExportAll,
                 import = ModuleImport ++ TokensImport,
                 types = Types,
                 records = Records,
                 local_specs = LocalSpecs,
                 remote_specs = RemoteSpecs,
                 functions = Functions,
                 grey_ranges = GreyRanges,
                 folding_ranges = FoldingRanges,
                 zones = Zones,
                 pois = Pois}.
