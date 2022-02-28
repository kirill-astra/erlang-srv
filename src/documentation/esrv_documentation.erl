-module(esrv_documentation).

-export([module_doc/2,
         function_doc/3,
         function_doc/4,
         function_name_doc/3,
         function_name_doc/4,
         type_doc/3,
         type_doc/4,
         type_name_doc/3,
         type_name_doc/4,
         type_or_function_doc/3,
         type_or_function_doc/4,
         type_or_function_name_doc/3,
         type_or_function_name_doc/4,
         record_doc/3,
         macros_doc/4]).

-include("types.hrl").
-include("parser.hrl").

-define(EMPTY_DOCUMENTATION, <<"Documentation not found">>).

-spec module_doc(MarkupKind :: markup_kind(), ModuleName :: module()) -> binary().
module_doc(MarkupKind, ModuleName) ->
    OtpData =
        case esrv_man:module_description(MarkupKind, ModuleName) of
            {ok, {MajorVersion, MinorVersion}, OtpDescription} ->
                Tag = io_lib:format("OTP (~p.~p)", [MajorVersion, MinorVersion]),
                {iolist_to_binary(Tag), OtpDescription};
            undefined ->
                undefined
        end,
    EdocData =
        case esrv_req_lib:find_module_data(ModuleName) of
            {ok, Uri, _} ->
                case esrv_edoc:module_description(MarkupKind, Uri) of
                    {ok, EdocDescription} ->
                        {<<"EDOC">>, EdocDescription};
                    undefined ->
                        undefined
                end;
            undefined ->
                undefined
        end,
    join_tagged_elements(MarkupKind, [OtpData, EdocData]).

-spec function_doc(MarkupKind :: markup_kind(),
                   ModuleName :: module(),
                   NameArity :: name_arity()) -> binary().
function_doc(MarkupKind, ModuleName, NameArity) ->
    case esrv_req_lib:find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            function_doc(MarkupKind, Uri, ModuleData, NameArity);
        undefined ->
            ?EMPTY_DOCUMENTATION
    end.

-spec function_doc(MarkupKind :: markup_kind(),
                   Uri :: uri(),
                   ModuleData :: module_data(),
                   NameArity :: name_arity()) -> binary().
function_doc(MarkupKind, Uri, ModuleData, NameArity) ->
    do_function_doc(MarkupKind, Uri, ModuleData, [NameArity]).

-spec function_name_doc(MarkupKind :: markup_kind(),
                        ModuleName :: module(),
                        FunctionName :: name()) -> binary().
function_name_doc(MarkupKind, ModuleName, FunctionName) ->
    case esrv_req_lib:find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            function_name_doc(MarkupKind, Uri, ModuleData, FunctionName);
        undefined ->
            ?EMPTY_DOCUMENTATION
    end.

-spec function_name_doc(MarkupKind :: markup_kind(),
                        Uri :: uri(),
                        ModuleData :: module_data(),
                        FunctionName :: name()) -> binary().
function_name_doc(MarkupKind, Uri, ModuleData, FunctionName) ->
    Collected = esrv_req_lib:collect_local_function_name(Uri, ModuleData, FunctionName),
    Targets = [ {FunctionName, Arity} || {Arity, _} <- Collected ],
    do_function_doc(MarkupKind, Uri, ModuleData, Targets).

-spec do_function_doc(MarkupKind :: markup_kind(),
                      Uri :: uri(),
                      ModuleData :: module_data(),
                      Targets :: [name_arity()]) -> binary().
do_function_doc(_, _, _, []) ->
    ?EMPTY_DOCUMENTATION;
do_function_doc(MarkupKind, Uri, ModuleData, Targets) ->
    SpecData =
        case spec_elements(MarkupKind, Uri, ModuleData, Targets) of
            [_|_] = SpecElements ->
                {<<"SPEC">>, iolist_to_binary(lists:join(<<"\n\n">>, SpecElements))};
            [] ->
                undefined
        end,
    OtpData =
        case esrv_man:function_description(MarkupKind, ModuleData, hd(Targets)) of
            {ok, {MajorVersion, MinorVersion}, OtpDescription} ->
                Tag = io_lib:format("OTP (~p.~p)", [MajorVersion, MinorVersion]),
                {iolist_to_binary(Tag), OtpDescription};
            undefined ->
                undefined
        end,
    EdocData =
        case edoc_elements(MarkupKind, Uri, Targets) of
            [_|_] = EdocElements ->
                {<<"EDOC">>, iolist_to_binary(lists:join(<<"\n\n">>, EdocElements))};
            [] ->
                undefined
        end,
    join_tagged_elements(MarkupKind, [SpecData, OtpData, EdocData]).

-spec spec_elements(MarkupKind :: markup_kind(),
                    Uri :: uri(),
                    ModuleData :: module_data(),
                    Targets :: [name_arity()]) -> [binary()].
spec_elements(_, _, _, []) ->
    [];
spec_elements(MarkupKind, Uri, ModuleData, [NameArity | T]) ->
    case esrv_req_lib:get_spec_data(Uri, ModuleData, NameArity) of
        {ok, {_, #spec_data{location = {Line, _}}}} ->
            Element = get_text_document_form(MarkupKind, Uri, Line),
            [Element | spec_elements(MarkupKind, Uri, ModuleData, T)];
        undefined ->
            spec_elements(MarkupKind, Uri, ModuleData, T)
    end.

-spec edoc_elements(MarkupKind :: markup_kind(),
                    Uri :: uri(),
                    Targets :: [name_arity()]) -> [binary()].
edoc_elements(_, _, []) ->
    [];
edoc_elements(MarkupKind, Uri, [NameArity | T]) ->
    case esrv_edoc:function_description(MarkupKind, Uri, NameArity) of
        {ok, Element} ->
            [Element | edoc_elements(MarkupKind, Uri, T)];
        undefined ->
            edoc_elements(MarkupKind, Uri, T)
    end.

-spec type_doc(MarkupKind :: markup_kind(),
               ModuleName :: module(),
               NameArity :: name_arity()) -> binary().
type_doc(MarkupKind, ModuleName, NameArity) ->
    case esrv_req_lib:find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            type_doc(MarkupKind, Uri, ModuleData, NameArity);
        undefined ->
            ?EMPTY_DOCUMENTATION
    end.

-spec type_doc(MarkupKind :: markup_kind(),
               Uri :: uri(),
               ModuleData :: module_data(),
               NameArity :: name_arity()) -> binary().
type_doc(MarkupKind, Uri, ModuleData, NameArity) ->
    do_type_doc(MarkupKind, Uri, ModuleData, [NameArity]).

-spec type_name_doc(MarkupKind :: markup_kind(),
                    ModuleName :: module(),
                    TypeName :: name()) -> binary().
type_name_doc(MarkupKind, ModuleName, TypeName) ->
    case esrv_req_lib:find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            type_name_doc(MarkupKind, Uri, ModuleData, TypeName);
        undefined ->
            ?EMPTY_DOCUMENTATION
    end.

-spec type_name_doc(MarkupKind :: markup_kind(),
                    Uri :: uri(),
                    ModuleData :: module_data(),
                    TypeName :: name()) -> binary().
type_name_doc(MarkupKind, Uri, ModuleData, TypeName) ->
    Collected = esrv_req_lib:collect_local_type_name(Uri, ModuleData, TypeName),
    Targets = [ {TypeName, Arity} || {Arity, _} <- Collected ],
    do_type_doc(MarkupKind, Uri, ModuleData, Targets).

-spec do_type_doc(MarkupKind :: markup_kind(),
                  Uri :: uri(),
                  ModuleData :: module_data(),
                  Targets :: [name_arity()]) -> binary().
do_type_doc(_, _, _, []) ->
    ?EMPTY_DOCUMENTATION;
do_type_doc(MarkupKind, Uri, ModuleData, Targets) ->
    Elements =
        lists:foldr(fun(NameArity, Acc) ->
                            case esrv_req_lib:get_local_type(Uri, ModuleData, NameArity) of
                                {ok, {DefinitionUri, {Line, _}}} when MarkupKind =:= plaintext ->
                                    [[plaintext_text_document_form(DefinitionUri, Line),
                                      <<"\n\n">>,
                                      <<"type defined in ">>,
                                      filename:basename(DefinitionUri),
                                      <<" (line: ">>,
                                      integer_to_binary(Line),
                                      <<")">>] | Acc];
                                {ok, {DefinitionUri, {Line, _}}} when MarkupKind =:= markdown ->
                                    [[markdown_text_document_form(DefinitionUri, Line),
                                      <<"\n\n">>,
                                      <<"type defined in `">>,
                                      filename:basename(DefinitionUri),
                                      <<"` (line: ">>,
                                      integer_to_binary(Line),
                                      <<")">>] | Acc];
                                undefined ->
                                    Acc
                            end
                    end, [], Targets),
    join_elements(Elements).

-spec type_or_function_doc(MarkupKind :: markup_kind(),
                           ModuleName :: module(),
                           NameArity :: name_arity()) -> binary().
type_or_function_doc(MarkupKind, ModuleName, NameArity) ->
    case esrv_req_lib:find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            type_or_function_doc(MarkupKind, Uri, ModuleData, NameArity);
        undefined ->
            ?EMPTY_DOCUMENTATION
    end.

-spec type_or_function_doc(MarkupKind :: markup_kind(),
                           Uri :: uri(),
                           ModuleData :: module_data(),
                           NameArity :: name_arity()) -> binary().
type_or_function_doc(MarkupKind, Uri, ModuleData, NameArity) ->
    Elements0 =
        [type_doc(MarkupKind, Uri, ModuleData, NameArity),
         function_doc(MarkupKind, Uri, ModuleData, NameArity)],
    Elements1 = lists:filter(fun(Element) -> Element =/= ?EMPTY_DOCUMENTATION end, Elements0),
    join_elements(Elements1).

-spec type_or_function_name_doc(MarkupKind :: markup_kind(),
                                ModuleName :: module(),
                                Name :: name()) -> binary().
type_or_function_name_doc(MarkupKind, ModuleName, Name) ->
    case esrv_req_lib:find_module_data(ModuleName) of
        {ok, Uri, ModuleData} ->
            type_or_function_name_doc(MarkupKind, Uri, ModuleData, Name);
        undefined ->
            ?EMPTY_DOCUMENTATION
    end.

-spec type_or_function_name_doc(MarkupKind :: markup_kind(),
                                Uri :: uri(),
                                ModuleData :: module_data(),
                                Name :: name()) -> binary().
type_or_function_name_doc(MarkupKind, Uri, ModuleData, Name) ->
    Elements0 =
        [type_name_doc(MarkupKind, Uri, ModuleData, Name),
         function_name_doc(MarkupKind, Uri, ModuleData, Name)],
    Elements1 = lists:filter(fun(Element) -> Element =/= ?EMPTY_DOCUMENTATION end, Elements0),
    join_elements(Elements1).

-spec record_doc(MarkupKind :: markup_kind(), Uri :: uri(), RecordName :: name()) -> binary().
record_doc(MarkupKind, Uri, RecordName) ->
    {ok, ModuleData} = esrv_lib:get_module_data(Uri),
    case esrv_req_lib:get_record_data(Uri, ModuleData, RecordName) of
        {ok, {DefinitionUri, #record_data{location = {Line, _}}}} when MarkupKind =:= plaintext ->
            iolist_to_binary([plaintext_text_document_form(DefinitionUri, Line),
                              <<"\n\n">>,
                              <<"record '">>,
                              atom_to_binary(RecordName, utf8),
                              <<"' defined in '">>,
                              filename:basename(DefinitionUri),
                              <<"' (line: ">>,
                              integer_to_binary(Line),
                              <<")">>]);
        {ok, {DefinitionUri, #record_data{location = {Line, _}}}} when MarkupKind =:= markdown ->
            iolist_to_binary([markdown_text_document_form(DefinitionUri, Line),
                              <<"\n\n">>,
                              <<"record `">>,
                              atom_to_binary(RecordName, utf8),
                              <<"` defined in `">>,
                              filename:basename(DefinitionUri),
                              <<"` (line: ">>,
                              integer_to_binary(Line),
                              <<")">>]);
        undefined ->
            ?EMPTY_DOCUMENTATION
    end.

-spec macros_doc(MarkupKind :: markup_kind(),
                 Uri :: uri(),
                 MacroName :: name(),
                 MacroArity :: macro_arity()) -> binary().
macros_doc(MarkupKind, Uri, MacroName, MacroArity) ->
    {ok, ModuleData} = esrv_lib:get_module_data(Uri),
    Elements0 =
        maps:fold(fun(DefinitionUri, Macros, Acc0) ->
                          maps:fold(fun({Name, Arity},
                                        #macro_definition{location = {Line, _}},
                                        Acc00) when Name =:= MacroName ->
                                            [{Arity, DefinitionUri, Line} | Acc00];
                                       (_, _, Acc00) ->
                                            Acc00
                                    end, Acc0, Macros)
                  end, [], esrv_req_lib:collect_macros(Uri, ModuleData)),
    Elements1 =
        lists:foldr(fun({Arity, DefinitionUri, Line}, Acc) when Arity =:= MacroArity
                                                                orelse
                                                                MacroArity =:= undefined ->
                            [if
                                 MarkupKind =:= plaintext ->
                                     [plaintext_text_document_form(DefinitionUri, Line),
                                      <<"\n\n">>,
                                      <<"macro '">>,
                                      esrv_req_lib:format_macro_name(MacroName),
                                      format_macro_arity(Arity),
                                      <<"' defined in '">>,
                                      filename:basename(DefinitionUri),
                                      <<"' (line: ">>,
                                      integer_to_binary(Line),
                                      <<")">>];
                                 MarkupKind =:= markdown ->
                                     [markdown_text_document_form(DefinitionUri, Line),
                                      <<"\n\n">>,
                                      <<"macro `">>,
                                      esrv_req_lib:format_macro_name(MacroName),
                                      format_macro_arity(Arity),
                                      <<"` defined in `">>,
                                      filename:basename(DefinitionUri),
                                      <<"` (line: ">>,
                                      integer_to_binary(Line),
                                      <<")">>]
                             end | Acc];
                       (_, Acc) ->
                            Acc
                    end, [], lists:sort(Elements0)),
    case Elements1 of
        [_|_] ->
            iolist_to_binary(lists:join(<<"\n\n\n">>, Elements1));
        [] ->
            case esrv_config:get_value(predefined_macros) of
                {ok, PredefinedMacros} when is_map_key(MacroName, PredefinedMacros) ->
                    Description =
                        case MacroName of
                            'MODULE' ->
                                <<": the name of the current module">>;
                            'MODULE_STRING' ->
                                <<": the name of the current module, as a string">>;
                            'FILE' ->
                                <<": the file name of the current module">>;
                            'LINE' ->
                                <<": the current line number">>;
                            'MACHINE' ->
                                <<": the machine name, 'BEAM'">>;
                            'FUNCTION_NAME' ->
                                <<": the name of the current function">>;
                            'FUNCTION_ARITY' ->
                                <<": the arity (number of arguments) for the current function">>;
                            'OTP_RELEASE' ->
                                OptRelease = erlang:system_info(otp_release),
                                IoData = io_lib:format(": the OTP release as an integer "
                                                       "(current: ~p)", [OptRelease]),
                                iolist_to_binary(IoData);
                            _ ->
                                <<>>
                        end,
                    <<"Predefined macro", Description/binary>>;
                _ ->
                    ?EMPTY_DOCUMENTATION
            end
    end.

-spec format_macro_arity(MacroArity :: macro_arity()) -> iolist().
format_macro_arity(undefined) ->
    [];
format_macro_arity(Arity) ->
    [<<"/">>, integer_to_binary(Arity)].

-spec get_text_document_form(MarkupKind :: markup_kind(), Uri :: uri(), Line :: line()) ->
          binary().
get_text_document_form(plaintext, Uri, Line) ->
    plaintext_text_document_form(Uri, Line);
get_text_document_form(markdown, Uri, Line) ->
    markdown_text_document_form(Uri, Line).

-spec markdown_text_document_form(Uri :: uri(), Line :: line()) -> binary().
markdown_text_document_form(Uri, Line) ->
    iolist_to_binary([<<"``` erlang">>, $\n,
                      plaintext_text_document_form(Uri, Line), $\n,
                      <<"```">>]).

-spec plaintext_text_document_form(Uri :: uri(), Line :: line()) -> binary().
plaintext_text_document_form(Uri, Line) ->
    FormText = esrv_lib:fetch_text_document_form(Uri, Line),
    strip_new_lines(FormText).

-spec join_elements(Elements :: [iodata()]) -> binary().
join_elements(Elements) ->
    case Elements of
        [_|_] ->
            iolist_to_binary(lists:join(<<"\n\n\n">>, Elements));
        [] ->
            ?EMPTY_DOCUMENTATION
    end.

-spec join_tagged_elements(MarkupKind :: markup_kind(),
                           Elements :: [{binary(), binary()} | undefined]) -> binary().
join_tagged_elements(MarkupKind, Elements0) ->
    case prepare_elements(MarkupKind, Elements0) of
        [_|_] = Elements1 ->
            iolist_to_binary(lists:join(<<"\n\n\n">>, Elements1));
        [] ->
            ?EMPTY_DOCUMENTATION
    end.

-spec prepare_elements(MarkupKind :: markup_kind(),
                       Elements :: [{binary() | undefined, binary()} | undefined]) -> [binary()].
prepare_elements(MarkupKind, Elements) ->
    lists:filtermap(fun({Tag, Data0}) ->
                            Data1 = strip_new_lines(Data0),
                            Data2 =
                                case MarkupKind of
                                    plaintext ->
                                        iolist_to_binary([Tag, <<":\n\n">>, Data1]);
                                    markdown ->
                                        iolist_to_binary([<<"### ">>, Tag, <<":\n\n">>, Data1])
                                end,
                            {true, Data2};
                       (undefined) ->
                            false
                    end, Elements).

-spec strip_new_lines(Data :: binary()) -> binary().
strip_new_lines(Data0) ->
    lists:foldl(fun(RegExp, Data00) ->
                        re:replace(Data00, RegExp, <<"">>, [{return, binary}])
                end, Data0, [<<"^\n">>, <<"\n*$">>]).
