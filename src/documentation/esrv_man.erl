-module(esrv_man).

-export([module_description/2,
         function_description/3]).

-include("parser.hrl").

-callback init() -> StateData :: any().
-callback merge_line(Line :: binary(),
                     Acc :: binary(),
                     StateData :: any()) -> {NewAcc :: binary(), NewStateData :: any()}.
-callback finalize(Acc :: binary(), StateData :: any()) -> NewAcc :: binary().

-type major_version() :: integer().
-type minor_version() :: integer().
-type man_version() :: {major_version(), minor_version()}.
-type man_paths_info() :: #{major_version() => {minor_version(), path()}}.

-spec module_description(MarkupKind :: markup_kind(),
                         ModuleName :: module()) -> {ok, man_version(), binary()} | undefined.
module_description(MarkupKind, ModuleName) ->
    case find_module_man(ModuleName) of
        {ok, Version, FilePath} ->
            case parse_module_description(MarkupKind, FilePath) of
                <<>> ->
                    undefined;
                Description ->
                    {ok, Version, Description}
            end;
        undefined ->
            undefined
    end.

-spec function_description(MarkupKind :: markup_kind(),
                           ModuleData :: module_data(),
                           NameArity :: name_arity()) -> {ok, man_version(), binary()} | undefined.
function_description(MarkupKind, ModuleData, {FunctionName, _}) ->
    case ModuleData of
        #module_data{module_name = {ModuleName, _}} ->
            case find_module_man(ModuleName) of
                {ok, Version, FilePath} ->
                    case parse_function_description(MarkupKind, FunctionName, FilePath) of
                        <<>> ->
                            undefined;
                        Description ->
                            {ok, Version, Description}
                    end;
                undefined ->
                    undefined
            end;
        _ ->
            undefined
    end.

%%%-------------------------------------------------------------------
%%% Searching man file
%%%-------------------------------------------------------------------
-spec find_module_man(ModuleName :: module()) -> {ok, man_version(), path()} | undefined.
find_module_man(ModuleName) ->
    case esrv_db:check_cache(man_path, fun() -> {cache, get_nearest_man_path()} end) of
        {ok, Version, ManPath} ->
            FilePath = filename:join([ManPath, "man", "man3", atom_to_list(ModuleName) ++ ".3"]),
            case filelib:is_regular(FilePath) of
                true ->
                    {ok, Version, FilePath};
                false ->
                    undefined
            end;
        undefined ->
            undefined
    end.

-spec get_nearest_man_path() -> {ok, man_version(), path()} | undefined.
get_nearest_man_path() ->
    ManRootPath = filename:join(esrv_lib:bdir_user_cache(), "otp_man"),
    case file:list_dir(ManRootPath) of
        {ok, Names} ->
            ManPathsInfo = parse_man_paths(ManRootPath, Names, #{}),
            case choose_major_version(ManPathsInfo) of
                {ok, MajorVersion} ->
                    {MinorVersion, ManPath} = maps:get(MajorVersion, ManPathsInfo),
                    {ok, {MajorVersion, MinorVersion}, ManPath};
                undefined ->
                    undefined
            end;
        _ ->
            undefined
    end.

-spec parse_man_paths(ManRootPath :: path(),
                      Names :: [file:filename()],
                      Acc :: man_paths_info()) -> man_paths_info().
parse_man_paths(_, [], Acc) ->
    Acc;
parse_man_paths(ManRootPath, [Name | T], Acc0) ->
    case re:run(Name, <<"^otp_doc_man_([0-9]*)\\.([0-9]*)$">>, [{capture, all_but_first, list}]) of
        {match, [MajorVersion, MinorVersion]} ->
            ManPath = filename:join(ManRootPath, Name),
            case filelib:is_dir(ManPath) of
                true ->
                    Acc1 = merge_man_path_info(ManPath,
                                               list_to_integer(MajorVersion),
                                               list_to_integer(MinorVersion),
                                               Acc0),
                    parse_man_paths(ManRootPath, T, Acc1);
                false ->
                    parse_man_paths(ManRootPath, T, Acc0)
            end;
        nomatch ->
            parse_man_paths(ManRootPath, T, Acc0)
    end.

-spec merge_man_path_info(ManPath :: path(),
                          MajorVersion :: major_version(),
                          MinorVersion :: minor_version(),
                          ManPathsInfo :: man_paths_info()) -> man_paths_info().
merge_man_path_info(ManPath, MajorVersion, MinorVersion, ManPathsInfo0) ->
    case maps:find(MajorVersion, ManPathsInfo0) of
        {ok, {MinorVersion0, _}} when MinorVersion < MinorVersion0 ->
            ManPathsInfo0;
        _ ->
            maps:put(MajorVersion, {MinorVersion, ManPath}, ManPathsInfo0)
    end.

-spec choose_major_version(ManPathsInfo :: man_paths_info()) -> {ok, major_version()} | undefined.
choose_major_version(ManPathsInfo) ->
    OtpRelease = erlang:system_info(otp_release),
    MajorVersion = maps:keys(ManPathsInfo),
    choose_major_version(list_to_integer(OtpRelease), lists:sort(MajorVersion)).

-spec choose_major_version(OtpRelease :: integer(), MajorVersions :: [major_version()]) ->
          {ok, major_version()} | undefined.
choose_major_version(_, []) ->
    undefined;
choose_major_version(OtpRelease, [MajorVersion | _]) when MajorVersion >= OtpRelease ->
    {ok, MajorVersion};
choose_major_version(OtpRelease, [_ | T]) ->
    choose_major_version(OtpRelease, T).

%%%-------------------------------------------------------------------
%%% Parsing man file
%%%-------------------------------------------------------------------
-spec parse_module_description(MarkupKind :: markup_kind(), FilePath :: path()) -> binary().
parse_module_description(MarkupKind, FilePath) ->
    {ok, Data0} = file:read_file(FilePath),
    case move_to_section(Data0, <<"DESCRIPTION">>) of
        {ok, Data1} ->
            format_section(MarkupKind, Data1);
        eod ->
            <<>>
    end.

-spec parse_function_description(MarkupKind :: markup_kind(),
                                 FunctionName :: name(),
                                 FilePath :: path()) -> binary().
parse_function_description(MarkupKind, FunctionName, FilePath) ->
    {ok, Data0} = file:read_file(FilePath),
    case move_to_section(Data0, <<"EXPORTS">>) of
        {ok, Data1} ->
            format_function(MarkupKind, FunctionName, Data1);
        eod ->
            <<>>
    end.

-spec move_to_section(Data :: binary(), Name :: binary()) -> {ok, binary()} | eod.
move_to_section(<<>>, _) ->
    eod;
move_to_section(Data0, Name) ->
    case binary:split(Data0, <<"\n">>) of
        [<<".SH ", Name/binary>>, Data1] ->
            {ok, Data1};
        [_, Data1] ->
            move_to_section(Data1, Name);
        _ ->
            eod
    end.

-record(section_state, {processor :: module(),
                        state_data :: any()}).

-spec format_section(MarkupKind :: markup_kind(), Data :: binary()) -> binary().
format_section(MarkupKind, Data) ->
    do_format_section([Data], <<>>, init_section_state(MarkupKind)).

-spec do_format_section(Left :: [binary()], Acc :: binary(), State :: #section_state{}) ->
          binary().
do_format_section([], Acc, State) ->
    finalize(Acc, State);
do_format_section([Data], Acc0, State0) ->
    [Line | Left] = binary:split(Data, <<"\n">>),
    case Line of
        <<".SH ", _/binary>> ->
            do_format_section([], Acc0, State0);
        Line ->
            {Acc1, State1} = merge_line(Line, Acc0, State0),
            do_format_section(Left, Acc1, State1)
    end.

-spec init_section_state(MarkupKind :: markup_kind()) -> #section_state{}.
init_section_state(MarkupKind) ->
    {Processor, StateData} = state_data(MarkupKind),
    #section_state{processor = Processor,
                   state_data = StateData}.

-record(function_state, {processor :: module(),
                         state_data :: any(),
                         function_name :: binary(),
                         is_active :: boolean(),
                         code_mode :: boolean()}).

-spec format_function(MarkupKind :: markup_kind(), FunctionName :: name(), Data :: binary()) ->
          binary().
format_function(MarkupKind, FunctionName, Data) ->
    do_format_function([Data], <<>>, init_function_state(MarkupKind, FunctionName)).

-spec do_format_function(Left :: [binary()], Acc :: binary(), State :: #function_state{}) ->
          binary().
do_format_function([], Acc, State) ->
    finalize(Acc, State);
do_format_function([Data], Acc0, #function_state{function_name = FunctionName,
                                                 is_active = IsActive,
                                                 code_mode = CodeMode} = State0) ->
    [Line | Left] = binary:split(Data, <<"\n">>),
    case Line of
        <<".SH ", _/binary>> ->
            do_format_function([], Acc0, State0);
        <<".nf", _/binary>> when CodeMode =:= false ->
            State1 = State0#function_state{code_mode = true},
            {Acc1, State2} = merge_line(Line, Acc0, State1),
            do_format_function(Left, Acc1, State2);
        <<".fi", _/binary>> when CodeMode =:= true ->
            State1 = State0#function_state{code_mode = false},
            {Acc1, State2} = merge_line(Line, Acc0, State1),
            do_format_function(Left, Acc1, State2);
        Line ->
            Regexp = <<"^[a-z0-9_]*?:?([0-9a-z_]+)\\(">>,
            case re:run(Line, Regexp, [{capture, all_but_first, binary}]) of
                {match, [FunctionName]} when IsActive =:= false ->
                    State1 = State0#function_state{is_active = true},
                    {Acc1, State2} =
                        case CodeMode of
                            true ->
                                merge_line(<<".nf">>, Acc0, State1);
                            false ->
                                {Acc0, State1}
                        end,
                    {Acc2, State3} = merge_line(Line, Acc1, State2),
                    do_format_function(Left, Acc2, State3);
                {match, [Matched]} when Matched =/= FunctionName andalso IsActive =:= true ->
                    do_format_function([], Acc0, State0);
                _ ->
                    {Acc1, State1} = merge_line(Line, Acc0, State0),
                    do_format_function(Left, Acc1, State1)
            end
    end.

-spec init_function_state(MarkupKind :: markup_kind(), FunctionName :: name()) ->
          #function_state{}.
init_function_state(MarkupKind, FunctionName) ->
    {Processor, StateData} = state_data(MarkupKind),
    #function_state{processor = Processor,
                    state_data = StateData,
                    function_name = atom_to_binary(FunctionName, utf8),
                    is_active = false,
                    code_mode = false}.

-type state() :: #section_state{} | #function_state{}.

-spec merge_line(Line :: binary(), Acc :: binary(), State :: state()) -> {binary(), state()}.
merge_line(Line, Acc0, #section_state{processor = Processor,
                                      state_data = StateData0} = State0) ->
    {Acc1, StateData1} = Processor:merge_line(Line, Acc0, StateData0),
    {Acc1, State0#section_state{state_data = StateData1}};
merge_line(Line, Acc0, #function_state{processor = Processor,
                                       state_data = StateData0,
                                       is_active = true} = State0) ->
    {Acc1, StateData1} = Processor:merge_line(Line, Acc0, StateData0),
    {Acc1, State0#function_state{state_data = StateData1}};
merge_line(_, Acc, State) ->
    {Acc, State}.

-spec finalize(Acc :: binary(), State :: state()) -> binary().
finalize(Acc, #section_state{processor = Processor,
                             state_data = StateData}) ->
    Processor:finalize(Acc, StateData);
finalize(Acc, #function_state{processor = Processor,
                              state_data = StateData,
                              is_active = true}) ->
    Processor:finalize(Acc, StateData);
finalize(Acc, _) ->
    Acc.

-spec state_data(MarkupKind :: markup_kind()) -> {module(), any()}.
state_data(MarkupKind) ->
    Processor =
        case MarkupKind of
            plaintext ->
                esrv_man_plaintext;
            markdown ->
                esrv_man_markdown
        end,
    {Processor, Processor:init()}.
