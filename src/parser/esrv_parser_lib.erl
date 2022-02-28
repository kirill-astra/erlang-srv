-module(esrv_parser_lib).

-include("types.hrl").
-include("parser.hrl").

%% API
-export([add_node_poi/4,
         get_node_location/1,
         add_token_poi/4,
         get_after_token_location/1,
         get_token_location/1,
         set_token_location/2,
         get_anno_location/1,
         normalize_location/1,
         location_to_line/1,
         location_to_column/1,
         first_token_location/1,
         last_token_location/1,
         parse_name_arity_tokens/1]).

%%%===================================================================
%%% API
%%%===================================================================
-spec add_node_poi(Node :: erl_syntax:syntaxTree(),
                   PoiData :: poi_data(),
                   PoiDefinition :: poi_definition() | undefined,
                   Pois :: pois()) -> NewPois :: pois().
add_node_poi(Node, PoiData, PoiDefinition, Pois0) ->
    Anno = erl_syntax:get_pos(Node),
    add_anno_poi(Anno, PoiData, PoiDefinition, Pois0).

-spec get_node_location(Node :: erl_syntax:syntaxTree()) -> location().
get_node_location(Node) ->
    Anno = erl_syntax:get_pos(Node),
    get_anno_location(Anno).

-spec add_token_poi(Token :: erl_scan:token(),
                    PoiData :: poi_data(),
                    PoiDefinition :: poi_definition() | undefined,
                    Pois :: pois()) -> NewPois :: pois().
add_token_poi(Token, PoiData, PoiDefinition, Pois0) ->
    Anno = get_token_anno(Token),
    add_anno_poi(Anno, PoiData, PoiDefinition, Pois0).

-spec get_after_token_location(Token :: erl_scan:token()) -> location().
get_after_token_location(Token) ->
    case erl_scan:text(Token) of
        Text when Text =/= undefined ->
            {Line, Column} = get_token_location(Token),
            {Line, Column + length(Text)};
        undefined ->
            get_token_location(Token)
    end.

-spec get_token_location(Token :: erl_scan:token()) -> location().
get_token_location(Token) ->
    Location = erl_scan:location(Token),
    normalize_location(Location).

-spec set_token_location(Token :: erl_scan:token(), Location :: location()) -> erl_scan:token().
set_token_location(Token, Location) ->
    update_token_anno(fun(Anno) -> erl_anno:set_location(Location, Anno) end, Token).

-spec update_token_anno(Updater :: fun((erl_anno:anno()) -> erl_anno:anno()),
                        Token :: erl_scan:token()) -> erl_scan:token().
update_token_anno(Updater, Token) ->
    Anno = get_token_anno(Token),
    set_token_anno(Updater(Anno), Token).

-spec get_token_anno(Token :: erl_scan:token()) -> erl_anno:anno().
get_token_anno(Token) ->
    element(2, Token).

-spec set_token_anno(Anno :: erl_anno:anno(), Token :: erl_scan:token()) -> erl_scan:token().
set_token_anno(Anno, Token) ->
    setelement(2, Token, Anno).

-spec add_anno_poi(Anno :: erl_anno:anno(),
                   PoiData :: poi_data(),
                   PoiDefinition :: poi_definition() | undefined,
                   Pois :: pois()) -> NewPois :: pois().
add_anno_poi(Anno, PoiData, PoiDefinition, {GbTree0, Set0} = Pois0) ->
    case get_anno_range(Anno) of
        {ok, {StartLocation, EndLocation}} ->
            Poi = #poi{data = PoiData,
                       start_location = StartLocation,
                       definition = PoiDefinition},
            case sets:is_element(StartLocation, Set0) of
                false ->
                    {gb_trees:enter(EndLocation, Poi, GbTree0),
                     sets:add_element(StartLocation, Set0)};
                true ->
                    Pois0
            end;
        undefined ->
            Pois0
    end.

-spec get_anno_range(Anno :: erl_anno:anno()) -> {ok, location_range()} | undefined.
get_anno_range(Anno) ->
    {StartLine, StartColumn} = get_anno_location(Anno),
    case erl_anno:text(Anno) of
        Text when Text =/= undefined ->
            {ok, {{StartLine, StartColumn}, {StartLine, StartColumn + length(Text)}}};
        undefined ->
            undefined
    end.

-spec get_anno_location(Anno :: erl_anno:anno()) -> location().
get_anno_location(Anno) ->
    Location = erl_anno:location(Anno),
    normalize_location(Location).

-spec normalize_location(Location :: erl_anno:location()) -> location().
normalize_location({Line, Column}) ->
    {Line, Column};
normalize_location(Line) ->
    {Line, 1}.

-spec location_to_line(Location :: location()) -> line().
location_to_line({Line, _}) ->
    Line.

-spec location_to_column(Location :: location()) -> line().
location_to_column({_, Column}) ->
    Column.

-spec first_token_location(Tokens :: [token()]) -> location().
first_token_location([FirstToken | _]) ->
    get_token_location(FirstToken).

-spec last_token_location(Tokens :: [token()]) -> location().
last_token_location(Tokens) ->
    LastToken = lists:last(Tokens),
    get_token_location(LastToken).

-spec parse_name_arity_tokens(Tokens :: [token()]) -> {ok, [parsed_name_arity()]} | undefined.
parse_name_arity_tokens(Tokens) ->
    parse_name_arity_tokens(Tokens, []).

-spec parse_name_arity_tokens(Tokens :: [token()], Acc :: [parsed_name_arity()]) ->
          {ok, [parsed_name_arity()]} | undefined.
parse_name_arity_tokens(Tokens, Acc) ->
    case Tokens of
        [{Type, _, Name} = Token, {'/', _}, {integer, _, Arity}, {',', _} | T]
          when Type =:= atom orelse Type =:= var ->
            parse_name_arity_tokens(T, [{{Name, Arity}, Token} | Acc]);
        [{Type, _, Name} = Token, {'/', _}, {integer, _, Arity}, {']', _}, {')', _}, {dot, _}]
          when Type =:= atom orelse Type =:= var ->
            {ok, [{{Name, Arity}, Token} | Acc]};
        _ ->
            undefined
    end.
