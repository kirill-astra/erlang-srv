-module(esrv_config).

-export([create/0,
         fetch_value/1,
         get_value/1,
         get_value/2,
         set_value/2]).

-type key() :: atom().
-type value() :: any().

-define(TABLE_NAME, esrv_config).

-spec create() -> ok.
create() ->
    ets:new(?TABLE_NAME, [named_table, public]), ok.

-spec fetch_value(Key :: key()) -> value().
fetch_value(Key) ->
    {ok, Value} = get_value(Key),
    Value.

-spec get_value(Key :: key()) -> {ok, value()} | undefined.
get_value(Key) ->
    case ets:lookup(?TABLE_NAME, Key) of
        [{Key, Value}] ->
            {ok, Value};
        [] ->
            undefined
    end.

-spec get_value(Key :: key(), Default :: value()) -> value().
get_value(Key, Default) ->
    case get_value(Key) of
        {ok, Value} ->
            Value;
        undefined ->
            Default
    end.

-spec set_value(Key :: key(), Value :: value()) -> ok.
set_value(Key, Value) ->
    ets:insert(?TABLE_NAME, {Key, Value}), ok.
