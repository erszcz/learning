-module(vr_lib).

-export([get/3,
         set/5,
         index/2,
         upgrade/3]).

get(Record, Field, Fields) ->
    case index(Field, Fields) of
        {error, missing_field} -> undefined;
        Index when is_integer(Index), Index > size(Record) -> undefined;
        Index when is_integer(Index) -> element(Index, Record)
    end.

set(Record, Field, Value, Fields, UpgradeFun) ->
    case index(Field, Fields) of
        {error, missing_field} ->
            {error, missing_field};
        Index when is_integer(Index), Index > size(Record) ->
            set(UpgradeFun(Record), Field, Value, Fields, UpgradeFun);
        Index when is_integer(Index) ->
            setelement(Index, Record, Value)
    end.

%% This is just a mapping from a field name to a field number for more convenient use
%% of erlang:element/2 on records-as-tuples.
index(Field, Fields) when is_atom(Field) ->
    case get_field_index(Field, Fields) of
        false -> {error, missing_field};
        {Field, Index} -> Index
    end.

upgrade(Record, NewVersion, FieldValues) ->
    NewVersionName = element(1, NewVersion),
    ExtendedRecord = erlang:make_tuple(size(NewVersion), undefined,
                                       tuple_to_indexed_list(Record) ++ FieldValues),
    setelement(1, ExtendedRecord, NewVersionName).

tuple_to_indexed_list(Tuple) ->
    lists:zip(lists:seq(1, size(Tuple)), tuple_to_list(Tuple)).

get_field_index(Field, Fields) ->
    lists:keyfind(Field, 1, lists:zip(Fields, lists:seq(1, length(Fields)))).
