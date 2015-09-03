-module(vr_SUITE).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("myrec.hrl").

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

all() ->
    [get_on_old_version,
     get_on_latest_version,
     upgrade_from_old_to_new].

%%
%% Tests
%%

get_on_old_version(_Config) ->
    %% given record version 1
    R = #myrec_v1{field1 = a},
    %% when accessing a version 1 field, it's present
    ?eq(a, myrec:get(R, field1)),
    %% when accessing a version 2 field, it's missing
    ?eq(undefined, myrec:get(R, field2)),
    %% when accessing an undefined field, it's missing
    ?eq(undefined, myrec:get(R, undefined_field)).

get_on_latest_version(_Config) ->
    %% given record version 2
    R = #myrec_v2{field1 = a, field2 = b},
    %% when accessing a version 1 field, it's present
    ?eq(a, myrec:get(R, field1)),
    %% when accessing a version 2 field, it's present
    ?eq(b, myrec:get(R, field2)),
    %% when accessing an undefined field, it's missing
    ?eq(undefined, myrec:get(R, undefined_field)).

upgrade_from_old_to_new(_Config) ->
    %% given record version 1
    R = #myrec_v1{field1 = a},
    %% when doing an upgrade it returns record version 2
    %% note that default value of field2 is defined in myrec.erl
    ?eq(#myrec_v2{field1 = a, field2 = b}, myrec:upgrade(R)).
