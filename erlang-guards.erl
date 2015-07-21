-module('erlang-guards').

-compile([export_all]).

%% > 'erlang-guards':is_valid_rid1(555, undef).
%% ** exception error: bad argument in an arithmetic expression
%%      in function  guards:is_valid_rid1/2 (guards.erl, line 8)
is_valid_rid1(Rid, OldRid) ->
    Rid == (OldRid + 1).

%% > 'erlang-guards':is_valid_rid2(555, undef).
%% false
is_valid_rid2(Rid, OldRid) when Rid == OldRid + 1 ->
    true;
is_valid_rid2(_,_) ->
    false.
