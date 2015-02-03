-module(mad_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(eq(E, A), ?assertEqual(E, A)).

all() ->
    [sanity_check,
     generate_subnet_addrs,
     parse_addr_or_subnet].

%%
%% Tests
%%

sanity_check(_) ->
    ok.

generate_subnet_addrs(_) ->
    M = mad,
    Addrs = M:generate_subnet_addrs({192,168,100,0}, 22),
    ?eq([{192,168,100,0}, {192,168,100,1}, {192,168,100,2}],
        [ lists:nth(I, Addrs) || I <- lists:seq(1,3) ]),
    ?eq([{192,168,103,253}, {192,168,103,254}, {192,168,103,255}],
        lists:nthtail(1024 - 3, Addrs)).

parse_addr_or_subnet(_) ->
    M = mad,
    ?eq([{192,168,100,254}, {192,168,100,255}],
        M:parse_addr_or_subnet("192.168.100.254/31")),
    ?eq([{192,168,100,252}, {192,168,100,253}],
        M:parse_addr_or_subnet("192.168.100.252/31")),
    ?eq([{192,168,100,254}],
        M:parse_addr_or_subnet("192.168.100.254")).
