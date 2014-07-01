-file("test2_SUITE.erl", 1).

-file("../deps/proper_eunit/include/pt_proper_eunit.hrl", 1).

-file("../deps/proper/include/proper.hrl", 1).

-file("../deps/proper/include/proper_common.hrl", 1).

-file("../deps/proper/include/proper.hrl", 29).

-file("", 29).

-file("../deps/proper_eunit/include/pt_proper_eunit.hrl", 5).

-file("", 5).

-file("/home/erszcz/apps/erlang/r16b/lib/eunit-2.2.4/include/eunit.hrl",
      1).

-file("../deps/proper_eunit/include/pt_proper_eunit.hrl", 7).

-file("", 7).

-file("test2_SUITE.erl", 6).

foo(Test) ->
    {setup,
     % fun-info: {0,0,'-foo/1-fun-0-'}
     fun() ->
            zzz
     end,
     Test}.

prop_1() ->
    proper:forall(proper_types:integer(0, 10),
                  % fun-info: {0,0,'-prop_1/0-fun-0-'}
                  fun(X) ->
                         X =:= X
                  end).

prop_1_test_() ->
    foo({11,
         % fun-info: {0,0,'-prop_1_test_/0-fun-0-'}
         fun() ->
                true = proper:quickcheck(prop_1(), [])
         end}).

test() ->
    eunit:test(test2_SUITE).

module_info() ->
    erlang:get_module_info(test2_SUITE).

module_info(X) ->
    erlang:get_module_info(test2_SUITE, X).

