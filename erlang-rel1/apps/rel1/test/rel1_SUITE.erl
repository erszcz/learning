-module(rel1_SUITE).
-compile([export_all]).

suite() -> [].

all() -> [just_a_test].

just_a_test(_Config) ->
    ct:fail("ooopsie").
