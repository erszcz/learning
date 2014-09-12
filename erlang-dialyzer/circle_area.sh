#!/bin/sh

erlc +debug_info circle_area.erl circle_area_break_encapsulation.erl
dialyzer --build_plt --output_plt dialyzer.plt *.beam
dialyzer --plts dialyzer.plt -- *.beam

# Expect:
#
#  $ dialyzer --plts dialyzer.plt -- *.beam
#    Checking whether the PLT dialyzer.plt is up-to-date... yes
#    Proceeding with analysis...
#  circle_area_break_encapsulation.erl:5: Function main/0 has no local return
#  circle_area_break_encapsulation.erl:7: The call erlang:element(2,Area::circle_area:circle_area()) contains an opaque term as 2nd argument when terms of different types are expected in these positions
#  Unknown functions:
#    erlang:get_module_info/1
#    erlang:get_module_info/2
#    math:pi/0
#    math:pow/2
#   done in 0m0.13s
#  done (warnings were emitted)
#
