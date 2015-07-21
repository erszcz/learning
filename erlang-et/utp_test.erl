-module(utp_test).

-compile([export_all]).

trace_test() ->
    Events = [{fred, bob, order_food},
              {bob, hank, order_food},
              {bob, fred, serve_wine},
              {hank, bob, pickup},
              {bob, fred, serve_feed},
              {fred, renee, pay},
              {fred, fred, eat}],
    [et:trace_me(50, F, T, L, [])
     || {F,T,L} <- Events].
