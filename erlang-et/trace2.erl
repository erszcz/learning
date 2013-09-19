-module(trace2).

-compile([export_all]).

report_event(DetailLevel, FromTo, Label, Contents) ->
    %% N.B External call
    ?MODULE:report_event(DetailLevel, FromTo, FromTo, Label, Contents).

report_event(_DetailLevel, _From, _To, _Label, _Contents) ->
    hopefully_traced.

test() ->
    Events = [{fred, bob, order_food},
              {bob, hank, order_food},
              {bob, fred, serve_wine},
              {hank, bob, pickup},
              {bob, fred, serve_feed},
              {fred, renee, pay}],
    [trace2:report_event(50, F, T, L, []) || {F,T,L} <- Events].

et() ->
    Options = [{event_order, event_ts},
               {scale, 2},
               {max_actors, 10},
               {detail_level, 90},
               {actors, [fred, bob, hank, renee]},
               {trace_pattern, {trace2, max}},
               {trace_global, true},
               {title, "uTP tracer"}],
    et_viewer:start(Options).
