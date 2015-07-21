-module(utp_filter).

-compile([export_all]).

start(ExtraOptions) ->
    Options =
        [{event_order, event_ts},
         {scale, 2},
         {max_actors, 10},
         {detail_level, 90},
         %{actors, [fred, bob, hank, renee]},
         %% Works just as well!
         {actors, []},
         {trace_pattern, {et, max}},
         {trace_global, true},
         {title, "uTP tracer"} | ExtraOptions],
    et_viewer:start(Options).
