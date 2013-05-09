#!/usr/bin/env escript

main(_) ->
    io:format("~s~n", [erlang:system_info(otp_release)]).
