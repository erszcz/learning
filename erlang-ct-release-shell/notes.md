`ct:break/1` doesn't work with `rebar ct` (no matter whether with `-v` or
without it, though without it you won't even see it tried).
The reason is rebar launches `ct_run` with `-noshell` which obviously
messes up the interactivity.

Using rebar's executed command after tweaking:

    ct_run -pa "/Users/erszcz/work/learning/erlang-ct-release-shell/ebin"
    "/Users/erszcz/work/learning/erlang-ct-release-shell/rebar/rebar/ebin"
    "/Users/erszcz/work/learning/erlang-ct-release-shell/rebar"
    "/Users/erszcz/work/learning/erlang-ct-release-shell/."
    "/Users/erszcz/.erlang.d"   -name test@x4.local -logdir
    "/Users/erszcz/work/learning/erlang-ct-release-shell/logs" -env TEST_DIR
    "/Users/erszcz/work/learning/erlang-ct-release-shell/test"  -dir test
    2>&1 | tee -a
    /Users/erszcz/work/learning/erlang-ct-release-shell/logs/raw.log

works, i.e. CT drops to the shell and can be resumed with `ct:continue/0`.
