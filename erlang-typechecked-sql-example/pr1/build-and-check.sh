./rebar compile
[ -f dialyzer.plt ] || dialyzer --build_plt --output_plt dialyzer.plt ebin/*.beam
dialyzer --check_plt --plt dialyzer.plt --output_plt dialyzer.plt ebin/*.beam
dialyzer --plt dialyzer.plt ebin/pr1_app.beam ebin/pr1_sup.beam
