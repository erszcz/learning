./rebar compile
dialyzer --check_plt --plt dialyzer.plt --output_plt dialyzer.plt ebin/*
dialyzer --plt dialyzer.plt ebin/pr1_app.beam ebin/pr1_sup.beam
