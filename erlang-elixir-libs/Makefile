shell: bootstrap
	. activate && mix
	erl -pa deps/elixir/lib/*/ebin _build/dev/lib/*/ebin

bootstrap: $(PWD)/deps/elixir/bin/iex
	. activate && mix deps.get && mix deps.compile

$(PWD)/deps/elixir/bin/iex:
	./rebar get-deps compile
	@echo "Freestanding Elixir env is ready. Activate it with: source activate"
