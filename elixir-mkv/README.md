# MKV

**TODO: Add description**

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `mkv` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:mkv, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/mkv](https://hexdocs.pm/mkv).


## Manual testing

First, run the app - development mode is fine:

```
iex -S mix
```

Then, store and retrieve a value with curl:

```
curl -X PUT -d $(echo asd | base64) http://localhost:4001/v1/kv/a
curl http://localhost:4001/v1/kv/a | base64 -d
```
