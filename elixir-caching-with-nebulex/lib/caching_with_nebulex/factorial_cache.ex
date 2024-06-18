defmodule CachingWithNebulex.FactorialCache do
  use Nebulex.Cache,
    otp_app: :caching_with_nebulex,
    adapter: Nebulex.Adapters.Local,
    default_key_generator: __MODULE__

  @behaviour Nebulex.Caching.KeyGenerator

  @impl true
  def generate(mod, fun, [n]), do: {mod, fun, n}
end
