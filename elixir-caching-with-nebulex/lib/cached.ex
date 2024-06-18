defmodule Cached do
  use Nebulex.Caching

  alias CachingWithNebulex.FactorialCache

  @decorate cacheable(cache: FactorialCache)
  def factorial(n) when n >= 0, do: f_(n)

  defp f_(0), do: 1
  defp f_(n) when n > 0, do: n * f_(n-1)

end
