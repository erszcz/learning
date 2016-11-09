defmodule FibAgent do

  def start_link do
    cache = Enum.into([{0, 0}, {1, 1}], Map.new)
    Agent.start_link(fn -> cache end)
  end

  def fib(pid, n) when n >= 0 do
    Agent.get_and_update(pid, &do_fib(&1, n))
  end

  defp do_fib(cache, n) do
    if cached = cache[n] do
      {cached, cache}
    else
      {val, cache} = do_fib(cache, n - 1)
      result = val + cache[n-2]
      {result, Map.put(cache, n, result)}
    end
  end

end

defmodule FibAgent.Main do

  def main do
    {:ok, agent} = FibAgent.start_link()
    IO.puts FibAgent.fib(agent, 2000)
  end

end
