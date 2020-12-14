defmodule MyServer do
  use GenServer

  @doc false
  def start_link do
    GenServer.start_link(__MODULE__, Map.new)
  end

  @impl true
  def init(state) do
    {:ok, state}
  end

end
