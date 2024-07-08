defmodule CastVsCall.Server do
  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, %{})
  end

  # When we call this with a pid of a dead process, it returns an error
  def call_it(pid) do
    GenServer.call(pid, :call_it)
  end

  # When we call this with a pid of a dead process, it fails silently
  def cast_it(pid) do
    GenServer.cast(pid, :cast_it)
  end

  # Callbacks

  @impl true
  def init(%{} = state) do
    {:ok, state}
  end

  @impl true
  def handle_call(message, _from, state) do
    IO.puts("got #{inspect(message)} via GenServer.call")
    {:reply, :ok, state}
  end

  @impl true
  def handle_cast(message, state) do
    IO.puts("got #{inspect(message)} via GenServer.cast")
    {:noreply, state}
  end
end
