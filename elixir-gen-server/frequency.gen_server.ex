defmodule Frequency.GenServer do
  use GenServer

  def start() do
    GenServer.start(__MODULE__, :no_opts, name: __MODULE__)
  end

  def init(:no_opts) do
    {:ok, {get_frequencies(), []}}
  end

  defp get_frequencies() do
    [10, 11, 12, 13, 14, 15]
  end

  def stop(),           do: GenServer.call(__MODULE__, :stop)
  def allocate(),       do: GenServer.call(__MODULE__, :allocate)
  def deallocate(freq), do: GenServer.call(__MODULE__, {:deallocate, freq})

  def handle_call(:allocate, pid, frequencies) do
    {frequencies, reply} = allocate(frequencies, pid)
    {:reply, reply, frequencies}
  end
  def handle_call({:deallocate, freq}, _, frequencies) do
    frequencies = deallocate(frequencies, freq)
    {:reply, :ok, frequencies}
  end
  def handle_call(:stop, _, frequencies) do
    {:stop, :normal, :ok, frequencies}
  end

  defp allocate({[], allocated}, _pid) do
    {{[], allocated}, {:error, :no_frequencies}}
  end
  defp allocate({[freq | free], allocated}, pid) do
    {{free, [{freq, pid} | allocated]}, {:ok, freq}}
  end

  defp deallocate({free, allocated}, freq) do
    allocated = List.keydelete(allocated, freq, 0)
    {[freq | free], allocated}
  end

end
