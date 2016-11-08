defmodule Frequency.Simple do

  def start() do
    pid = spawn(Frequency.Simple, :init, [])
    Process.register(pid, :frequency)
  end

  def init() do
    frequencies = {get_frequencies(), []}
    loop(frequencies)
  end

  defp get_frequencies() do
    [10, 11, 12, 13, 14, 15]
  end

  def stop(),           do: call(:stop)
  def allocate(),       do: call(:allocate)
  def deallocate(freq), do: call({:deallocate, freq})

  defp call(message) do
    send(:frequency, {:request, self(), message})
    receive do
      {:reply, reply} -> reply
    end
  end

  defp reply(pid, message) do
    send(pid, {:reply, message})
  end

  defp loop(frequencies) do
    receive do
      {:request, pid, :allocate} ->
        {frequencies, reply} = allocate(frequencies, pid)
        reply(pid, reply)
        loop(frequencies)
      {:request, pid, {:deallocate, freq}} ->
        frequencies = deallocate(frequencies, freq)
        reply(pid, :ok)
        loop(frequencies)
      {:request, pid, :stop} ->
        reply(pid, :ok)
    end
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
