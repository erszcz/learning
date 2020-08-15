defmodule MKV.UDPListener do
  use GenServer
  require Logger

  defmodule State do
    defstruct [:port, :socket]
  end

  def start_link(port) do
    GenServer.start_link(__MODULE__, %State{port: port})
  end

  def init(state) do
    Logger.info "Starting #{__MODULE__}"
    {:ok, socket} = :gen_udp.open(state.port, [{:active, :once}, :binary, {:reuseaddr, :true}])
    {:ok, %{state | socket: socket}}
  end

  def handle_info({:udp, socket, ip, in_port_no, data} = dgram, state) do
    Logger.info "handle_info: #{inspect dgram}"
    :inet.setopts(socket, [{:active, :once}])
    # For real, we should rather make this process stick around for a while
    # to receive further dgrams from ip:in_port_no.
    Task.Supervisor.start_child(MKV.UDPHandlerSupervisor, fn ->
      handle_datagram(socket, {ip, in_port_no}, data)
    end)
    {:noreply, state}
  end

  def handle_datagram(socket, dest, data) do
    Logger.debug "handling data: #{data}"
    case MKV.Protocol.decode!(data) do
      %MKV.Protocol.Put{key: k, value: v} ->
        MKV.Store.put(%MKV.Entry{key: k, value: v})
      %MKV.Protocol.Get{key: k} ->
        v = MKV.Store.get!(k)
        response = %MKV.Protocol.Put{key: k, value: v}
        :gen_udp.send(socket, dest, MKV.Protocol.encode(response))
    end
  end

end
