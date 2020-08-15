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
    # In a real life case we should rather make this process stick around for a while
    # to receive further dgrams from ip:in_port_no.
    Task.Supervisor.start_child(MKV.UDPHandlerSupervisor, fn ->
      handle_datagram(socket, {ip, in_port_no}, data)
    end)
    {:noreply, state}
  end

  def handle_datagram(socket, dest, data) do
    decoded = MKV.Protocol.decode!(data)
    Logger.info "handling: #{inspect decoded}"
    case decoded do
      %MKV.Protocol.Put{key: k, value: v} ->
        MKV.Store.put(%MKV.Entry{key: k, value: v})
      %MKV.Protocol.Get{key: k} ->
        response = case MKV.Store.get(k, :not_found) do
          :not_found ->
            %MKV.Protocol.NotFound{key: k}
          {^k, v} ->
            %MKV.Protocol.Result{key: k, value: v}
        end
        :gen_udp.send(socket, dest, MKV.Protocol.encode(response))
    end
  end

end
