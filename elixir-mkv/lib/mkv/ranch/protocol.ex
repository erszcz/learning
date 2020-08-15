defmodule MKV.Ranch.Protocol do
  # based on:
  # - https://github.com/ninenines/ranch/blob/master/examples/tcp_echo/src/echo_protocol.erl
  # - https://github.com/ninenines/ranch/blob/master/examples/tcp_reverse/src/reverse_protocol.erl
  # - https://blog.oestrich.org/2017/07/using-ranch-with-elixir/
  use GenServer
  require Logger

  @behaviour :ranch_protocol

  def start_link(ref, socket, transport, _opts) do
    pid = :proc_lib.spawn_link(__MODULE__, :init, [ref, socket, transport])
    {:ok, pid}
  end

  def init(init_arg) do
    {:ok, init_arg}
  end

  def init(ref, socket, transport) do
    Logger.info "Starting #{__MODULE__}"
    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, [{:active, :once}])
    :gen_server.enter_loop(__MODULE__, [], %{socket: socket, transport: transport})
  end

  def handle_info({:tcp, socket, data} = segment, state = %{socket: socket, transport: transport}) do
    Logger.info "handle_info: #{inspect segment}"
    transport.setopts(socket, [{:active, :once}])
    state = handle_data(data, state)
    {:noreply, state}
  end
  def handle_info({:tcp_closed, socket}, state = %{socket: socket, transport: transport}) do
    Logger.debug "Closing #{__MODULE__}"
    transport.close(socket)
    {:stop, :normal, state}
  end

  defp handle_data(data, state = %{socket: socket, transport: transport}) do
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
        transport.send(socket, MKV.Protocol.encode(response))
    end
    state
  end

end
