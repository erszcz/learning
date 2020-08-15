defmodule MKV.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    MKV.Store.init()

    ranch_opts = [{:port, MKV.tcp_port()}]
    :ranch.start_listener(:tcp_endpoint, :ranch_tcp, ranch_opts, MKV.Ranch.Protocol, [])

    children = [
      # Starts a worker by calling: MKV.Worker.start_link(arg)
      # {MKV.Worker, arg}
      {Plug.Cowboy, scheme: :http, plug: MKV.Router, options: [port: MKV.rest_port()]},
      {Task.Supervisor, name: MKV.UDPHandlerSupervisor},
      {MKV.UDPListener, MKV.udp_port()}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: MKV.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
