defmodule MKV.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    MKV.Store.init()

    children = [
      # Starts a worker by calling: MKV.Worker.start_link(arg)
      # {MKV.Worker, arg}
      {Plug.Cowboy, scheme: :http, plug: MKV.Router, options: [port: 4001]}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: MKV.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
