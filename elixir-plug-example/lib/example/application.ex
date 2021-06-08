defmodule Example.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application
  require Logger

  @impl true
  def start(_type, _args) do
    children = [
      # Starts a worker by calling: Example.Worker.start_link(arg)
      # {Example.Worker, arg}
      {Plug.Cowboy, scheme: :http, plug: Example.Router, options: [port: 18080]}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Example.Supervisor]
    Logger.info("Starting application...")
    Supervisor.start_link(children, opts)
  end
end
