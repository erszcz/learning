defmodule ElixirLibs.Mixfile do
  use Mix.Project

  def project do
    [ app: :elixir_libs,
      version: "0.0.1",
      elixir: "~> 1.0",
      description: "An example of using Elixir libs from Erlang",
      package: package,
      deps: deps ]

  end

  defp package do
    [ files: ["src", "priv", "mix.exs"],
      contributors: ["Radek Szymczyszyn"],
      licenses: ["MIT"],
      links: %{} ]
  end

  def deps do
    [{:timex, "0.19.2"}]
  end

end
