defmodule ConfigRuntime do
  @moduledoc """
  Documentation for `ConfigRuntime`.
  """

  @opt1 Application.compile_env!(:config_runtime_test, :opt1)

  defmodule Config do
    def opt1, do: Application.get_env(:config_runtime_test, :opt1, "default value 1")
  end

  def hello1 do
    "hello #{@opt1}"
  end

  def hello2 do
    "hello #{config()[:opt1]}"
  end

  defp config do
    defaults = %{opt1: "default value 1"}
    config = Application.get_env(:config_runtime_test, __MODULE__, [])
             |> Enum.into %{}
    Map.merge(defaults, config)
  end
end
