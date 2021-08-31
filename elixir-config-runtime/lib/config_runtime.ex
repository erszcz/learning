defmodule ConfigRuntime do
  @moduledoc """
  Documentation for `ConfigRuntime`.
  """

  @opt1 Application.compile_env!(:config_runtime_test, :opt1)


  def hello do
    "hello #{@opt1}"
  end
end
