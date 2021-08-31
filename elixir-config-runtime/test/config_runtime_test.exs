defmodule ConfigRuntimeTest do
  use ExUnit.Case
  doctest ConfigRuntime

  test "greets the world" do
    assert ConfigRuntime.hello() == :world
  end
end
