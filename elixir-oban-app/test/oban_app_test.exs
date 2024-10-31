defmodule ObanAppTest do
  use ExUnit.Case
  doctest ObanApp

  test "greets the world" do
    assert ObanApp.hello() == :world
  end
end
