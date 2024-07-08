defmodule CastVsCallTest do
  use ExUnit.Case
  doctest CastVsCall

  test "greets the world" do
    assert CastVsCall.hello() == :world
  end
end
