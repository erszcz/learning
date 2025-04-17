defmodule ProtocolsTest do
  use ExUnit.Case
  doctest Protocols

  test "greets the world" do
    assert Protocols.hello() == :world
  end
end
