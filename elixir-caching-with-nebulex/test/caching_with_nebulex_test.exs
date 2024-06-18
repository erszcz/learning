defmodule CachingWithNebulexTest do
  use ExUnit.Case
  doctest CachingWithNebulex

  test "greets the world" do
    assert CachingWithNebulex.hello() == :world
  end
end
