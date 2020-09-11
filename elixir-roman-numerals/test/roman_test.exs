defmodule RomanTest do
  use ExUnit.Case
  doctest Roman

  test "greets the world" do
    assert Roman.hello() == :world
  end
end
