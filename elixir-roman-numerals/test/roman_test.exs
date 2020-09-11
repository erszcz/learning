defmodule RomanTest do
  use ExUnit.Case
  doctest Roman

  test "basic conversions" do
    assert Roman.to_string(1)  == "I"
    assert Roman.to_string(2)  == "II"
    assert Roman.to_string(3)  == "III"
    assert Roman.to_string(4)  == "IV"
    assert Roman.to_string(5)  == "V"
    assert Roman.to_string(6)  == "VI"
    assert Roman.to_string(9)  == "IX"
    assert Roman.to_string(10) == "X"
    assert Roman.to_string(11) == "XI"
    assert Roman.to_string(14) == "XIV"
    assert Roman.to_string(19) == "XIX"
    assert Roman.to_string(20) == "XX"
    assert Roman.to_string(21) == "XXI"
    assert Roman.to_string(24) == "XXIV"
    assert Roman.to_string(29) == "XXIX"
    assert Roman.to_string(30) == "XXX"
    assert Roman.to_string(60) == "LX"
    assert Roman.to_string(70) == "LXX"
    assert Roman.to_string(80) == "LXXX"
    assert Roman.to_string(369) == "CCCLXIX"
    assert Roman.to_string(448) == "CDXLVIII"
    assert Roman.to_string(1998) == "MCMXCVIII"
    assert Roman.to_string(2751) == "MMDCCLI"
  end

end
