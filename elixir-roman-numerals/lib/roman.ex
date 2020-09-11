defmodule Roman do
  @moduledoc """
  Documentation for `Roman`.
  """

  def to_string(n), do: to_string(n, 0, [])

  def to_string( 0, _, roman), do: :erlang.iolist_to_binary(roman)
  def to_string( n, rank, roman) do
    {rest, units} = {div(n, 10), rem(n, 10)}
    to_string(rest, rank + 1, [roman_digit(units, rank) | roman])
  end

  def roman_digit(0, _), do: ""
  def roman_digit(1, 0), do: "I"
  def roman_digit(2, 0), do: "II"
  def roman_digit(3, 0), do: "III"
  def roman_digit(4, 0), do: "IV"
  def roman_digit(5, 0), do: "V"
  def roman_digit(6, 0), do: "VI"
  def roman_digit(7, 0), do: "VII"
  def roman_digit(8, 0), do: "VIII"
  def roman_digit(9, 0), do: "IX"

  def roman_digit(1, 1), do: "X"
  def roman_digit(2, 1), do: "XX"
  def roman_digit(3, 1), do: "XXX"
  def roman_digit(4, 1), do: "XL"
  def roman_digit(5, 1), do: "L"
  def roman_digit(6, 1), do: "LX"
  def roman_digit(7, 1), do: "LXX"
  def roman_digit(8, 1), do: "LXXX"
  def roman_digit(9, 1), do: "XC"

  def roman_digit(1, 2), do: "C"
  def roman_digit(2, 2), do: "CC"
  def roman_digit(3, 2), do: "CCC"
  def roman_digit(4, 2), do: "CD"
  def roman_digit(5, 2), do: "D"
  def roman_digit(6, 2), do: "DC"
  def roman_digit(7, 2), do: "DCC"
  def roman_digit(8, 2), do: "DCCC"
  def roman_digit(9, 2), do: "CM"

  def roman_digit(1, 3), do: "M"
  def roman_digit(2, 3), do: "MM"
  def roman_digit(3, 3), do: "MMM"

end
