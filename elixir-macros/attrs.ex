defmodule Attrs do
  @doc ~S"""
  This module shows whether it's possible to define an attribute conditionally based on an env var.
  TL;DR: Yes, it is.
  """

  case System.get_env("A_IS_A") do
    "true" ->
      @a "A"
    _ ->
      @a "B"
  end

  def test do
    @a
  end
end
