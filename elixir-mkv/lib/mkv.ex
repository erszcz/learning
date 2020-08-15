defmodule MKV do
  @moduledoc """
  Documentation for MKV.
  """

  @doc "The REST endpoint listen port"
  def rest_port, do: 4001

  @doc "The TCP (non-REST) endpoint listen port"
  def tcp_port, do: 4002

  @doc "The UDP port to listen on"
  def udp_port, do: 4003

end
