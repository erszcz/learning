defmodule MKVTest do
  use ExUnit.Case, async: true
  doctest MKV

  test "getting unset key via REST returns 404" do
    ## given
    key = "no_such_key_rest"
    ## when
    r = HTTPoison.get! "http://localhost:#{MKV.rest_port()}/v1/kv/#{key}"
    ## then
    assert r.status_code == 404
  end

  test "getting unset key via TCP returns NotFound" do
    ## given
    key = "no_such_key_tcp"
    socket_opts = [:binary, {:packet, 0}, {:active, :false}]
    {:ok, socket} = :gen_tcp.connect('localhost', MKV.tcp_port(), socket_opts)
    ## when
    :ok = :gen_tcp.send(socket, MKV.Protocol.encode(%MKV.Protocol.Get{key: key}))
    ## then
    {:ok, data} = :gen_tcp.recv(socket, 0)
    decoded = MKV.Protocol.decode!(data)
    assert %MKV.Protocol.NotFound{key: ^key} = decoded
  end

  test "getting unset key via UDP returns NotFound" do
    ## given
    key = "no_such_key_udp"
    socket_opts = [:binary, {:active, :false}]
    {:ok, socket} = :gen_udp.open(0, socket_opts)
    dest = {{127,0,0,1}, MKV.udp_port()}
    ## when
    :ok = :gen_udp.send(socket, dest, MKV.Protocol.encode(%MKV.Protocol.Get{key: key}))
    ## then
    {:ok, {_, _, data}} = :gen_udp.recv(socket, 0)
    decoded = MKV.Protocol.decode!(data)
    assert %MKV.Protocol.NotFound{key: ^key} = decoded
  end

  test "put and get via REST succeeds" do
    ## given
    key = random_string()
    val = random_string()
    put = %MKV.Protocol.Put{key: key, value: val}
    ## when
    HTTPoison.put!("http://localhost:#{MKV.rest_port()}/v1/kv/#{key}",
                   Base.encode64(val))
    ## then
    r = HTTPoison.get! "http://localhost:#{MKV.rest_port()}/v1/kv/#{key}"
    assert val = Base.decode64(r.body)
  end

  test "put via REST and get via TCP succeeds" do
    ## given
    key = random_string()
    val = random_string()
    put = %MKV.Protocol.Put{key: key, value: val}
    ## when
    HTTPoison.put!("http://localhost:#{MKV.rest_port()}/v1/kv/#{key}",
                   Base.encode64(val))
    ## then
    assert Liveness.eventually(fn ->
      socket_opts = [:binary, {:packet, 0}, {:active, :false}]
      {:ok, socket} = :gen_tcp.connect('localhost', MKV.tcp_port(), socket_opts)
      :ok = :gen_tcp.send(socket, MKV.Protocol.encode(%MKV.Protocol.Get{key: key}))
      {:ok, data} = :gen_tcp.recv(socket, 0)
      decoded = MKV.Protocol.decode!(data)
      %MKV.Protocol.Result{key: ^key, value: ^val} = decoded
    end, interval: 100)
  end

  test "put via REST and get via UDP succeeds" do
    ## given
    key = random_string()
    val = random_string()
    put = %MKV.Protocol.Put{key: key, value: val}
    ## when
    HTTPoison.put!("http://localhost:#{MKV.rest_port()}/v1/kv/#{key}",
                   Base.encode64(val))
    ## then
    assert Liveness.eventually(fn ->
      socket_opts = [:binary, {:active, :false}]
      {:ok, socket} = :gen_udp.open(0, socket_opts)
      dest = {{127,0,0,1}, MKV.udp_port()}
      :ok = :gen_udp.send(socket, dest, MKV.Protocol.encode(%MKV.Protocol.Get{key: key}))
      {:ok, {_, _, data}} = :gen_udp.recv(socket, 0)
      decoded = MKV.Protocol.decode!(data)
      %MKV.Protocol.Result{key: ^key, value: ^val} = decoded
    end, interval: 100)
  end

  def random_string, do: :crypto.strong_rand_bytes(20) |> Base.encode64()

end
