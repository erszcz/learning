defmodule Example.RouterTest do
  use ExUnit.Case
  use Plug.Test

  alias Example.Router

  @content "<html><body>Hi!</body></html>"
  @mimetype "text/html"

  test "returns welcome" do
    conn =
      :get
      |> conn("/", "")
      |> Router.call(opts())

    assert conn.state == :sent
    assert conn.status == 200
    #IO.inspect(conn.resp_body, label: "resp_body")
    assert conn.resp_body == "Welcome"
  end

  #test "returns uploaded" do
  #  conn =
  #    :get
  #    |> conn("/upload?content=#{@content}&mimetype=#{@mimetype}")
  #    |> Router.call(opts())

  #  assert conn.state == :sent
  #  assert conn.status == 201
  #end

  #test "returns 404" do
  #  conn =
  #    :get
  #    |> conn("/missing", "")
  #    |> Router.call(opts())

  #  assert conn.state == :sent
  #  assert conn.status == 404
  #end

  defp opts, do: Router.init([])

end
