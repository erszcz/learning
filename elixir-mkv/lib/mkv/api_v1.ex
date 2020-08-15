defmodule MKV.ApiV1 do
  use Plug.Router
  require Logger

  plug :match
  plug :dispatch

  get "/kv/:id" do
    case MKV.Store.get(id, :not_found) do
      :not_found ->
        send_resp(conn, 404, "")
      {^id, value} ->
        send_resp(conn, 200, Base.encode64(value))
    end
  end

  put "/kv/:id" do
    {:ok, body, conn} = Plug.Conn.read_body(conn, length: 1_000_000)
    entry = %MKV.Entry{key: id, value: Base.decode64!(body)}
    MKV.Store.put(entry)
    send_resp(conn, 200, "")
  end

  match _ do
    send_resp(conn, 404, "/v1/: oops")
  end

end
