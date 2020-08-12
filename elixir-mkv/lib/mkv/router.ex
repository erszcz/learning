defmodule MKV.Router do
  use Plug.Router

  plug Plug.Logger
  plug :match
  plug :dispatch

  forward "/v1", to: MKV.ApiV1

  match _ do
    send_resp(conn, 404, "/: oops")
  end

end
