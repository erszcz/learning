defmodule Community.Repo do
  use Ecto.Repo,
    otp_app: :community,
    adapter: Ecto.Adapters.Postgres
end
