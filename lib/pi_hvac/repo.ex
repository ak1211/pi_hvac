defmodule PiHvac.Repo do
  use Ecto.Repo,
    otp_app: :pi_hvac,
    adapter: Ecto.Adapters.Postgres
end
