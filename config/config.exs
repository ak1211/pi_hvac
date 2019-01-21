# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :pi_hvac,
  ecto_repos: [PiHvac.Repo]

# Configures the endpoint
config :pi_hvac, PiHvacWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "4oTXuR8CfVZrdvk/4TojgXGYSoDFAWlKQYuannxYYsuDu03isPn6UDMZcNFqockZ",
  render_errors: [view: PiHvacWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: PiHvac.PubSub, adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
