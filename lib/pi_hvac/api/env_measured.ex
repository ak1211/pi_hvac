defmodule PiHvac.Api.EnvMeasured do
  use Ecto.Schema
  import Ecto.Changeset


  schema "envmeasured" do
    field :degc, :decimal
    field :hpa, :decimal
    field :measured_at, :utc_datetime
    field :rh, :decimal
    field :sensor_id, :string

#    timestamps()
  end

  @doc false
  def changeset(env_measured, attrs) do
    env_measured
    |> cast(attrs, [:measured_at, :degc, :hpa, :rh, :sensor_id])
    |> validate_required([:measured_at, :degc, :hpa, :rh, :sensor_id])
  end
end
