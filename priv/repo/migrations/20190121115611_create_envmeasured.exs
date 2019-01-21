defmodule PiHvac.Repo.Migrations.CreateEnvmeasured do
  use Ecto.Migration

  def change do
    create table(:envmeasured) do
      add :measured_at, :utc_datetime
      add :degc, :decimal
      add :hpa, :decimal
      add :rh, :decimal
      add :sensor_id, :string

#      timestamps()
    end

  end
end
