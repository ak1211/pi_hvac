defmodule PiHvac.Repo.Migrations.CreateIrdb do
  use Ecto.Migration

  def change do
    create table(:irdb) do
      add :manufacturer, :string
      add :product, :string
      add :key, :string
      add :code, :string

      #      timestamps()
    end

  end
end
