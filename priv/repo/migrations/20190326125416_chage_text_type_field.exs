defmodule PiHvac.Repo.Migrations.ChageTextTypeField do
  use Ecto.Migration

  def change do
    alter table(:irdb) do
      modify :code, :text

    end

  end
end
