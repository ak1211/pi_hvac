defmodule PiHvacWeb.V1.IRDBController do
  use PiHvacWeb, :controller

  require CSV
  alias PiHvac.Api
  alias PiHvac.Api.IRDB

  action_fallback PiHvacWeb.FallbackController

  def index(conn, _params) do
    irdb = Api.list_irdb()
    render(conn, "index.json", irdb: irdb)
  end

  def create(conn, %{"irdb" => irdb_params}) do
    with {:ok, %IRDB{} = irdb} <- Api.create_irdb(irdb_params) do
    conn
    |> put_status(:created)
    |> put_resp_header("location", Routes.v1_irdb_path(conn, :show, irdb))
    |> render("show.json", irdb: irdb)
    end
  end

  def show(conn, %{"id" => id}) do
    irdb = Api.get_irdb(id)
    render(conn, "show.json", irdb: irdb)
  end

  def update(conn, %{"id" => id, "irdb" => irdb_params}) do
    irdb = Api.get_irdb(id)

    with {:ok, %IRDB{} = irdb} <- Api.update_irdb(irdb, irdb_params) do
      render(conn, "show.json", irdb: irdb)
    end
  end

  def delete(conn, %{"id" => id}) do
    irdb = Api.get_irdb(id)

    with {:ok, %IRDB{}} <- Api.delete_irdb(irdb) do
      send_resp(conn, :no_content, "")
    end
  end
end
