defmodule PiHvacWeb.V1.IRDBController do
  use PiHvacWeb, :controller

  require CSV
  alias PiHvac.Api
  alias PiHvac.Api.IRDB
  alias PiHvac.Api.ListIrdbOptions

  action_fallback PiHvacWeb.FallbackController

  defp toNumber(a) do
    a
    |> Kernel.||("")
    |> Integer.parse
    |> case do
      {v, _} -> v
      _ -> nil
    end
  end

  def index(conn, params) do
    # default params
    default =
      %{:limits       => 100,
        :page         => 1,
        :manufacturer => nil,
        :product      => nil
      }
    # shadowing 'params'
    params =
      %{:limits       => params |> Map.get("limits") |> toNumber,
        :page         => params |> Map.get("page") |> toNumber,
        :manufacturer => params |> Map.get("manufacturer"),
        :product      => params |> Map.get("product")}
    |> Enum.reject(fn {_key, val} -> is_nil(val) end)
    |> Map.new
    |> fn a -> Map.merge(default, a) end.()
    #
    opts = %ListIrdbOptions{
      limits:       params.limits,
      offset:       params.limits * (params.page - 1),
      manufacturer: params.manufacturer,
      product:      params.product
    }
    counts = Api.counts_irdb(opts)
    #
    opts
    |> Api.list_irdb
    |> fn v ->
      %{irdb: v,
        limits: params.limits,
        counts: counts,
        page: params.page,
        pages: ceil(counts / params.limits)
      }
    end.()
    |> (&render(conn, "index.json", &1)).()
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

  def manufacturers(conn, _params) do
    irdb = Api.list_irdb_manufacturer()
    render(conn, "manufacturers.json", irdb: irdb)
  end

  def products(conn, _params) do
    irdb = Api.list_irdb_product()
    render(conn, "products.json", irdb: irdb)
  end

end
