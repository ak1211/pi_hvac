defmodule PiHvacWeb.V1.EnvMeasuredController do
  use PiHvacWeb, :controller

  alias PiHvac.Api
  alias PiHvac.Api.EnvMeasured

  action_fallback PiHvacWeb.FallbackController

  def index(conn, params) do
    env_measured = Api.list_env_measured(params)
    render(conn, "index.json", env_measured: env_measured)
  end

  def create(conn, %{"env_measured" => env_measured_params}) do
    with {:ok, %EnvMeasured{} = env_measured} <- Api.create_env_measured(env_measured_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.v1_env_measured_path(conn, :show, env_measured))
      |> render("show.json", env_measured: env_measured)
    end
  end

  def show(conn, %{"id" => id}) do
    env_measured = Api.get_env_measured!(id)
    render(conn, "show.json", env_measured: env_measured)
  end

  def update(conn, %{"id" => id, "env_measured" => env_measured_params}) do
    env_measured = Api.get_env_measured!(id)

    with {:ok, %EnvMeasured{} = env_measured} <- Api.update_env_measured(env_measured, env_measured_params) do
      render(conn, "show.json", env_measured: env_measured)
    end
  end

  def delete(conn, %{"id" => id}) do
    env_measured = Api.get_env_measured!(id)

    with {:ok, %EnvMeasured{}} <- Api.delete_env_measured(env_measured) do
      send_resp(conn, :no_content, "")
    end
  end
end
