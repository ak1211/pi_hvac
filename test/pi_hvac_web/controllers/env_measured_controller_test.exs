defmodule PiHvacWeb.EnvMeasuredControllerTest do
  use PiHvacWeb.ConnCase

  alias PiHvac.Api
  alias PiHvac.Api.EnvMeasured

  @create_attrs %{
    degc: "120.5",
    hpa: "120.5",
    measured_at: "2010-04-17T14:00:00Z",
    rh: "120.5",
    sensor_id: "some sensor_id"
  }
  @update_attrs %{
    degc: "456.7",
    hpa: "456.7",
    measured_at: "2011-05-18T15:01:01Z",
    rh: "456.7",
    sensor_id: "some updated sensor_id"
  }
  @invalid_attrs %{degc: nil, hpa: nil, measured_at: nil, rh: nil, sensor_id: nil}

  def fixture(:env_measured) do
    {:ok, env_measured} = Api.create_env_measured(@create_attrs)
    env_measured
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all envmeasured", %{conn: conn} do
      conn = get(conn, Routes.env_measured_path(conn, :index))
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create env_measured" do
    test "renders env_measured when data is valid", %{conn: conn} do
      conn = post(conn, Routes.env_measured_path(conn, :create), env_measured: @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get(conn, Routes.env_measured_path(conn, :show, id))

      assert %{
               "id" => id,
               "degc" => "120.5",
               "hpa" => "120.5",
               "measured_at" => "2010-04-17T14:00:00Z",
               "rh" => "120.5",
               "sensor_id" => "some sensor_id"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.env_measured_path(conn, :create), env_measured: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update env_measured" do
    setup [:create_env_measured]

    test "renders env_measured when data is valid", %{conn: conn, env_measured: %EnvMeasured{id: id} = env_measured} do
      conn = put(conn, Routes.env_measured_path(conn, :update, env_measured), env_measured: @update_attrs)
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get(conn, Routes.env_measured_path(conn, :show, id))

      assert %{
               "id" => id,
               "degc" => "456.7",
               "hpa" => "456.7",
               "measured_at" => "2011-05-18T15:01:01Z",
               "rh" => "456.7",
               "sensor_id" => "some updated sensor_id"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, env_measured: env_measured} do
      conn = put(conn, Routes.env_measured_path(conn, :update, env_measured), env_measured: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete env_measured" do
    setup [:create_env_measured]

    test "deletes chosen env_measured", %{conn: conn, env_measured: env_measured} do
      conn = delete(conn, Routes.env_measured_path(conn, :delete, env_measured))
      assert response(conn, 204)

      assert_error_sent 404, fn ->
        get(conn, Routes.env_measured_path(conn, :show, env_measured))
      end
    end
  end

  defp create_env_measured(_) do
    env_measured = fixture(:env_measured)
    {:ok, env_measured: env_measured}
  end
end
