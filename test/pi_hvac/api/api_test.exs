defmodule PiHvac.ApiTest do
  use PiHvac.DataCase

  alias PiHvac.Api

  describe "envmeasured" do
    alias PiHvac.Api.EnvMeasured

    @valid_attrs %{degc: "120.5", hpa: "120.5", measured_at: "2010-04-17T14:00:00Z", rh: "120.5", sensor_id: "some sensor_id"}
    @update_attrs %{degc: "456.7", hpa: "456.7", measured_at: "2011-05-18T15:01:01Z", rh: "456.7", sensor_id: "some updated sensor_id"}
    @invalid_attrs %{degc: nil, hpa: nil, measured_at: nil, rh: nil, sensor_id: nil}

    def env_measured_fixture(attrs \\ %{}) do
      {:ok, env_measured} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Api.create_env_measured()

      env_measured
    end

    test "list_envmeasured/0 returns all envmeasured" do
      env_measured = env_measured_fixture()
      assert Api.list_envmeasured() == [env_measured]
    end

    test "get_env_measured!/1 returns the env_measured with given id" do
      env_measured = env_measured_fixture()
      assert Api.get_env_measured!(env_measured.id) == env_measured
    end

    test "create_env_measured/1 with valid data creates a env_measured" do
      assert {:ok, %EnvMeasured{} = env_measured} = Api.create_env_measured(@valid_attrs)
      assert env_measured.degc == Decimal.new("120.5")
      assert env_measured.hpa == Decimal.new("120.5")
      assert env_measured.measured_at == DateTime.from_naive!(~N[2010-04-17T14:00:00Z], "Etc/UTC")
      assert env_measured.rh == Decimal.new("120.5")
      assert env_measured.sensor_id == "some sensor_id"
    end

    test "create_env_measured/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Api.create_env_measured(@invalid_attrs)
    end

    test "update_env_measured/2 with valid data updates the env_measured" do
      env_measured = env_measured_fixture()
      assert {:ok, %EnvMeasured{} = env_measured} = Api.update_env_measured(env_measured, @update_attrs)
      assert env_measured.degc == Decimal.new("456.7")
      assert env_measured.hpa == Decimal.new("456.7")
      assert env_measured.measured_at == DateTime.from_naive!(~N[2011-05-18T15:01:01Z], "Etc/UTC")
      assert env_measured.rh == Decimal.new("456.7")
      assert env_measured.sensor_id == "some updated sensor_id"
    end

    test "update_env_measured/2 with invalid data returns error changeset" do
      env_measured = env_measured_fixture()
      assert {:error, %Ecto.Changeset{}} = Api.update_env_measured(env_measured, @invalid_attrs)
      assert env_measured == Api.get_env_measured!(env_measured.id)
    end

    test "delete_env_measured/1 deletes the env_measured" do
      env_measured = env_measured_fixture()
      assert {:ok, %EnvMeasured{}} = Api.delete_env_measured(env_measured)
      assert_raise Ecto.NoResultsError, fn -> Api.get_env_measured!(env_measured.id) end
    end

    test "change_env_measured/1 returns a env_measured changeset" do
      env_measured = env_measured_fixture()
      assert %Ecto.Changeset{} = Api.change_env_measured(env_measured)
    end
  end
end
