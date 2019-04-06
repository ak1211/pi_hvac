defmodule PiHvacWeb.V1.EnvMeasuredView do
  use PiHvacWeb, :view
  alias PiHvacWeb.V1.EnvMeasuredView

  def render("index.json", %{env_measured: env_measured}) do
    %{env: render_many(env_measured, EnvMeasuredView, "env_measured.json")}
  end

  def render("show.json", %{env_measured: env_measured}) do
    %{env: render_one(env_measured, EnvMeasuredView, "env_measured.json")}
  end

  def render("env_measured.json", %{env_measured: env_measured}) do
    %{id: env_measured.id,
      measured_at: env_measured.measured_at,
      degc: env_measured.degc |> Decimal.to_float,
      hpa: env_measured.hpa |> Decimal.to_float,
      rh: env_measured.rh |> Decimal.to_float,
      sensor_id: env_measured.sensor_id}
  end
end
