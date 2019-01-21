defmodule PiHvacWeb.EnvMeasuredView do
  use PiHvacWeb, :view
  alias PiHvacWeb.EnvMeasuredView

  def render("index.json", %{envmeasured: envmeasured}) do
    %{data: render_many(envmeasured, EnvMeasuredView, "env_measured.json")}
  end

  def render("show.json", %{env_measured: env_measured}) do
    %{data: render_one(env_measured, EnvMeasuredView, "env_measured.json")}
  end

  def render("env_measured.json", %{env_measured: env_measured}) do
    %{id: env_measured.id,
      measured_at: env_measured.measured_at,
      degc: env_measured.degc,
      hpa: env_measured.hpa,
      rh: env_measured.rh,
      sensor_id: env_measured.sensor_id}
  end
end
