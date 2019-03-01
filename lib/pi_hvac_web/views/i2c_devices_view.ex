defmodule PiHvacWeb.V1.I2cDevicesView do
  use PiHvacWeb, :view
  alias PiHvacWeb.V1.I2cDevicesView

  def render("index.json", %{i2c_devices: devices}) do
    %{data: render_many(devices, I2cDevicesView, "device_id.json")}
  end

  def render("device_id.json", %{i2c_devices: device_id}) do
    device_id
  end
end
