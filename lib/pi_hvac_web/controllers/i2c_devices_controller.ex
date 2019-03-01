defmodule PiHvacWeb.V1.I2cDevicesController do
  use PiHvacWeb, :controller

  action_fallback PiHvacWeb.FallbackController

  # i2cdetect default: 0x03 to 0x77
  @i2c_range 0x03..0x77

  def index(conn, params) do
    default = %{"busnum" => 1}
    %{"busnum" => n} = Map.merge(default, params)
    device_name = "i2c-#{n}"
    #
    if ElixirALE.I2C.device_names |> Enum.member?(device_name) do
      devices = ElixirALE.I2C.detect_devices(device_name)
      clipped = Enum.filter(devices, fn v -> Enum.member?(@i2c_range, v) end)
      render(conn, "index.json", i2c_devices: clipped)
    else
      render(conn, "index.json", i2c_devices: [])
    end
  end
end
