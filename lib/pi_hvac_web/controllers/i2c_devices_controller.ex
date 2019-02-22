defmodule PiHvacWeb.I2cDevicesController do
  use PiHvacWeb, :controller

  action_fallback PiHvacWeb.FallbackController

  def index(conn, _params) do
    devices = ElixirALE.I2C.detect_devices("i2c-1")
    # i2cdetect default: 0x03 to 0x77
    range = 0x03..0x77
    clipped = devices |> Enum.filter(fn v -> Enum.member?(range, v) end)
    render(conn, "index.json", i2c_devices: clipped)
  end
end
