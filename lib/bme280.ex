#
# PiHVAC <https://github.com/ak1211/pi_hvac>
# Copyright 2019 Akihiro Yamamoto
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# module: Bosch BME280 デバイスドライバ

defmodule BME280 do
  @moduledoc """
  Bosch BME280 Temperature / Pressure / Humidity sensor module driver on the Raspberry Pi.
  """

  @behaviour GenServer

  alias BME280.Driver
  alias BME280.Calibration, as: C

  defmodule SensedValue do
    defstruct degc: nil, hpa: nil, rh: nil
    @typedoc "BME280 sensed value"
    @type t :: %SensedValue{degc: float, hpa: float, rh: float}
  end

  defmodule CalibData do
    defstruct dig_T1: nil, dig_T2: nil, dig_T3: nil,
              dig_P1: nil, dig_P2: nil, dig_P3: nil, dig_P4: nil, dig_P5: nil, dig_P6: nil,
              dig_P7: nil, dig_P8: nil, dig_P9: nil,
              dig_H1: nil, dig_H2: nil, dig_H3: nil, dig_H4: nil, dig_H5: nil, dig_H6: nil
    @type t :: %CalibData{
      dig_T1: integer, dig_T2: integer, dig_T3: integer,
      dig_P1: integer, dig_P2: integer, dig_P3: integer, dig_P4: integer, dig_P5: integer, dig_P6: integer,
      dig_P7: integer, dig_P8: integer, dig_P9: integer,
      dig_H1: integer, dig_H2: integer, dig_H3: integer, dig_H4: integer, dig_H5: integer, dig_H6: integer}
    @type t(t1, t2, t3, p1, p2, p3, p4, p5, p6, p7, p8, p9, h1, h2, h3, h4, h5, h6) :: %CalibData{
      dig_T1: t1, dig_T2: t2, dig_T3: t3,
      dig_P1: p1, dig_P2: p2, dig_P3: p3, dig_P4: p4, dig_P5: p5, dig_P6: p6,
      dig_P7: p7, dig_P8: p8, dig_P9: p9,
      dig_H1: h1, dig_H2: h2, dig_H3: h3, dig_H4: h4, dig_H5: h5, dig_H6: h6}
  end

  defmodule State do
    defstruct i2c: nil, calib_data: nil
    @type t :: %State{i2c: pid(), calib_data: BME280.CalibData}
  end

  #
  # Public API
  #

  def start_link(i2c) do
    GenServer.start_link(__MODULE__, i2c, name: __MODULE__)
  end

  def read() do
    GenServer.call(__MODULE__, :read)
  end

  #
  # GenServer callbacks
  #

  @impl GenServer
  def init(i2c) do
    :ok   = Driver.init_sensor(i2c)
    :ok   = Driver.setup_sensor(i2c)
    data  = Driver.get_calib_data(i2c)
    state = %State{i2c: i2c, calib_data: data}
    {:ok, state}
  end

  @impl GenServer
  def handle_call(:read, _from, state) do
    adc = Driver.get_sensor_data(state.i2c)
    cal = state.calib_data
    t_fine = C.temperature_fine(adc.t, cal)
    value = %BME280.SensedValue{
      degc: C.compensate_temperature(t_fine, cal, adc.t) |> Float.round(2),
      hpa: C.compensate_pressure(t_fine, cal, adc.p) |> Float.round(2),
      rh: C.compensate_humidity(t_fine, cal, adc.h) |> Float.round(2)
    }
    {:reply, value, state}
  end

end

defmodule BME280.Driver do
  @moduledoc """
  BME280 sensor module driver
  """

  use Bitwise

  # Register Address, related read size and more
  @bme280_chip_id               %{addr: 0xD0, len: 1, genuine: 0x60}
  @bme280_reset                 %{addr: 0xE0, len: 1, cmd: 0xB6}
  @bme280_temp_press_calib_data %{addr: 0x88, len: 26}
  @bme280_humidity_calib_data   %{addr: 0xE1, len: 7}
  @bme280_ctrl_hum_addr         0xF2
  @bme280_ctrl_meas_addr        0xF4
  @bme280_config_addr           0xF5
  @bme280_data                  %{addr: 0xF7, len: 8}

  @spec get_sensor_data(pid()) :: %{t: integer, p: integer, h: integer} | {:error, term}
  def get_sensor_data(i2c) do
    <<pres_msb  :: unsigned-8,
      pres_lsb  :: unsigned-8,
      pres_xlsb :: unsigned-8,
      temp_msb  :: unsigned-8,
      temp_lsb  :: unsigned-8,
      temp_xlsb :: unsigned-8,
      hum_msb   :: unsigned-8,
      hum_lsb   :: unsigned-8
    >> = ElixirALE.I2C.write_read(i2c, <<@bme280_data.addr>>, @bme280_data.len)
    %{
      t: (temp_msb <<< 12) ||| (temp_lsb <<< 4) ||| (temp_xlsb >>> 2),
      p: (pres_msb <<< 12) ||| (pres_lsb <<< 4) ||| (pres_xlsb >>> 2),
      h: (hum_msb <<< 8) ||| hum_lsb
    }
  end

  @spec get_calib_data(pid()) :: BME280.CalibData.t() | {:error, term}
  def get_calib_data(i2c) do
    <<t1 :: unsigned-little-16,
      t2 :: signed-little-16,
      t3 :: signed-little-16,
      p1 :: unsigned-little-16,
      p2 :: signed-little-16,
      p3 :: signed-little-16,
      p4 :: signed-little-16,
      p5 :: signed-little-16,
      p6 :: signed-little-16,
      p7 :: signed-little-16,
      p8 :: signed-little-16,
      p9 :: signed-little-16,
      __ :: 8,
      h1 :: unsigned-8
    >> = ElixirALE.I2C.write_read(i2c, <<@bme280_temp_press_calib_data.addr>>, @bme280_temp_press_calib_data.len)
    <<h2        :: signed-little-16,
      h3        :: unsigned-8,
      h4_upper  :: signed-8,
      h5_lower  :: size(4),
      h4_lower  :: size(4),
      h5_upper  :: signed-8,
      h6        :: signed-8
    >> = ElixirALE.I2C.write_read(i2c, <<@bme280_humidity_calib_data.addr>>, @bme280_humidity_calib_data.len)
    h4 = h4_upper <<< 4 ||| h4_lower
    h5 = h5_upper <<< 4 ||| h5_lower
    #
    %BME280.CalibData{
      dig_T1: t1, dig_T2: t2, dig_T3: t3,
      dig_P1: p1, dig_P2: p2, dig_P3: p3, dig_P4: p4, dig_P5: p5, dig_P6: p6, dig_P7: p7, dig_P8: p8, dig_P9: p9,
      dig_H1: h1, dig_H2: h2, dig_H3: h3, dig_H4: h4, dig_H5: h5, dig_H6: h6
    }
  end

  @spec setup_sensor(pid()) :: :ok | {:error, term}
  def setup_sensor(i2c) do
    osrs_t    = 0b001 # Temperature oversampling * 1
    osrs_p    = 0b001 # Pressure oversampling * 1
    osrs_h    = 0b001 # Humidity oversampling * 1
    mode      = 0b11  # Normal mode
    t_sb      = 0b101 # T standby 1000ms
    filter    = 0b001 # Filter coefficient 2
    spi3w_en  = 0     # 3-wire SPI Disable
    #
    ctrl_meas_reg = (osrs_t <<< 5) ||| (osrs_p <<< 2) ||| mode
    config_reg    = (t_sb <<< 5) ||| (filter <<< 2) ||| spi3w_en
    ctrl_hum_reg  = osrs_h
    #
    :ok = ElixirALE.I2C.write(i2c, <<@bme280_ctrl_hum_addr, ctrl_hum_reg>>)
    :ok = ElixirALE.I2C.write(i2c, <<@bme280_ctrl_meas_addr, ctrl_meas_reg>>)
    :ok = ElixirALE.I2C.write(i2c, <<@bme280_config_addr, config_reg>>)
    :ok
  end

  @spec init_sensor(pid(), pos_integer) :: :ok | {:error, charlist()}
  def init_sensor(i2c, try_count \\ 5) do
    f = fn _ ->
          with  :ok <- check_chip_id(i2c),
                :ok <- soft_reset(i2c)
          do
            :ok
          end
        end
    retry_on_error(try_count, [], f)
  end

  @spec retry_on_error(non_neg_integer, [{:error, charlist()}], fun()) :: :ok | {:error, charlist()}
  defp retry_on_error(0, acc, _fun) do
    Enum.reverse(acc)
  end

  defp retry_on_error(remaings, acc, fun) do
    case fun.(remaings) do
      :ok ->
        :ok

      error ->
        retry_on_error(remaings-1, [error | acc], fun)
    end
  end

  @spec check_chip_id(pid()) :: :ok | {:error, charlist()}
  defp check_chip_id(i2c) do
    <<cid :: 8>> = ElixirALE.I2C.write_read(i2c, <<@bme280_chip_id.addr>>, @bme280_chip_id.len)
    if cid == @bme280_chip_id.genuine do
      :ok
    else
      a = "0x#{Integer.to_string(@bme280_chip_id.genuine, 16)}"
      b = "0x#{Integer.to_string(cid, 16)}"
      {:error, "Correct chip ID is #{a}, but it is #{b}"}
    end
  end

  @spec soft_reset(pid()) :: :ok | {:error, charlist()}
  defp soft_reset(i2c) do
    # Write the soft reset command in the sensor
    with :ok <- ElixirALE.I2C.write(i2c, <<@bme280_reset.addr, @bme280_reset.cmd>>)
    do
      # As per data sheet, startup time is 2 ms.
      Process.sleep(2)
      :ok
    end
  end

end

defmodule BME280.Calibration do
  @moduledoc """
  Compensation of BME280 sensor module readings
  """

  @spec temperature_fine(integer(), BME280.CalibData.t()) :: float
  def temperature_fine(adc_t, cal) do
    var1 = (adc_t / 16384.0 - cal.dig_T1 / 1024.0)
    var1 = var1 * cal.dig_T2
    var2 = adc_t / 131072.0 - cal.dig_T1 / 8192.0
    var2 = (var2 * var2) * cal.dig_T3
    var1 + var2
  end

  @spec compensate_temperature(float(), BME280.CalibData.t(), integer) :: float
  def compensate_temperature(t_fine, _cal, _adc_t) do
    temperature_min = -40.0
    temperature_max = 85.0

    temprerature = t_fine / 5120.0
    temprerature
    |> min(temperature_max)
    |> max(temperature_min)
  end

  @spec compensate_pressure(float(), BME280.CalibData.t(), integer) :: float
  def compensate_pressure(t_fine, cal, adc_p) do
    pressure_min = 30000.0
    pressure_max = 110000.0

    var1 = (t_fine / 2.0) - 64000.0
    var2 = var1 * var1 * cal.dig_P6 / 32768.0
    var2 = var2 + var1 * cal.dig_P5 * 2.0
    var2 = (var2 / 4.0) + cal.dig_P4 * 65536.0
    var3 = cal.dig_P3 * var1 * var1 / 524288.0
    var1 = (var3 + cal.dig_P2 * var1) / 524288.0
    var1 = (1.0 + var1 / 32768.0) * cal.dig_P1
    # avoid exception caused by division by zero
    pressure =  if var1 != 0.0 do
                  pressure = 1048576.0 - adc_p
                  pressure = (pressure - (var2 / 4096.0)) * 6250.0 / var1
                  var1 = cal.dig_P9 * pressure * pressure / 2147483648.0
                  var2 = pressure * cal.dig_P8 / 32768.0
                  pressure = pressure + (var1 + var2 + cal.dig_P7) / 16.0
                  pressure
                  |> min(pressure_max)
                  |> max(pressure_min)
                else
                  pressure_min
                end
    # convert pascal to hectopascal
    pressure / 100.0
  end

  @spec compensate_humidity(float(), BME280.CalibData.t(), integer) :: float
  def compensate_humidity(t_fine, cal, adc_h) do
    humidity_min = 0.0
    humidity_max = 100.0

    var1 = t_fine - 76800.0
    var2 = cal.dig_H4 * 64.0 + ((cal.dig_H5 / 16384.0) * var1)
    var3 = adc_h - var2
    var4 = cal.dig_H2 / 65536.0
    var5 = (1.0 + (cal.dig_H3 / 67108864.0) * var1)
    var6 = 1.0 + (cal.dig_H6 / 67108864.0) * var1 * var5
    var6 = var3 * var4 * (var5 * var6)
    humidity = var6 * (1.0 - cal.dig_H1 * var6 / 524288.0)
    humidity
    |> min(humidity_max)
    |> max(humidity_min)
  end

end


