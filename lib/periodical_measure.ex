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

# module: 定期的に測定を行なうサーバー

defmodule PeriodicalMeasure do
  require Logger
  @behaviour GenServer
  alias PiHvac.Repo
  alias PiHvac.Api.EnvMeasured

  @doc "Periodical measure in Humidity, pressure, temperature and store to database."

  # Wire connections definition
  # The BME280 device is connected to I2C bus 1, and I2C address is 0x76
  @i2c_bus      "i2c-1"
  @i2c_address  0x76
  @sensor_id    "BME280 0x#{Integer.to_string(@i2c_address, 16)}"

  def child_spec(_args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, []},
      restart: :permanent,
      shutdown: 500,
      type: :worker
    }
  end

  defmodule State do
    defstruct timeslot_ms: nil, sensor: nil
    @type t :: %State{timeslot_ms: integer(), sensor: pid()}
  end

  def start_link(arg \\ %{timeslot_sec: 900}) do
    GenServer.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl GenServer
  def init(%{timeslot_sec: tsec}) do
    Logger.info("#{__MODULE__} is up on. Every #{tsec} seconds measurement.")
    {:ok, i2c}    = ElixirALE.I2C.start_link(@i2c_bus, @i2c_address)
    {:ok, sensor} = BME280.start_link(i2c)
    state         = %State{timeslot_ms: tsec*1000, sensor: sensor}
    Timex.now
    |> to_milliseconds
    |> reserve_next_schedule(state)
    {:ok, state}
  end

  @impl GenServer
  def handle_info(:work, state) do
    utc_now = Timex.now
    utc_now
    |> to_milliseconds
    |> reserve_next_schedule(state)
    record = BME280.read(state.sensor)
    %EnvMeasured{
      measured_at: DateTime.truncate(utc_now, :second),
      degc: record.degc,
      hpa: record.hpa,
      rh: record.rh,
      sensor_id: @sensor_id
    } |> Repo.insert
    {:noreply, state}
  end

  defp to_milliseconds(utc) do
    hour  = utc.hour
    min   = utc.minute
    sec   = utc.second
    usec  = elem(utc.microsecond, 0)
    msec  = div(usec, 1000)
    (((hour * 60) + min) * 60 + sec) * 1000 + msec
  end

  defp reserve_next_schedule(milliseconds, state) do
    timeslot      = state.timeslot_ms
    remainder     = rem(milliseconds, timeslot)
    next_schedule = milliseconds + timeslot - remainder
    delta_msec    = next_schedule - milliseconds
    Process.send_after(self(), :work, delta_msec)
  end

end

