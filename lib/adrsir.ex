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

# module: ADRSIR デバイスドライバ

defmodule ADRSIR do
  @moduledoc """
  ADRSIR server
  """

  @behaviour GenServer
  alias ADRSIR.Driver

  # Wire connections definition
  # The ADRSIR device is connected to I2C bus 1, and I2C address is 0x52
  @i2c_bus      "i2c-1"
  @i2c_address  0x52

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
    defstruct i2c: nil
    @type t :: %State{i2c: pid()}
  end

  #
  # Public API
  #

  def start_link() do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def stop() do
    GenServer.stop(__MODULE__)
  end

  def read_command(num) do
    to_hex_string = fn x ->
      Integer.to_string(x, 16) |> String.pad_leading(2, "0")
    end
    read_ir(num)
    |> Enum.map_join("", to_hex_string)
  end

  def write_command(num, hex_string) do
    fun = fn v -> write_ir(num, v) end
    do_command(hex_string, fun)
  end

  def trans_command(hex_string) do
    fun = fn v -> transmit_ir(v) end
    do_command(hex_string, fun)
  end

  defp do_command(hex_string, fun) do
    chars = String.graphemes(hex_string)
    if Integer.mod(length(chars), 2) == 0 do
      v = String.graphemes(hex_string)
          |> Stream.chunk_every(2)
          |> Stream.map(fn [a, b] -> String.to_integer(a <> b, 16) end)
          |> Enum.to_list
      fun.(v)
    else
      {:error, "Illegal data length."}
    end
  end

  def read_ir(num) do
    GenServer.call(__MODULE__, {:read_ir, num})
  end

  def write_ir(num, irdata) do
    GenServer.call(__MODULE__, {:write_ir, num, irdata})
  end

  def transmit_ir(irdata) do
    GenServer.call(__MODULE__, {:transmit_ir, irdata})
  end

  #
  # GenServer callbacks
  #

  @impl GenServer
  def init(_arg) do
    {:ok, i2c}  = ElixirALE.I2C.start_link(@i2c_bus, @i2c_address)
    state       = %State{i2c: i2c}
    {:ok, state}
  end

  @impl GenServer
  def handle_call({:read_ir, memnum}, _from, state) do
    value = Driver.read_irdata(state.i2c, memnum)
    {:reply, value, state}
  end

  @impl GenServer
  def handle_call({:write_ir, memnum, irdata}, _from, state) do
    rep = Driver.write_irdata(state.i2c, memnum, irdata)
    {:reply, rep, state}
  end

  @impl GenServer
  def handle_call({:transmit_ir, irdata}, _from, state) do
    rep = Driver.transmit_irdata(state.i2c, irdata)
    {:reply, rep, state}
  end

end


defmodule ADRSIR.Driver do
  @moduledoc """
  ADRSIR device driver on the Raspberry Pi.
  """

  # some data type
  @type memory_number :: 0..9
  @type infrared_data :: [integer()]

  # Register Address
  @adrsir_R1_write_ward   %{addr: 0x15}
  @adrsir_R2_read_len     %{addr: 0x25}
  @adrsir_R3_read_data    %{addr: 0x35}
  #
  @adrsir_W1_write_ward   %{addr: 0x19}
  @adrsir_W2_write_len    %{addr: 0x29}
  @adrsir_W3_write_data   %{addr: 0x39}
  @adrsir_W4_write_nvmem  %{addr: 0x49}
  #
  @adrsir_T1_transmit_IR  %{addr: 0x59}

  @spec read_irdata(pid(), memory_number) :: infrared_data | {:error, term()}
  def read_irdata(i2c, memnum) do
    #
    set_read_memory_number = fn ->
      ElixirALE.I2C.write(i2c, <<@adrsir_R1_write_ward.addr, memnum>>)
    end
    #
    get_data_length = fn ->
      <<_ :: unsigned-8, len :: unsigned-big-16>>
      = ElixirALE.I2C.write_read(i2c, <<@adrsir_R2_read_len.addr>>, 3)
      octet = len * 4
        {:ok, octet}
    end
    #
    get_data = fn octet ->
      Stream.resource(
        fn -> ElixirALE.I2C.write_read(i2c, <<@adrsir_R3_read_data.addr>>, 1) end,
        fn dummy ->
          double_ward = ElixirALE.I2C.write_read(i2c, <<@adrsir_R3_read_data.addr>>, 4)
          <<d0 :: 8, d1 :: 8, d2 :: 8, d3 :: 8>> = double_ward
          {[d0, d1, d2, d3], dummy}
        end,
        fn _dummy -> :ok end
      )
      |> Stream.take(octet)
      |> Enum.to_list
    end
    #
    with  :ok           <- set_read_memory_number.(),
          {:ok, octet}  <- get_data_length.()
    do
      get_data.(octet)
    end
  end

  @spec write_irdata(pid(), memory_number, infrared_data) :: :ok | {:error, term()}
  def write_irdata(i2c, memnum, remocon_data) do
    len = length(remocon_data)
    cond do
      len == 0 ->
        {:error, "Empty data"}

      Integer.mod(len, 4) != 0 ->
        {:error, "Bad data, Data must be 32-bit wards."}

      true ->
        with  :ok <- set_write_memory_number(i2c, memnum),
              :ok <- set_data_length(i2c, len),
              :ok <- set_irdata(i2c, remocon_data)
        do
          ElixirALE.I2C.write(i2c, <<@adrsir_W4_write_nvmem.addr, memnum>>)
        else
          error -> error
        end
    end
  end

  @spec transmit_irdata(pid(), infrared_data) :: :ok | {:error, term()}
  def transmit_irdata(i2c, remocon_data) do
    len = length(remocon_data)
    cond do
      len == 0 ->
        {:error, "Empty data"}

      Integer.mod(len, 4) != 0 ->
        {:error, "Bad data, Data must be 32-bit wards."}

      true ->
        with  :ok <- set_data_length(i2c, len),
              :ok <- set_irdata(i2c, remocon_data)
        do
          dummy = 0x00
          ElixirALE.I2C.write(i2c, <<@adrsir_T1_transmit_IR.addr, dummy>>)
        else
          error -> error
        end
    end
  end

  @spec set_write_memory_number(pid(), memory_number) :: :ok | {:error, term()}
  defp set_write_memory_number(i2c, memnum) do
    ElixirALE.I2C.write(i2c, <<@adrsir_W1_write_ward.addr, memnum>>)
  end

  @spec set_data_length(pid(), pos_integer) :: :ok | {:error, term()}
  defp set_data_length(i2c, length) do
    binary = <<@adrsir_W2_write_len.addr, div(length, 4) :: unsigned-big-16>>
    ElixirALE.I2C.write(i2c, binary)
  end

  @spec set_irdata(pid(), infrared_data) :: :ok | {:error, term()}
  defp set_irdata(i2c, irdata) do
    Stream.chunk_every(irdata, 4)
    |> Stream.map(fn [d0, d1, d2, d3] ->
      <<@adrsir_W3_write_data.addr, d0 :: 8, d1 :: 8, d2 :: 8, d3 :: 8>>
      end)
    |> Stream.map(fn val -> ElixirALE.I2C.write(i2c, val) end)
    |> Enum.reduce(
      :ok,
      fn result, _acc ->
        case result do
          :ok -> :ok
          error -> error
        end
      end)
  end

 end

