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

# module: Bosch BME280 測定値データベースのスキーマ定義

defmodule PiHvac.Api.EnvMeasured do
  use Ecto.Schema
  import Ecto.Changeset


  schema "envmeasured" do
    field :degc, :decimal
    field :hpa, :decimal
    field :measured_at, :utc_datetime
    field :rh, :decimal
    field :sensor_id, :string

#    timestamps()
  end

  @doc false
  def changeset(env_measured, attrs) do
    env_measured
    |> cast(attrs, [:measured_at, :degc, :hpa, :rh, :sensor_id])
    |> validate_required([:measured_at, :degc, :hpa, :rh, :sensor_id])
  end
end
