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

# module: IR code データベースのスキーマ定義

defmodule PiHvac.Api.IRDB do
  use Ecto.Schema
  import Ecto.Changeset


  schema "irdb" do
    field :manufacturer, :string
    field :product, :string
    field :key, :string
    field :code, :string
  end

  @doc false
  def changeset(irdb, attrs) do
    irdb
    |> cast(attrs, [:manufacturer, :product, :key, :code])
    |> validate_required([:manufacturer, :product, :key, :code])
  end
end
