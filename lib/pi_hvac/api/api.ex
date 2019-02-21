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

# module: REST API backend

defmodule PiHvac.Api do
  @moduledoc """
  The Api context.
  """

  import Ecto.Query, warn: false
  alias PiHvac.Repo

  alias PiHvac.Api.EnvMeasured

  @doc """
  Returns the list of envmeasured.

  ## Examples

      iex> list_envmeasured()
      [%EnvMeasured{}, ...]

  """
  def list_envmeasured(params) do
    default = %{"limits" => 3}
    %{"limits" => lmt} = Map.merge(default, params)
    EnvMeasured
    |> order_by([e], desc: e.id)
    |> limit(^lmt)
    |> Repo.all
    |> Enum.reverse
  end

  @doc """
  Gets a single env_measured.

  Raises `Ecto.NoResultsError` if the Env measured does not exist.

  ## Examples

      iex> get_env_measured!(123)
      %EnvMeasured{}

      iex> get_env_measured!(456)
      ** (Ecto.NoResultsError)

  """
  def get_env_measured!(id), do: Repo.get!(EnvMeasured, id)

  @doc """
  Creates a env_measured.

  ## Examples

      iex> create_env_measured(%{field: value})
      {:ok, %EnvMeasured{}}

      iex> create_env_measured(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_env_measured(attrs \\ %{}) do
    %EnvMeasured{}
    |> EnvMeasured.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a env_measured.

  ## Examples

      iex> update_env_measured(env_measured, %{field: new_value})
      {:ok, %EnvMeasured{}}

      iex> update_env_measured(env_measured, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_env_measured(%EnvMeasured{} = env_measured, attrs) do
    env_measured
    |> EnvMeasured.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a EnvMeasured.

  ## Examples

      iex> delete_env_measured(env_measured)
      {:ok, %EnvMeasured{}}

      iex> delete_env_measured(env_measured)
      {:error, %Ecto.Changeset{}}

  """
  def delete_env_measured(%EnvMeasured{} = env_measured) do
    Repo.delete(env_measured)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking env_measured changes.

  ## Examples

      iex> change_env_measured(env_measured)
      %Ecto.Changeset{source: %EnvMeasured{}}

  """
  def change_env_measured(%EnvMeasured{} = env_measured) do
    EnvMeasured.changeset(env_measured, %{})
  end
end
