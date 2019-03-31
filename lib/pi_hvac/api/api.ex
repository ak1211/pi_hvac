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
  alias PiHvac.Api.IRDB

  defmodule ListIrdbOptions do
    defstruct [:limits, :offset, :manufacturer, :product]
  end

  @doc """
  Returns the list of envmeasured.

  ## Examples

      iex> list_envmeasured()
      [%EnvMeasured{}, ...]

  """
  def list_env_measured(params) do
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


  @doc """
  Returns the list of irdb.

  ## Examples

      iex> list_irdb()
      [%IRDB{}, ...]

  """
  def list_irdb(%{:limits       => limits,
                  :offset       => offset,
                  :manufacturer => manuf,
                  :product      => prod}) do
    IRDB
    |> case do
      v when not is_nil(manuf) -> where(v, manufacturer: ^manuf)
      v -> v
    end
    |> case do
      v when not is_nil(prod) -> where(v, product: ^prod)
      v -> v
    end
    |> case do
      v when not is_nil(limits) -> limit(v, ^limits)
      v -> v
    end
    |> case do
      v when not is_nil(offset) -> offset(v, ^offset)
      v -> v
    end
    |> Repo.all
  end

  @doc """
  Returns the counts of irdb.
  """
  def counts_irdb(%{:manufacturer => manuf,
                    :product => prod}) do
    IRDB
    |> case do
      v when not is_nil(manuf) -> where(v, manufacturer: ^manuf)
      v -> v
    end
    |> case do
      v when not is_nil(prod) -> where(v, product: ^prod)
      v -> v
    end
    |> select(count())
    |> Repo.one
  end

  @doc """
  Gets a single irdb.

  Raises `Ecto.NoResultsError` if the irdb does not exist.

  ## Examples

      iex> get_irdb(123)
      %IRDB{}

      iex> get_irdb(456)
      ** (Ecto.NoResultsError)

  """
  def get_irdb(id), do: Repo.get!(IRDB, id)

  @doc """
  Creates a irdb

  ## Examples

      iex> create_irdb(%{field: value})
      {:ok, %IRDB{}}

      iex> create_irdb(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_irdb(attrs \\ %{}) do
    %IRDB{}
    |> IRDB.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a irdb.

  ## Examples

      iex> update_irdb(irdb, %{field: new_value})
      {:ok, %IRDB{}}

      iex> update_irdb(irdb, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_irdb(%IRDB{} = irdb, attrs) do
    irdb 
    |> IRDB.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a irdb.

  ## Examples

      iex> delete_irdb(irdb)
      {:ok, %IRDB{}}

      iex> delete_irdb(irdb)
      {:error, %Ecto.Changeset{}}

  """
  def delete_irdb(%IRDB{} = irdb) do
    Repo.delete(irdb)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking irdb changes.

  ## Examples

      iex> change_irdb(irdb)
      %Ecto.Changeset{source: %IRDB{}}

  """
  def change_irdb(%IRDB{} = irdb) do
    IRDB.changeset(irdb, %{})
  end

  @doc """
  Returns the list of irdb manufacturer.

  ## Examples

      iex> list_irdb_manufacturer()
      [%IRDB{}, ...]

  """
  def list_irdb_manufacturer() do
    IRDB
    |> select([x], %{manufacturer: x.manufacturer})
    |> distinct([x], x.manufacturer)
    |> Repo.all
  end

  @doc """
  Returns the list of irdb product.

  ## Examples

      iex> list_irdb_product()
      [%IRDB{}, ...]

  """
  def list_irdb_product() do
    IRDB
    |> select([x], %{product: x.product})
    |> distinct([x], x.product)
    |> Repo.all
  end

end

