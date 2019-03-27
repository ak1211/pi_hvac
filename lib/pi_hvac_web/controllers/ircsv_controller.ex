defmodule PiHvacWeb.V1.IRCSVController do
  use PiHvacWeb, :controller

  require CSV
  alias PiHvac.Api

  action_fallback PiHvacWeb.FallbackController

  def fileupload(conn, %{"ircsv" => ircsv}) do
    with true <- File.stream!(ircsv.path, [:read, :utf8, :trim_bom], :line)
                  |> Stream.map(&String.trim(&1))
                  |> Enum.to_list
                  |> CSV.decode
                  |> Stream.map(&field(&1))
                  |> Stream.map(fn {:ok, v} -> Api.create_irdb(v) end)
                  |> Enum.to_list
                  |> Enum.all?(&match?({:ok,_}, &1)) do
      send_resp(conn, 205, "ok")
    else
      {:error, reason} ->
        send_resp(conn, 400, reason)
      _ ->
        send_resp(conn, 400,"")
    end
  end

  defp field(values) do
    case values do
      {:ok, [manufacturer, product, key, code]} ->
        {:ok, %{:manufacturer => manufacturer,
                :product => product,
                :key => key,
                :code => code
                }
        }
      {:err, reason} ->
        {:err, reason}
      _ ->
        {:err, "csv fields mismatched"}
    end
  end

end
