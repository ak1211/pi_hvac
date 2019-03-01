defmodule PiHvacWeb.V1.InfraRedController do
  use PiHvacWeb, :controller
  require Logger

  action_fallback PiHvacWeb.FallbackController

  def create(conn, %{"data" => data}) do
    case ADRSIR.trans_command(data) do
      :ok ->
        send_resp(conn, 204, "")
      {:error, reason} ->
        send_resp(conn, 400, reason)
    end
  end

  def show(conn, %{"id" => button_num}) do
    num = String.to_integer(button_num)
    if Enum.member?(1..10, num) do
      # 基板上のボタン番号は1から10まで、メモリ番号は0から9まで
      code = ADRSIR.read_command(num - 1)
      render(conn, "show.json", button_number: num, ir_code: code)
    else
      send_resp(conn, 404, "")
    end
  end

  def update(conn, %{"id" => button_num, "data" => data}) do
    num = String.to_integer(button_num)
    Logger.debug("\"#{data}\", set to #{num}")
    if Enum.member?(1..10, num) do
      # 基板上のボタン番号は1から10まで、メモリ番号は0から9まで
      case ADRSIR.write_command(num - 1, data) do
        :ok ->
          send_resp(conn, 204, "")
        {:error, reason} ->
          send_resp(conn, 400, reason)
      end
    else
      send_resp(conn, 404, "")
    end
  end

end
