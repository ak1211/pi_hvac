defmodule PiHvacWeb.V1.InfraRedController do
  use PiHvacWeb, :controller
  require Logger

  action_fallback PiHvacWeb.FallbackController

  def create(conn, %{"data" => data}) do
    with  %{"code" => ir_code, "button_number" => bn} <- data,
          true <- Enum.member?(1..10, bn)
    do
      Logger.debug("\"#{ir_code}\", set to #{bn}")
      # 基板上のボタン番号は1から10まで、メモリ番号は0から9まで
      case ADRSIR.write_command(bn - 1, ir_code) do
        :ok -> send_resp(conn, 204, "")
        {:error, reason} -> send_resp(conn, 400, reason)
      end
    end
  end

  def show(conn, %{"id" => button_num}) do
    num = String.to_integer(button_num)
    unless Enum.member?(1..10, num) do
      send_resp(conn, 404, "")
    else
      # 基板上のボタン番号は1から10まで、メモリ番号は0から9まで
      code = ADRSIR.read_command(num - 1)
      render(conn, "show.json", button_number: num, ir_code: code)
    end
  end

end
