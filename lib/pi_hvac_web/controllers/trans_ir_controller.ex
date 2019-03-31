defmodule PiHvacWeb.V1.TransIRController do
  use PiHvacWeb, :controller
  require Logger

  action_fallback PiHvacWeb.FallbackController

  def transmit(conn, %{"code" => ir_code}) do
    case ADRSIR.trans_command(ir_code) do
      :ok -> send_resp(conn, 204, "")
      {:error, reason} -> send_resp(conn, 400, reason)
    end
  end

end
