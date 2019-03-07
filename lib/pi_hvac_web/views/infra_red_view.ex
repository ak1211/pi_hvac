defmodule PiHvacWeb.V1.InfraRedView do
  use PiHvacWeb, :view
  alias PiHvacWeb.V1.InfraRedView

  def render("show.json", param) do
    %{data: render_one(param, InfraRedView, "ir_code.json")}
  end

  def render("ir_code.json", %{infra_red: ir}) do
    %{button_number: ir.button_number,
      code: ir.ir_code}
  end
end
