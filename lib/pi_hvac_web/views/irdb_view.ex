defmodule PiHvacWeb.V1.IRDBView do
  use PiHvacWeb, :view
  alias PiHvacWeb.V1.IRDBView

  def render("index.json", %{irdb: irdb}) do
    %{data: render_many(irdb, IRDBView, "irdb.json")}
  end

  def render("show.json", %{irdb: irdb}) do
    %{data: render_one(irdb, IRDBView, "irdb.json")}
  end

  def render("manufacturers.json", %{irdb: irdb}) do
    %{manufacturers: Enum.map(irdb, fn x -> x.manufacturer end)}
  end

  def render("irdb.json", %{irdb: irdb}) do
    %{id: irdb.id,
      manuf: irdb.manufacturer,
      prod: irdb.product,
      key: irdb.key,
      code: irdb.code}
  end

end
