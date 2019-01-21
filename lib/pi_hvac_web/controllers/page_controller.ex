defmodule PiHvacWeb.PageController do
  use PiHvacWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
