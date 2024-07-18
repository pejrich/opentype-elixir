defmodule OpenType.Font do
  @moduledoc """
  Provides parsers and utilities
  """

  use GenServer
  alias OpenType
  alias UnicodeData

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def init(:ok) do
    {:ok, OpenType.new()}
  end

  # read in and parse a TTF or OTF file
  def parse(pid, filename) do
    GenServer.cast(pid, {:parse, filename})
    pid
  end

  # layout a run of text
  # send back the glyphs and positioning data
  def layout(pid, text, features \\ nil) do
    GenServer.call(pid, {:layout, text, features})
  end

  # get the font structure (for additional analysis)
  def font_structure(pid) do
    GenServer.call(pid, :ttf)
  end

  def scale_factor(pid) do
    GenServer.call(pid, :scale)
  end

  def handle_cast({:parse, filename}, _ttf) do
    parsed = OpenType.parse_file(filename)
    {:noreply, parsed}
  end

  def handle_call(:ttf, _from, ttf) do
    {:reply, ttf, ttf}
  end

  def handle_call(:scale, _from, ttf) do
    {:reply, 1000.0 / ttf.units_per_em, ttf}
  end

  def handle_call({:layout, text, features}, _from, ttf) do
    output = OpenType.layout_text(ttf, text, features)
    {:reply, output, ttf}
  end
end
