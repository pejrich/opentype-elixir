defmodule OpenTypeTest do
  use ExUnit.Case
  doctest OpenType
  alias OpenType
  alias OpenType.Layout

  test "parse Truetype font metrics" do
    ttf = OpenType.parse_file("./test/support/fonts/NotoSansCJKjp-Bold.otf")
    assert 733 == ttf.capHeight
    assert -120 == ttf.descent
    assert 880 == ttf.ascent
    assert [-1013.0, -1046.0, 2926.0, 1806.0] == ttf.bbox
    assert 0 == ttf.italicAngle
    assert 1000 == ttf.unitsPerEm
    assert 1000 == ttf.defaultWidth
    assert 700 == ttf.usWeightClass
  end

  test "Basic CMAP support" do
    ttf = OpenType.parse_file("./test/support/fonts/NotoSans-Bold.ttf")
    {glyphs, _} = OpenType.layout_text(ttf, "ABC")
    assert glyphs == [36, 37, 38]
  end

  test "Apply OpenType substitutions (GSUB 4) - exercise ligature" do
    ttf = OpenType.parse_file("./test/support/fonts/NotoSans-Bold.ttf")
    {glyphs, _} = OpenType.layout_text(ttf, "ffl", ["liga"])
    assert glyphs == [603]
  end

  test "Apply OpenType substitutions (GSUB 6) - exercise chained" do
    ttf = OpenType.parse_file("./test/support/fonts/SourceSansPro-Regular.otf")
    {glyphs, _} = OpenType.layout_text(ttf, "1/2", ["liga", "frac"])
    assert glyphs == [1617, 1726, 1604]
  end

  test "Apply mark-to-base positioning" do
    ttf = OpenType.parse_file("./test/support/fonts/NotoSans-Bold.ttf")
    {glyphs, pos} = OpenType.layout_text(ttf, "bi\u0300g", ["ccmp", "mark"])
    # ccmp will replace i with dotless variant
    assert glyphs == [69, 243, 608, 74]

    # mark will position accent over the i
    xpos = pos |> Enum.map(fn {_, x, _, _, _} -> x end)
    xadv = pos |> Enum.map(fn {_, _, _, x, _} -> x end)
    # mark has an xOffset
    assert xpos == [0, 0, 317, 0]
    # mark has zero width
    assert xadv == [1296, 625, 0, 1296]
  end

  test "Exercise GSUB 2 - one-to-many substitution" do
    ttf = OpenType.parse_file("./test/support/fonts/NotoNastaliqUrdu-Regular.ttf")
    # include a following character to ensure returned glyphs are flattened properly
    # Qaf, Alef
    {glyphs, _} = OpenType.layout_text(ttf, "\u0642\u0627", ["ccmp"])
    # decompose QAF into QAFX and TWO_DOTS_ABOVE
    assert glyphs == [858, 16, 889]
  end

  test "Test positional substitutions" do
    ttf = OpenType.parse_file("./test/support/fonts/NotoNastaliqUrdu-Regular.ttf")

    # no substitutions
    {glyphs, _} = OpenType.layout_text(ttf, "\u0644", [])
    assert glyphs == [881]

    # enable isolation
    {glyphs, _} = OpenType.layout_text(ttf, "\u0644", ["isol"])
    assert glyphs == [248]

    # standard shaping
    # init, media, fina, isol
    {glyphs, _} = OpenType.layout_text(ttf, "\u0642\u0644\u0627\u06A9")
    assert glyphs == [247, 279, 268, 16, 304]

    {glyphs, _} = OpenType.layout_text(ttf, "\u0644\u0627\u06A9\u0642")
    # init, fina, init, fina
    assert glyphs == [16, 391, 422, 279, 267]

    {glyphs, _} = OpenType.layout_text(ttf, "\u0627\u06A9\u0642\u0644")
    # isol, init, medi, fina
    assert glyphs == [16, 460, 249, 717, 227]
  end

  test "Urdu cursive" do
    ttf = OpenType.parse_file("./test/support/fonts/NotoNastaliqUrdu-Regular.ttf")

    s = Layout.detect_script("\u062D\u062D\u062D\u062D\u062D\u062D\u0628")
    assert s == "arab"

    {glyphs, _pos} = OpenType.layout_text(ttf, "\u062D\u062D\u062D\u062D\u062D\u062D\u0628")
    assert glyphs == [230, 591, 591, 591, 591, 591, 18, 523]
  end

  test "Exercise GSUB 8 (reverse contextual chaining)" do
    ttf = OpenType.parse_file("./test/support/fonts/UrdType.ttf")

    {glyphs, _pos} = OpenType.layout_text(ttf, "\u0630\u0698 \u0621\u0647")
    assert glyphs == [304, 248, 7, 277, 269]
  end

  test "Exercise GPOS 5 (mark to ligature)" do
    ttf = OpenType.parse_file("./test/support/fonts/UrdType.ttf")

    # step 1: generate the ligature we'll position
    {glyphs, pos} = OpenType.layout_text(ttf, "\u0630\u0644\u0622\u0650")
    assert glyphs == [830, 317, 268]
    # without ligature positioning: hd(pos) == {:pos, 320, -378, 0, 0}
    assert hd(pos) == {:pos, 300, -300, 0, 0}
  end

  test "mark to ligature with embedded mark" do
    # ttf = OpenType.parse_file("./test/support/fonts/UrdType.ttf")

    # step 1: generate the ligature we'll position
    # {glyphs, pos} = OpenType.layout_text(ttf, "\u0630\u0644\u0650\u0622")
    # assert glyphs ==  [830,317,268]
    # without ligature positioning: hd(pos) == {:pos, 320, -378, 0, 0}
    # assert hd(pos) == {:pos, 300, -300, 0, 0}
  end

  # TODO: can't distribute Verdana so find OFL-licensed TTF file with same issue
  # test "Verdana bug" do
  #  ttf = OpenType.parse_file("./test/support/fonts/Verdana.ttf")
  #  {g, p}  = OpenType.layout_text(ttf, "Hello")
  #  assert g == [43, 72, 79, 79, 82]
  #  assert p == [{:std_width, 0, 0, 1539, 0}, {:std_width, 0, 0, 1220, 0}, {:std_width, 0, 0, 562, 0}, {:std_width, 0, 0, 562, 0}, {:std_width, 0, 0, 1243, 0}]
  # end
end
