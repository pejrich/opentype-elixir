defmodule OpenType.ParserTest do
  use ExUnit.Case
  alias OpenType.Parser

  test "coverage v1" do
    # v1 returns a list of glyphs
    actual = Parser.parseCoverage(<<1::16, 2::16, 97::16, 98::16>>)
    assert actual == [97, 98]
  end

  test "coverage v2" do
    #v2 returns a list of {start glyph, end glyph, coverage index}
    actual = Parser.parseCoverage(<<2::16, 2::16, 97::16, 100::16, 1::16, 1000::16, 1500::16, 2::16>>)
    assert actual == [{97, 100, 1}, {1000, 1500, 2}]
  end

  test "glyph class v1" do
    # v1 returns a map of glyph to class
    actual = Parser.parseGlyphClass(<<1::16, 10::16, 2::16, 97::16, 98::16>>)
    assert actual == %{10 => 97, 11 => 98}
  end

  test "glyph class v2" do
    #v2 returns a list of {start glyph, end glyph, glyph class}
    actual = Parser.parseGlyphClass(<<2::16, 2::16, 97::16, 100::16, 1::16, 1000::16, 1500::16, 2::16>>)
    assert actual == [{97, 100, 1}, {1000, 1500, 2}]
  end

  test "anchor v1" do
    #v1 returns signed {x, y}
    actual = Parser.parseAnchor(<<1::16, -10::signed-16, 20::signed-16>>)
    assert actual == {-10, 20}
  end

end
