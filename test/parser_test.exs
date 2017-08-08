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

  test "parse features (tag-to-lookup index map)" do
    # two features with multiple indices each
    #  'liga': 1, 2
    #  'dlig': 3, 4, 5
    actual = Parser.parseFeatures(<<2::16, "liga", 14::16, "dlig", 22::16, #header
                                    0::16, 2::16, 1::16, 2::16, #liga
                                    0::16, 3::16, 3::16, 4::16, 5::16 #dlig
                                    >>)
    assert actual == [{"liga", [1, 2]}, {"dlig", [3, 4, 5]}]
  end


  test "parse table definition" do
    actual = Parser.readTables(<<"kern", 1::32, 2::32, 3::32>>, 1)
    assert actual == [%Parser.FontTable{name: "kern", checksum: 1, offset: 2, length: 3}]
  end

end
