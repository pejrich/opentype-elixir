defmodule OpenType.GlyphInfo do
  defstruct glyph: nil, cdelta: 0, mdelta: 0, 
    codepoints: [], components: [],
    isMark: false, isLigature: false,
    mLigComponent: 0, tag: nil
end
