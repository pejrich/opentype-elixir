defmodule OpenType.GlyphInfo do
  defstruct glyph: 0,
            cursiveDelta: 0,
            markDelta: 0,
            codepoints: [],
            components: [],
            isMark: false,
            isLigature: false,
            mLigComponent: 0,
            tag: nil
end
