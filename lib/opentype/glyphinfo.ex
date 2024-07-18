defmodule OpenType.GlyphInfo do
  defstruct glyph: 0,
            cursive_delta: 0,
            mark_delta: 0,
            codepoints: [],
            components: [],
            is_mark: false,
            is_ligature: false,
            m_lig_component: 0,
            tag: nil
end
