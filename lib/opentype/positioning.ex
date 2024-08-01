defmodule OpenType.Positioning do
  alias OpenType.Parser
  require Logger

  # add two positions together, treat nils as zeroed structure
  # category std_width, kern, pos can be used to optimize PDF output
  defp add_pos(p, nil), do: p
  defp add_pos(nil, {a, b, c, d}), do: {:pos, a, b, c, d}
  defp add_pos(p, {0, 0, 0, 0}), do: p
  defp add_pos({:std_width, a, b, c, d}, {0, 0, g, 0}), do: {:kern, a, b, c + g, d}
  defp add_pos({:std_width, a, b, c, d}, {e, f, g, h}), do: {:pos, a + e, b + f, c + g, d + h}
  defp add_pos({type, a, b, c, d}, {e, f, g, h}), do: {type, a + e, b + f, c + g, d + h}

  # ==============================================
  # GPOS glyph positioning
  # Used for kerning, optical alignment, diacratics, etc
  # if a lookup type is as-yet unsupported
  # simply passes through the input
  # ==============================================

  # parse a lookup table
  def parse_lookup_table(index, lookups) do
    lookup = lookups |> Enum.at(index)

    parsed =
      case lookup do
        # extended lookup table, need to return actual lookup type
        {9, flag, offsets, table, mfs} ->
          {actual_type, output} = parse_lookup(9, offsets, table)
          {:parsed, actual_type, flag, mfs, output}

        # standard lookup table
        {n, flag, offsets, table, mfs} ->
          {:parsed, n, flag, mfs, parse_lookup(n, offsets, table)}

        # already parsed (or unparsable), ignore
        _ ->
          lookup
      end

    # replace with parsed content and return
    if parsed != lookup do
      lookups |> List.replace_at(index, parsed)
    else
      lookups
    end
  end

  defp parse_and_apply({:parsed, type, flag, mfs, data}, gdef, lookups, is_rtl, input) do
    apply_lookup({:parsed, type, flag, mfs, data}, gdef, lookups, is_rtl, input)
  end

  defp parse_and_apply({9, flag, offsets, data, mfs}, gdef, lookups, is_rtl, input) do
    {actual_type, output} = parse_lookup(9, offsets, data)
    val = {:parsed, actual_type, flag, mfs, output}
    apply_lookup(val, gdef, lookups, is_rtl, input)
  end

  defp parse_and_apply({type, flag, offsets, data, mfs}, gdef, lookups, is_rtl, input) do
    val = {:parsed, type, flag, mfs, parse_lookup(type, offsets, data)}
    apply_lookup(val, gdef, lookups, is_rtl, input)
  end

  def apply_lookup({:parsed, type, flag, mfs, data}, gdef, lookups, is_rtl, {glyphs, pos})
      when is_list(data) do
    Enum.reduce(data, {glyphs, pos}, fn subdata, input ->
      apply_lookup({:parsed, type, flag, mfs, subdata}, gdef, lookups, is_rtl, input)
    end)
  end

  def apply_lookup({:parsed, 1, _flag, _mfs, data}, _gdef, _lookups, _is_rtl, {glyphs, pos}) do
    {fmt, coverage, values} = data

    adjusted =
      case fmt do
        1 ->
          Enum.map(glyphs, fn g ->
            coverloc = find_coverage_index(coverage, g.glyph)
            if coverloc != nil, do: values, else: nil
          end)

        2 ->
          Enum.map(glyphs, fn g ->
            coverloc = find_coverage_index(coverage, g.glyph)
            if coverloc != nil, do: Enum.at(values, coverloc), else: nil
          end)
      end

    positioning = Enum.zip(pos, adjusted) |> Enum.map(fn {v1, v2} -> add_pos(v1, v2) end)
    {glyphs, positioning}
  end

  def apply_lookup({:parsed, 2, _flag, _mfs, data}, _gdef, _lookups, _is_rtl, {glyphs, pos}) do
    {fmt, values} = data

    kerning =
      case fmt do
        1 ->
          {coverage, pair_sets} = values
          apply_kerning(coverage, pair_sets, glyphs, [])

        2 ->
          # apply the kerning
          # classify both glyphs
          # get pair_set[c1][c2]
          # position
          {class1, class2, pair_sets} = values
          apply_kerning2(class1, class2, pair_sets, glyphs, [])
      end

    positioning = Enum.zip(pos, kerning) |> Enum.map(fn {v1, v2} -> add_pos(v1, v2) end)
    {glyphs, positioning}
  end

  def apply_lookup({:parsed, 3, flag, mfs, data}, gdef, _lookups, is_rtl, {glyphs, pos}) do
    {coverage, anchor_pairs} = data

    # filter the glyphs
    g = filter_glyphs(glyphs, flag, gdef, mfs) |> Enum.to_list()

    # align entry/exit points
    {p, d_glyphs} = apply_cursive(coverage, anchor_pairs, flag, mfs, gdef, is_rtl, [], g, pos)

    g2 =
      d_glyphs
      |> Enum.reduce(glyphs, fn {x, i}, acc ->
        if Enum.at(acc, i).cursive_delta == 0, do: List.replace_at(acc, i, x), else: acc
      end)

    {g2, p}
  end

  def apply_lookup({:parsed, 4, flag, mfs, data}, gdef, _lookups, _is_rtl, {glyphs, pos}) do
    {mark_coverage, base_coverage, base_array, mark_array} = data

    # filter the glyphs
    g = filter_glyphs(glyphs, flag, gdef, mfs) |> Enum.to_list()

    # align attachment points
    {adjusted, d_glyphs} =
      apply_mark_to_base(
        mark_coverage,
        base_coverage,
        base_array,
        mark_array,
        # skip flags and GDEF info
        flag,
        mfs,
        gdef,
        [],
        g,
        pos
      )

    g2 =
      d_glyphs
      |> Enum.reduce(glyphs, fn {x, i}, acc ->
        if Enum.at(acc, i).mark_delta == 0, do: List.replace_at(acc, i, x), else: acc
      end)

    # apply the adjustments to positioning
    # positioning = Enum.zip(pos, adjusted) |> Enum.map(fn {v1, v2} -> add_pos(v1,v2) end)
    {g2, adjusted}
  end

  def apply_lookup({:parsed, 5, flag, mfs, data}, gdef, _lookups, _is_rtl, {glyphs, pos}) do
    {mark_coverage, base_coverage, base_array, mark_array} = data

    g = filter_glyphs(glyphs, flag, gdef, mfs) |> Enum.to_list()

    {adjusted, d_glyphs} =
      apply_mark_to_lig(
        mark_coverage,
        base_coverage,
        base_array,
        mark_array,
        # skip flags and GDEF info
        flag,
        mfs,
        gdef,
        [],
        g,
        pos
      )

    g2 =
      d_glyphs
      |> Enum.reduce(glyphs, fn {x, i}, acc ->
        if Enum.at(acc, i).mark_delta == 0, do: List.replace_at(acc, i, x), else: acc
      end)

    # raise "GPOS 5 - mark to ligature"
    # for this to work, we need to know which ligature component to
    # attach the mark to -- needs to be set during GSUB 4 processing!
    # in the absense of such info we could work backwards through the components
    # until we find one with an attachment point for the current mark class
    # see https://bugzilla.gnome.org/show_bug.cgi?id=437633 for a torture test
    # where a 'calt' liga + subsequent 'liga' moves target component for
    # mark that is itself a ligature!
    # positioning = Enum.zip(pos, adjusted) |> Enum.map(fn {v1, v2} -> add_pos(v1,v2) end)
    {g2, adjusted}
  end

  def apply_lookup({:parsed, 6, _flag, _mfs, data}, _gdef, _lookups, _is_rtl, {glyphs, pos}) do
    # same as format 4, except "base" is another mark
    {_mark_coverage, _base_coverage, _base_array, _mark_array} = data

    # adjusted = apply_mark_to_base(mark_coverage, base_coverage, base_array, mark_array, flag, mfs, gdef, [hd(glyphs)], tl(glyphs), pos)
    # positioning = Enum.zip(pos, adjusted) |> Enum.map(fn {v1, v2} -> add_pos(v1,v2) end)
    # {glyphs, positioning, c, m}
    {glyphs, pos}
  end

  def apply_lookup({:parsed, 7, _flag, _mfs, data}, gdef, lookups, _is_rtl, {glyphs, pos}) do
    {format, val} = data

    pos =
      case format do
        1 ->
          pos

        2 ->
          {coverage, rulesets, classes} = val

          _positioning =
            apply_context_pos2(coverage, rulesets, classes, gdef, lookups, glyphs, pos, [])

          pos

        3 ->
          pos

        _ ->
          pos
      end

    {glyphs, pos}
  end

  def apply_lookup({:parsed, 8, flag, mfs, data}, gdef, lookups, _is_rtl, {glyphs, pos}) do
    {format, val} = data

    pos =
      case format do
        1 ->
          pos

        2 ->
          pos

        3 ->
          {bt_coverage, coverage, la_coverage, pos_records} = val

          g = filter_glyphs(glyphs, flag, gdef, mfs) |> Enum.to_list()

          apply_chaining_context_pos3(
            bt_coverage,
            coverage,
            la_coverage,
            pos_records,
            gdef,
            lookups,
            g,
            pos,
            []
          )

        _ ->
          pos
      end

    {glyphs, pos}
  end

  def parse_lookup(9, offsets, data) do
    # GPOS type 9 -- extended table
    subtables =
      offsets
      |> Enum.map(fn x ->
        <<1::16, lt::16, off::32>> = binary_part(data, x, 8)
        {lt, Parser.subtable(data, x + off)}
      end)

    # spec says extended type is identical for all subtables
    {actual_type, _} = hd(subtables)

    output =
      subtables
      |> Enum.map(fn {type, table} -> parse_lookup(type, table) end)

    {actual_type, output}
  end

  # parse a lookup (which is expected to have multiple subtables)
  def parse_lookup(type, offsets, data) do
    offsets
    |> Enum.map(fn o -> parse_lookup(type, Parser.subtable(data, o)) end)
  end

  def parse_lookup(1, data) do
    <<fmt::16, cov_off::16, value_format::16, rest::binary>> = data
    coverage = Parser.parse_coverage(Parser.subtable(data, cov_off))
    val_size = Parser.value_record_size(value_format)

    values =
      case fmt do
        1 ->
          <<val::binary-size(val_size), _::binary>> = rest
          Parser.read_positioning_value_record(value_format, val)

        2 ->
          <<n_vals::16, _::binary>> = rest
          recs = binary_part(rest, 16, n_vals * val_size)

          for <<(<<val::binary-size(val_size)>> <- recs)>>,
            do: Parser.read_positioning_value_record(value_format, val)
      end

    {fmt, coverage, values}
  end

  def parse_lookup(2, data) do
    <<fmt::16, cov_off::16, record1::16, record2::16, rest::binary>> = data

    kerning =
      case fmt do
        1 ->
          # FMT 1 - identifies individual glyphs
          # pair set table
          <<n_pairs::16, pair_off::binary-size(n_pairs)-unit(16), _::binary>> = rest
          pairset_offsets = for <<(<<x::16>> <- pair_off)>>, do: x
          # coverage table
          coverage = Parser.parse_coverage(Parser.subtable(data, cov_off))
          # parse the pair sets
          pair_sets =
            Enum.map(pairset_offsets, fn off ->
              Parser.parse_pair_set(data, off, record1, record2)
            end)

          {coverage, pair_sets}

        2 ->
          # FMT 2
          # offset to classdef, offset to classdef
          # n_class1_records, n_class2_records
          <<class1Off::16, class2Off::16, n_class1_records::16, n_class2_records::16,
            records::binary>> = rest

          # read in the class definitions
          class1 = Parser.parse_glyph_class(Parser.subtable(data, class1Off))
          class2 = Parser.parse_glyph_class(Parser.subtable(data, class2Off))
          # fmt==1, startglyph, nglyphs, array of ints (each int is a class)
          # fmt==2, n_ranges, {start_glyph, end_glyph, class}

          # read in the actual positioning pairs
          size_a = Parser.value_record_size(record1)
          size_b = Parser.value_record_size(record2)
          class2size = size_a + size_b
          class1size = n_class2_records * class2size
          c1recs = binary_part(records, 0, n_class1_records * class1size)
          c1Recs = for <<(<<c2recs::binary-size(class1size)>> <- c1recs)>>, do: c2recs

          pair_sets =
            Enum.map(c1Recs, fn c2recs ->
              c2Recs = for <<(<<c2Rec::binary-size(class2size)>> <- c2recs)>>, do: c2Rec

              c2Recs
              |> Enum.map(fn c2Rec ->
                for <<(<<v1::binary-size(size_a), v2::binary-size(size_b)>> <- c2Rec)>>,
                  do: {v1, v2}
              end)
              |> Enum.map(fn [{v1, v2}] ->
                {Parser.read_positioning_value_record(record1, v1),
                 Parser.read_positioning_value_record(record2, v2)}
              end)
            end)

          {class1, class2, pair_sets}
      end

    {fmt, kerning}
  end

  def parse_lookup(3, data) do
    <<_fmt::16, coverage_off::16, n_anchor_pairs::16, nrecs::binary-size(n_anchor_pairs)-unit(32),
      _::binary>> = data

    coverage = Parser.parse_coverage(Parser.subtable(data, coverage_off))

    records =
      for <<(<<entry_anchor::16, exit_anchor::16>> <- nrecs)>>, do: {entry_anchor, exit_anchor}

    anchor_pairs =
      records
      |> Enum.map(fn {entry_anchor, exit_anchor} ->
        entry_anchor =
          if entry_anchor != 0,
            do: Parser.parse_anchor(Parser.subtable(data, entry_anchor)),
            else: nil

        exit_anchor =
          if exit_anchor != 0,
            do: Parser.parse_anchor(Parser.subtable(data, exit_anchor)),
            else: nil

        {entry_anchor, exit_anchor}
      end)

    {coverage, anchor_pairs}
  end

  def parse_lookup(4, data) do
    <<_fmt::16, mark_coverage_off::16, base_coverage_off::16, n_classes::16,
      mark_array_offset::16, base_array_offset::16, _::binary>> = data

    # coverage definitions
    mark_coverage = Parser.parse_coverage(Parser.subtable(data, mark_coverage_off))
    base_coverage = Parser.parse_coverage(Parser.subtable(data, base_coverage_off))

    # base_array table
    base_tbl = Parser.subtable(data, base_array_offset)
    <<n_recs::16, records::binary>> = base_tbl
    # 2 bytes per class
    record_size = n_classes * 2
    records = binary_part(records, 0, n_recs * record_size)
    records = for <<(<<record::binary-size(record_size)>> <- records)>>, do: record
    # each record is array of offsets
    base_array =
      records
      |> Enum.map(fn r -> for <<(<<offset::16>> <- r)>>, do: offset end)
      |> Enum.map(
        &Enum.map(&1, fn o ->
          Parser.parse_anchor(Parser.subtable(base_tbl, o))
        end)
      )

    # mark_array table
    mark_array_tbl = Parser.subtable(data, mark_array_offset)
    mark_array = Parser.parse_mark_array(mark_array_tbl)

    {mark_coverage, base_coverage, base_array, mark_array}
  end

  def parse_lookup(5, data) do
    # same as format 4, except "base" is a ligature with (possibly) multiple anchors
    <<_fmt::16, mark_coverage_off::16, base_coverage_off::16, n_classes::16,
      mark_array_offset::16, base_array_offset::16, _::binary>> = data

    mark_coverage = Parser.parse_coverage(Parser.subtable(data, mark_coverage_off))
    base_coverage = Parser.parse_coverage(Parser.subtable(data, base_coverage_off))

    mark_array_tbl = Parser.subtable(data, mark_array_offset)
    mark_array = Parser.parse_mark_array(mark_array_tbl)
    # base array table
    base_tbl = Parser.subtable(data, base_array_offset)
    <<n_recs::16, records::binary-size(n_recs)-unit(16), _::binary>> = base_tbl
    # array of offsets to ligature attach tables
    la = for <<(<<off::16>> <- records)>>, do: Parser.subtable(base_tbl, off)
    component_size = n_classes * 2
    # each component is array of offsets (size == size of mark array) to anchor tables
    #  -- one for each mark class including class 0; may be NULL
    base_array =
      la
      |> Enum.map(fn la_tbl ->
        <<n_components::16, recs::binary>> = la_tbl
        recs = binary_part(recs, 0, n_components * component_size)
        comps = for <<(<<record::binary-size(component_size)>> <- recs)>>, do: record

        comps
        |> Enum.map(fn r -> for <<(<<offset::16>> <- r)>>, do: offset end)
        |> Enum.map(
          &Enum.map(&1, fn o ->
            if o != 0, do: Parser.parse_anchor(binary_part(la_tbl, o, 6)), else: nil
          end)
        )
      end)

    {mark_coverage, base_coverage, base_array, mark_array}
  end

  def parse_lookup(6, data) do
    # same format at GPOS 4
    parse_lookup(4, data)
  end

  def parse_lookup(7, data) do
    <<format::16, details::binary>> = data

    val =
      case format do
        1 ->
          nil

        2 ->
          <<cov_off::16, class_def_off::16, n_rulesets::16,
            srs_off::binary-size(n_rulesets)-unit(16), _::binary>> = details

          coverage = Parser.parse_coverage(Parser.subtable(data, cov_off))
          classes = Parser.parse_glyph_class(Parser.subtable(data, class_def_off))

          srs =
            for <<(<<offset::16>> <- srs_off)>>,
              do: if(offset != 0, do: Parser.subtable(data, offset), else: nil)

          rulesets =
            srs
            |> Enum.map(fn ruleset ->
              if ruleset != nil do
                <<n_rules::16, sr_off::binary-size(n_rules)-unit(16), _::binary>> = ruleset
                rules = for <<(<<offset::16>> <- sr_off)>>, do: Parser.subtable(ruleset, offset)
                rules |> Enum.map(&Parser.parse_context_sub_rule1(&1))
              else
                nil
              end
            end)

          {coverage, rulesets, classes}

        3 ->
          nil

        _ ->
          nil
      end

    {format, val}
  end

  def parse_lookup(8, data) do
    <<format::16, details::binary>> = data

    val =
      case format do
        1 ->
          nil

        2 ->
          nil

        3 ->
          <<backtrack_count::16, backoff::binary-size(backtrack_count)-unit(16), input_count::16,
            input_off::binary-size(input_count)-unit(16), lookahead_count::16,
            lookahead_off::binary-size(lookahead_count)-unit(16), subst_count::16,
            subst_recs::binary-size(subst_count)-unit(32), _::binary>> = details

          # parse the coverage tables and positioning records
          back_offsets = for <<(<<x::16>> <- backoff)>>, do: x

          bt_coverage =
            Enum.map(back_offsets, fn cov_off ->
              Parser.parse_coverage(Parser.subtable(data, cov_off))
            end)

          input_offsets = for <<(<<x::16>> <- input_off)>>, do: x

          coverage =
            Enum.map(input_offsets, fn cov_off ->
              Parser.parse_coverage(Parser.subtable(data, cov_off))
            end)

          lookahead_offsets = for <<(<<x::16>> <- lookahead_off)>>, do: x

          la_coverage =
            Enum.map(lookahead_offsets, fn cov_off ->
              Parser.parse_coverage(Parser.subtable(data, cov_off))
            end)

          # index x at which to apply lookup y
          pos_records = for <<(<<x::16, y::16>> <- subst_recs)>>, do: {x, y}

          {bt_coverage, coverage, la_coverage, pos_records}

        _ ->
          nil
      end

    {format, val}
  end

  defp apply_mark_to_lig(
         _mark_coverage,
         _base_coverage,
         _base_array,
         _mark_array,
         _lookup_flag,
         _mfs,
         _gdef,
         prev,
         [],
         pos
       ),
       do: {pos, prev}

  defp apply_mark_to_lig(
         mark_coverage,
         base_coverage,
         base_array,
         mark_array,
         lookup_flag,
         mfs,
         gdef,
         [],
         [{g, gi} | glyphs],
         pos
       ) do
    apply_mark_to_lig(
      mark_coverage,
      base_coverage,
      base_array,
      mark_array,
      lookup_flag,
      mfs,
      gdef,
      [{g, gi}],
      glyphs,
      pos
    )
  end

  defp apply_mark_to_lig(
         mark_coverage,
         base_coverage,
         base_array,
         mark_array,
         lookup_flag,
         mfs,
         gdef,
         prev,
         [{g, gi} | glyphs],
         pos
       ) do
    skip_mark = should_skip_glyph(g.glyph, lookup_flag, gdef, mfs)
    markloc = find_coverage_index(mark_coverage, g.glyph)

    # TODO: this code assumes prev is a lig
    {base_glyph, prev_i} =
      if gdef != nil do
        # find a base
        base = Enum.find(prev, fn {x, _} -> classify_glyph(x.glyph, gdef.classes) == 2 end)
        # handle case where no lig anywhere in preceding characters
        if base == nil, do: hd(prev), else: base
      else
        hd(prev)
      end

    baseloc = find_coverage_index(base_coverage, base_glyph.glyph)

    {mark_offset, delta} =
      if markloc != nil and baseloc != nil and !skip_mark and g.m_lig_component > 0 do
        # IO.puts "LIG: ready to apply #{inspect g} to #{inspect base_glyph}"
        b = Enum.at(base_array, baseloc)
        {class, {mark_x, mark_y}} = Enum.at(mark_array, markloc)
        base_anchors = Enum.at(b, g.m_lig_component - 1)
        # IO.puts "LIG sel anchors: #{inspect base_anchors}"
        {base_x, base_y} = Enum.at(base_anchors, class)
        # align the anchors
        {off_x, off_y} = {base_x - mark_x, base_y - mark_y}
        index_delta = gi - prev_i
        {{:pos, off_x, off_y, 0, 0}, -index_delta}
      else
        {Enum.at(pos, gi), 0}
      end

    updated = pos |> List.replace_at(gi, mark_offset)
    g = if g.mark_delta == 0, do: %{g | mark_delta: delta}, else: g

    apply_mark_to_lig(
      mark_coverage,
      base_coverage,
      base_array,
      mark_array,
      lookup_flag,
      mfs,
      gdef,
      [{g, gi} | prev],
      glyphs,
      updated
    )
  end

  defp apply_mark_to_base(
         _mark_coverage,
         _base_coverage,
         _base_array,
         _mark_array,
         _lookup_flag,
         _mfs,
         _gdef,
         prev,
         [],
         pos
       ),
       do: {pos, prev}

  defp apply_mark_to_base(
         mark_coverage,
         base_coverage,
         base_array,
         mark_array,
         lookup_flag,
         mfs,
         gdef,
         [],
         [{g, gi} | glyphs],
         pos
       ) do
    apply_mark_to_base(
      mark_coverage,
      base_coverage,
      base_array,
      mark_array,
      lookup_flag,
      mfs,
      gdef,
      [{g, gi}],
      glyphs,
      pos
    )
  end

  defp apply_mark_to_base(
         mark_coverage,
         base_coverage,
         base_array,
         mark_array,
         lookup_flag,
         mfs,
         gdef,
         prev,
         [{g, gi} | glyphs],
         pos
       ) do
    # should we skip this glyph?
    skip_mark = should_skip_glyph(g.glyph, lookup_flag, gdef, mfs)
    markloc = find_coverage_index(mark_coverage, g.glyph)

    # TODO: this code assumes prev is a base
    {base_glyph, prev_i} =
      if gdef != nil do
        # find a base
        base = Enum.find(prev, fn {x, _} -> classify_glyph(x.glyph, gdef.classes) == 1 end)
        # handle case where no base anywhere in preceding characters
        if base == nil, do: hd(prev), else: base
      else
        hd(prev)
      end

    baseloc = find_coverage_index(base_coverage, base_glyph.glyph)

    {mark_offset, delta} =
      if markloc != nil and baseloc != nil and !skip_mark do
        b = Enum.at(base_array, baseloc)
        {class, {mark_x, mark_y}} = Enum.at(mark_array, markloc)
        {base_x, base_y} = Enum.at(b, class)
        # align the anchors
        {off_x, off_y} = {base_x - mark_x, base_y - mark_y}
        index_delta = gi - prev_i
        {{:pos, off_x, off_y, 0, 0}, -index_delta}
      else
        {Enum.at(pos, gi), 0}
      end

    updated = pos |> List.replace_at(gi, mark_offset)
    g = if g.mark_delta == 0, do: %{g | mark_delta: delta}, else: g

    apply_mark_to_base(
      mark_coverage,
      base_coverage,
      base_array,
      mark_array,
      lookup_flag,
      mfs,
      gdef,
      [{g, gi} | prev],
      glyphs,
      updated
    )
  end

  defp apply_kerning2(_class_def1, _clas_def2, _pair_sets, [], output), do: output
  defp apply_kerning2(_class_def1, _clas_def2, _pair_sets, [_], output), do: output ++ [nil]

  defp apply_kerning2(class_def1, class_def2, pairsets, [g1, g2 | glyphs], output) do
    c1 = classify_glyph(g1.glyph, class_def1)
    c2 = classify_glyph(g2.glyph, class_def2)
    pair = pairsets |> Enum.at(c1) |> Enum.at(c2)

    {output, glyphs} =
      if pair != nil do
        {v1, v2} = pair
        oo = output ++ [v1]

        if v2 != nil do
          {oo ++ [v2], glyphs}
        else
          {oo, [g2 | glyphs]}
        end
      else
        {output ++ [nil], [g2 | glyphs]}
      end

    apply_kerning2(class_def1, class_def1, pairsets, glyphs, output)
  end

  defp apply_kerning(_coverage, _pair_sets, [], output), do: output
  defp apply_kerning(_coverage, _pair_sets, [_], output), do: output ++ [nil]

  defp apply_kerning(coverage, pair_sets, [g | glyphs], output) do
    # get the index of a pair set that might apply
    coverloc = find_coverage_index(coverage, g.glyph)

    {output, glyphs} =
      if coverloc != nil do
        pair_set = Enum.at(pair_sets, coverloc)
        next_char = hd(glyphs)
        pair = Enum.find(pair_set, fn {g, _, _} -> g == next_char.glyph end)

        if pair != nil do
          {_, v1, v2} = pair
          oo = output ++ [v1]

          if v2 != nil do
            {oo ++ [v2], tl(glyphs)}
          else
            {oo, glyphs}
          end
        else
          {output ++ [nil], glyphs}
        end
      else
        {output ++ [nil], glyphs}
      end

    apply_kerning(coverage, pair_sets, glyphs, output)
  end

  defp apply_cursive(_coverage, _anchor_pairs, _flag, _mfs, _gdef, _is_rtl, prev, [], pos),
    do: {pos, prev}

  defp apply_cursive(_coverage, _anchor_pairs, _flag, _mfs, _gdef, _is_rtl, prev, [x], pos),
    do: {pos, [x | prev]}

  defp apply_cursive(
         coverage,
         anchor_pairs,
         flag,
         mfs,
         gdef,
         is_rtl,
         prev,
         [{g, gi}, {g2, gi2} | glyphs],
         pos
       ) do
    # flag now ignored!
    # decompose the flag
    <<_attachment_type::8, _::3, _use_mark::1, _ignore_mark::1, _ignore_lig::1, _ignore_base::1,
      rtl::1>> = <<flag::16>>

    curloc = find_coverage_index(coverage, g.glyph)
    nextloc = find_coverage_index(coverage, g2.glyph)

    p = Enum.at(pos, gi)
    p2 = Enum.at(pos, gi2)

    # if glyphs are covered
    [cur, next, gdelta] =
      if curloc != nil and nextloc != nil do
        {entry_a, _} = Enum.at(anchor_pairs, nextloc)
        {_, exit_a} = Enum.at(anchor_pairs, curloc)

        if exit_a != nil and entry_a != nil do
          {entry_x, entry_y} = entry_a
          {exit_x, exit_y} = exit_a
          {_, x_off, _y_off, x_adv, y_adv} = p
          {_, x2Off, y2Off, x2Adv, y2Adv} = p2
          delta_y = if rtl, do: entry_y - exit_y, else: exit_y - entry_y
          index_delta = gi2 - gi

          if is_rtl do
            delta_x = exit_x + x_off
            # post-process -- our y_offset needs to be adjusted by next y_offset
            [
              {:pos, x_off - delta_x, delta_y, x_adv - delta_x, y_adv},
              {:pos, x2Off, y2Off, entry_x + x2Off, y2Adv},
              index_delta
            ]
          else
            delta_x = entry_x + x2Off
            # post-process -- next needs to adjust y_offset by our y_offset
            [
              {:pos, x_off, delta_y, exit_x + x_off, y_adv},
              {:pos, x2Off - delta_x, y2Off, x2Adv - delta_x, y2Adv},
              -index_delta
            ]
          end
        else
          [p, p2, 0]
        end
      else
        [p, p2, 0]
      end

    updated = pos |> List.replace_at(gi, cur) |> List.replace_at(gi2, next)
    g = if g.cursive_delta == 0, do: %{g | cursive_delta: gdelta}, else: g

    apply_cursive(
      coverage,
      anchor_pairs,
      flag,
      mfs,
      gdef,
      is_rtl,
      [{g, gi} | prev],
      [{g2, gi2} | glyphs],
      updated
    )
  end

  # adjust the y-offsets of connected glyphs
  def adjust_cursive_offset(positions, deltas) do
    0..(length(deltas) - 1)
    |> Enum.reduce({positions, deltas}, fn x, {p, d} -> adjust_cursive_offsets(x, p, d) end)
  end

  # this is recursive since offset accumulates over a run of connected glyphs
  # Urdu in particular shows this in action
  def adjust_cursive_offsets(index, positions, deltas) do
    d = Enum.at(deltas, index)

    if d == 0 do
      {positions, deltas}
    else
      next = index + d
      {p2, d2} = adjust_cursive_offsets(next, positions, List.replace_at(deltas, index, 0))
      {type, xo, yo, xa, ya} = Enum.at(p2, index)
      {_, _, yo2, _, _} = Enum.at(p2, next)

      {List.replace_at(p2, index, {type, xo, yo + yo2, xa, ya}), d2}
    end
  end

  # adjust marks relative to base
  def adjust_mark_offsets(positions, deltas, is_rtl) do
    0..(length(deltas) - 1)
    |> Enum.reduce({positions, deltas}, fn x, {p, d} -> adjust_mark_offsets(x, p, d, is_rtl) end)
  end

  def adjust_mark_offsets(index, positions, deltas, is_rtl) do
    d = Enum.at(deltas, index)

    if d == 0 do
      {positions, deltas}
    else
      base = index + d
      {_, box, boy, bax, bay} = Enum.at(positions, base)
      {type, mox, moy, _max, _may} = Enum.at(positions, index)

      adjusted =
        if is_rtl do
          {type, box + mox, boy + moy, 0, 0}
        else
          # LTR marks subtract the base advance
          {type, box + mox - bax, boy + moy - bay, 0, 0}
        end

      {List.replace_at(positions, index, adjusted), List.replace_at(deltas, index, 0)}
    end
  end

  # class-based context
  defp apply_context_pos2(_coverage, _rulesets, _classes, _gdef, _lookups, [], [], output),
    do: output

  defp apply_context_pos2(
         coverage,
         rulesets,
         classes,
         gdef,
         lookups,
         [g | glyphs],
         [p | pos],
         output
       ) do
    coverloc = find_coverage_index(coverage, g.glyph)
    ruleset = Enum.at(rulesets, classify_glyph(g.glyph, classes))

    {o, glyphs, pos} =
      if coverloc != nil and ruleset != nil do
        # find first match in this ruleset
        # TODO: flag might mean we need to filter ignored categories
        # ie; skip marks
        rule =
          Enum.find(ruleset, fn {input, _} ->
            candidates =
              glyphs
              |> Enum.take(length(input))
              |> Enum.map(fn g -> classify_glyph(g.glyph, classes) end)

            candidates == input
          end)

        if rule != nil do
          {matched, subst_records} = rule

          input =
            [g | Enum.take(glyphs, length(matched))]
            |> Enum.zip([p | Enum.take(pos, length(matched))])

          replaced =
            subst_records
            |> Enum.reduce(input, fn {input_loc, lookup_index}, acc ->
              {candidate, candidate_position} = Enum.at(acc, input_loc)
              lookup = Enum.at(lookups, lookup_index)

              {_, [adjusted_pos | _]} =
                parse_and_apply(lookup, gdef, lookups, nil, {[candidate], [candidate_position]})

              List.replace_at(acc, input_loc, {candidate, adjusted_pos})
            end)

          # skip over any matched glyphs
          # TODO: handle flags correctly
          # probably want to prepend earlier ignored glyphs to remaining
          _remaining = Enum.slice(glyphs, length(matched), length(glyphs))
          _remaining_pos = Enum.slice(pos, length(matched), length(pos))
          # {replaced, remaining, remaining_pos}
          {[nil], glyphs, pos}
        else
          {[nil], glyphs, pos}
        end
      else
        {[nil], glyphs, pos}
      end

    output = output ++ o
    apply_context_pos2(coverage, rulesets, classes, gdef, lookups, glyphs, pos, output)
  end

  # handle coverage-based format for context chaining
  defp apply_chaining_context_pos3(
         _bt_coverage,
         _coverage,
         _la_coverage,
         _subst,
         _gdef,
         _lookups,
         [],
         pos,
         _
       ),
       do: pos

  defp apply_chaining_context_pos3(
         bt_coverage,
         coverage,
         la_coverage,
         pos_records,
         gdef,
         lookups,
         [{g, index} | glyphs],
         pos,
         output
       ) do
    backtrack = length(bt_coverage)
    input_extra = length(coverage) - 1
    lookahead = length(la_coverage)
    # not enough backtracking or lookahead to even attempt match
    oo =
      if length(output) < backtrack or length(glyphs) < lookahead + input_extra do
        # positioning unchanged
        pos
      else
        # do we match the input
        input = [{g, index} | Enum.take(glyphs, input_extra)]

        input_matches =
          input
          |> Enum.zip(coverage)
          |> Enum.all?(fn {{g, _index}, cov} -> find_coverage_index(cov, g.glyph) != nil end)

        # do we match backtracking?
        back_matches =
          if backtrack > 0 do
            output
            |> Enum.take(backtrack)
            |> Enum.zip(bt_coverage)
            |> Enum.all?(fn {{g, _index}, cov} -> find_coverage_index(cov, g.glyph) != nil end)
          else
            true
          end

        # do we match lookahead
        la_matches =
          if lookahead > 0 do
            glyphs
            |> Enum.drop(input_extra)
            |> Enum.take(lookahead)
            |> Enum.zip(la_coverage)
            |> Enum.all?(fn {{g, _index}, cov} -> find_coverage_index(cov, g.glyph) != nil end)
          else
            true
          end

        if input_matches and back_matches and la_matches do
          pos_records
          |> Enum.reduce(pos, fn {input_index, lookup_index}, acc_pos ->
            lookup = Enum.at(lookups, lookup_index)
            {g, index} = Enum.at(input, input_index)
            candidate_position = Enum.at(acc_pos, index)
            # we only care about the new pos
            {_, [adjusted_pos | _]} =
              parse_and_apply(lookup, gdef, lookups, nil, {[g], [candidate_position]})

            List.replace_at(acc_pos, index, adjusted_pos)
          end)
        else
          pos
        end
      end

    # oo is the possibly updated positioning
    # output will be used for backtracking
    apply_chaining_context_pos3(
      bt_coverage,
      coverage,
      la_coverage,
      pos_records,
      gdef,
      lookups,
      glyphs,
      oo,
      [{g, index} | output]
    )
  end

  # given a glyph, find out the coverage index (can be nil)
  defp find_coverage_index(cov, g) when is_integer(hd(cov)) do
    Enum.find_index(cov, fn i -> i == g end)
  end

  defp find_coverage_index(cov, g) when is_tuple(hd(cov)) do
    r = Enum.find(cov, fn {f, l, _} -> f <= g and g <= l end)

    if r != nil do
      {s, _, i} = r
      i + g - s
    else
      nil
    end
  end

  # catch-all
  defp find_coverage_index(_cov, _g) do
    nil
  end

  defp classify_glyph(_g, nil), do: 0

  defp classify_glyph(g, classes) when is_map(classes) do
    Map.get(classes, g, 0)
  end

  defp classify_glyph(g, ranges) do
    r = Enum.find(ranges, fn {f, l, _} -> f <= g and g <= l end)
    # if no match, class 0
    if r == nil do
      0
    else
      {_, _, class} = r
      class
    end
  end

  # uses the lookup flag to determine which glyphs are ignored
  # this could be shared with GSUB implementation
  def should_skip_glyph(g, flag, gdef, mfs) do
    # decompose the flag
    <<attachment_type::8, _::3, use_mark_filtering_set::1, ignore_mark::1, ignore_lig::1,
      ignore_base::1, _rtl::1>> = <<flag::16>>

    # only care about classification if flag set
    # if only RTL flag don't bother classifying
    glyph_class =
      if flag > 1 do
        classify_glyph(g, gdef.classes)
      else
        0
      end

    cond do
      # short circuit - if no flags, we aren't skipping anything
      flag == 0 ->
        false

      # if only RTL flag not skipping anything
      flag == 1 ->
        false

      # skip if ignore is set and we match the corresponding GDEF class
      ignore_base == 1 and glyph_class == 1 ->
        true

      ignore_lig == 1 and glyph_class == 2 ->
        true

      ignore_mark == 1 and glyph_class == 3 ->
        true

      # skip if we have a mark that isn't in specified mark glyph set
      use_mark_filtering_set == 1 and classify_glyph(g, gdef.classes) == 3 and
          find_coverage_index(Enum.at(gdef.mark_sets, mfs), g) == nil ->
        true

      # skip if we don't match a non-zero attachment type
      attachment_type != 0 and classify_glyph(g, gdef.attachments) != attachment_type ->
        true

      # default is DO NOT SKIP
      true ->
        false
    end
  end

  # then we use / save {pos, index}
  # returns array of {glyph, index} of glyphs that are not ignored (according to lookup flag)
  def filter_glyphs(glyphs, flag, gdef, mfs) do
    glyphs
    |> Stream.with_index()
    |> Stream.reject(fn {g, _} -> should_skip_glyph(g.glyph, flag, gdef, mfs) end)
  end
end
