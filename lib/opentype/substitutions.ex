defmodule OpenType.Substitutions do
  alias OpenType.Parser
  alias OpenType.GlyphInfo
  require Logger

  # ==============================================
  # GSUB glyph substitutions
  # Used for ligatures, swashes, alternate forms
  # if a lookup type is as-yet unsupported
  # simply passes through the input
  # ==============================================

  # parse a lookup table
  def parse_lookup_table(index, lookups) do
    lookup = lookups |> Enum.at(index)

    parsed =
      case lookup do
        # extended lookup table, need to return actual lookup type
        {7, flag, offsets, table, mfs} ->
          {actual_type, output} = parse_lookup(7, offsets, table)
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

  defp parse_and_apply({:parsed, type, flag, mfs, data}, gdef, lookups, tag, glyphs) do
    apply_substitution({:parsed, type, flag, mfs, data}, gdef, lookups, tag, glyphs)
  end

  defp parse_and_apply({7, flag, offsets, data, mfs}, gdef, lookups, tag, glyphs) do
    {actual_type, output} = parse_lookup(7, offsets, data)
    val = {:parsed, actual_type, flag, mfs, output}
    apply_substitution(val, gdef, lookups, tag, glyphs)
  end

  defp parse_and_apply({type, flag, offsets, data, mfs}, gdef, lookups, tag, glyphs) do
    val = {:parsed, type, flag, mfs, parse_lookup(type, offsets, data)}
    apply_substitution(val, gdef, lookups, tag, glyphs)
  end

  @doc """
  Apply the substitution to the input list of glyphs.
  """
  def apply_substitution({:parsed, type, flag, mfs, data}, gdef, lookups, tag, glyphs)
      when is_list(data) do
    Enum.reduce(data, glyphs, fn subdata, input ->
      apply_substitution({:parsed, type, flag, mfs, subdata}, gdef, lookups, tag, input)
    end)
  end

  # GSUB 1 -- single substitution (one-for-one)
  def apply_substitution({:parsed, 1, _flag, _mfs, sub}, _gdef, _lookups, tag, glyphs) do
    {coverage, replacements} = sub
    replace = fn g -> apply_single_sub(g, coverage, replacements) end

    output =
      if tag != nil do
        glyphs
        |> Enum.map(fn g -> if g.tag == tag, do: replace.(g), else: g end)
      else
        Enum.map(glyphs, replace)
      end

    output
  end

  # GSUB 2 - multiple substitution (expand one glyph into several)
  def apply_substitution({:parsed, 2, _flag, _mfs, sub}, _gdef, _, tag, glyphs) do
    {coverage, sequences} = sub

    output =
      if tag != nil do
        glyphs
        |> Enum.map(fn g ->
          if g.tag == tag, do: apply_multi_sub(g, coverage, sequences), else: g
        end)
      else
        glyphs
        |> Enum.map(fn g -> apply_multi_sub(g, coverage, sequences) end)
      end

    List.flatten(output)
  end

  # GSUB type 3 -- alternate substitution (one-for-one)
  def apply_substitution({:parsed, 3, _flag, _mfs, sub}, _gdef, _, tag, glyphs) do
    {coverage, alts} = sub
    # TODO: seems like there's a way in unicode to specify alt??
    # More research required, for now substitute a random alt
    if tag != nil do
      glyphs
      |> Enum.map(fn g -> if g.tag == tag, do: apply_random_alt(g, coverage, alts), else: g end)
    end

    Enum.map(glyphs, fn g -> apply_random_alt(g, coverage, alts) end)
  end

  # GSUB type 4 -- ligature substition (single glyph replaces multiple glyphs)
  def apply_substitution({:parsed, 4, flag, mfs, sub}, gdef, _, tag, glyphs) do
    {coverage, liga_off} = sub

    if tag != nil do
      IO.puts("GSUB 4 per-glyph #{tag} lookup")
    end

    # TODO: figure out per-glyph-annotation for ligature!
    # TODO: also need to track components through to positioning for GPOS5
    apply_ligature(coverage, liga_off, flag, gdef, mfs, glyphs, [])
  end

  # GSUB type 5 -- contextual substitution
  def apply_substitution({:parsed, 5, _flag, _mfs, sub}, _gdef, lookups, tag, glyphs) do
    if tag != nil do
      IO.puts("GSUB 5 per-glyph #{tag} lookup")
    end

    {format, rules} = sub

    output =
      case format do
        1 ->
          {coverage, rulesets} = rules
          apply_context_sub1(coverage, rulesets, lookups, glyphs, [])

        2 ->
          {coverage, rulesets, classes} = rules
          apply_context_sub2(coverage, rulesets, classes, lookups, glyphs, [])

        3 ->
          Logger.debug("GSUB 5.3 - contextual substitution")
          glyphs

        _ ->
          Logger.debug("GSUB 5 - contextual substitution format #{format}")
          glyphs
      end

    output
  end

  # GSUB type 6 -- chained contextual substitution
  def apply_substitution({:parsed, 6, flag, mfs, sub}, gdef, lookups, tag, glyphs) do
    {format, rules} = sub

    output =
      case format do
        1 ->
          Logger.debug("GSUB 6.1 - chained substitution")
          glyphs

        2 ->
          if tag != nil do
            IO.puts("GSUB 6 per-glyph #{tag} lookup; type #{format}")
          end

          {coverage, rulesets, bt_classes, input_classes, la_classes} = rules
          # {f, skipped} = filter_glyphs(glyphs, flag, gdef, mfs)
          apply_chaining_context_sub2(
            coverage,
            rulesets,
            {bt_classes, input_classes, la_classes},
            lookups,
            glyphs,
            []
          )

        # unfilter_glyphs(replaced, skipped)
        3 ->
          {bt_coverage, coverage, la_coverage, subst_records} = rules
          {f, skipped} = filter_glyphs(glyphs, flag, gdef, mfs)

          replaced =
            apply_chaining_context_sub3(
              bt_coverage,
              coverage,
              la_coverage,
              subst_records,
              lookups,
              f,
              tag,
              []
            )

          unfilter_glyphs(replaced, skipped)

        _ ->
          Logger.debug("GSUB 6 - chaining substitution format #{format}")
          glyphs
      end

    output
  end

  # GSUB type 7 is only parsed, never applied!!
  def apply_substitution({:parsed, 7, _flag, _mfs, _sub}, _gdef, _lookups, _tag, glyphs) do
    Logger.error("GSUB 7 is for parsing extended tables, not applying!")
    glyphs
  end

  # GSUB type 8 -- reverse chained contextual substitution
  def apply_substitution({:parsed, 8, flag, mfs, sub}, gdef, lookups, tag, glyphs) do
    if tag != nil do
      IO.puts("GSUB 8 per-glyph #{tag} lookup")
    end

    # Logger.debug "GSUB 8 - reverse chaining substitution"
    Logger.debug("GSUB 8 - reverse chaining substitution")
    {bt_coverage, coverage, la_coverage, subst_records} = sub
    {f, skipped} = filter_glyphs(glyphs, flag, gdef, mfs)

    replaced =
      apply_reverse_chaining_context(
        bt_coverage,
        coverage,
        la_coverage,
        subst_records,
        lookups,
        f,
        []
      )

    output = unfilter_glyphs(replaced, skipped)
    output
  end

  @doc """
  Parse a lookup table into the information needed to resolve it
  """
  def parse_lookup(7, offsets, data) do
    # GSUB type 7 -- extended table
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

  # GSUB 1 - return {coverage, delta | [replacements]}
  def parse_lookup(1, data) do
    <<format::16, cov_off::16, rest::binary>> = data
    coverage = Parser.parse_coverage(Parser.subtable(data, cov_off))

    replacements =
      case format do
        1 ->
          <<delta::16, _::binary>> = rest
          delta

        2 ->
          <<n_glyphs::16, ga::binary-size(n_glyphs)-unit(16), _::binary>> = rest
          for <<(<<x::16>> <- ga)>>, do: x

        _ ->
          nil
      end

    {coverage, replacements}
  end

  # GSUB 2 - return {coverage, sequences}
  def parse_lookup(2, data) do
    <<_format::16, cov_off::16, n_seq::16, sub_off::binary-size(n_seq)-unit(16), _::binary>> =
      data

    coverage = Parser.parse_coverage(Parser.subtable(data, cov_off))
    # sequence tables
    seq_offsets = for <<(<<x::16>> <- sub_off)>>, do: x

    # sequence table structure identical to alt table
    sequences = Enum.map(seq_offsets, fn seq_offset -> Parser.parse_alts(data, seq_offset) end)
    {coverage, sequences}
  end

  # GSUB 3 - return {coverage, alts}
  def parse_lookup(3, data) do
    <<1::16, cov_off::16, n_alt_sets::16, aoff::binary-size(n_alt_sets)-unit(16), _::binary>> =
      data

    coverage = Parser.parse_coverage(Parser.subtable(data, cov_off))
    # alternate set tables
    alt_offsets = for <<(<<x::16>> <- aoff)>>, do: x
    alts = Enum.map(alt_offsets, fn alt_offset -> Parser.parse_alts(data, alt_offset) end)
    {coverage, alts}
  end

  # GSUB type 4 -- ligature substition (single glyph replaces multiple glyphs)
  def parse_lookup(4, data) do
    # parse ligature table
    <<1::16, cov_off::16, n_lig_sets::16, lsl::binary-size(n_lig_sets)-unit(16), _::binary>> =
      data

    # ligature set tables
    ls = for <<(<<x::16>> <- lsl)>>, do: x
    # coverage table
    coverage = Parser.parse_coverage(Parser.subtable(data, cov_off))
    # ligatures
    liga_off = Enum.map(ls, fn ls_offset -> Parser.parse_ligature_set(data, ls_offset) end)
    {coverage, liga_off}
  end

  #
  # GSUB type 5 -- contextual substitution
  def parse_lookup(5, table) do
    <<format::16, details::binary>> = table

    output =
      case format do
        1 ->
          <<cov_off::16, n_rulesets::16, srs_off::binary-size(n_rulesets)-unit(16), _::binary>> =
            details

          coverage = Parser.parse_coverage(Parser.subtable(table, cov_off))
          srs = for <<(<<offset::16>> <- srs_off)>>, do: Parser.subtable(table, offset)

          rulesets =
            srs
            |> Enum.map(fn ruleset ->
              <<n_rules::16, sr_off::binary-size(n_rules)-unit(16), _::binary>> = ruleset
              rules = for <<(<<offset::16>> <- sr_off)>>, do: Parser.subtable(ruleset, offset)
              rules |> Enum.map(&Parser.parse_context_sub_rule1(&1))
            end)

          {coverage, rulesets}

        2 ->
          <<cov_off::16, class_def_off::16, n_rulesets::16,
            srs_off::binary-size(n_rulesets)-unit(16), _::binary>> = details

          coverage = Parser.parse_coverage(Parser.subtable(table, cov_off))
          classes = Parser.parse_glyph_class(Parser.subtable(table, class_def_off))

          srs =
            for <<(<<offset::16>> <- srs_off)>>,
              do: if(offset != 0, do: Parser.subtable(table, offset), else: nil)

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
          Logger.debug("GSUB 5 - contextual substitution format #{format}")
          nil
      end

    {format, output}
  end

  # GSUB type 6 -- chained contextual substitution
  def parse_lookup(6, table) do
    <<format::16, details::binary>> = table

    output =
      case format do
        1 ->
          Logger.debug("GSUB 6.1 - chained substitution")
          nil

        2 ->
          <<cov_off::16, bt_class_off::16, input_class_off::16, la_class_off::16,
            n_class_sets::16, class_set_off::binary-size(n_class_sets)-unit(16),
            _::binary>> = details

          coverage = Parser.parse_coverage(Parser.subtable(table, cov_off))
          bt_classes = Parser.parse_glyph_class(Parser.subtable(table, bt_class_off))
          input_classes = Parser.parse_glyph_class(Parser.subtable(table, input_class_off))
          la_classes = Parser.parse_glyph_class(Parser.subtable(table, la_class_off))

          srs =
            for <<(<<offset::16>> <- class_set_off)>>,
              do: if(offset != 0, do: Parser.subtable(table, offset), else: nil)

          rulesets =
            srs
            |> Enum.map(fn ruleset ->
              if ruleset != nil do
                <<n_rules::16, sr_off::binary-size(n_rules)-unit(16), _::binary>> = ruleset
                rules = for <<(<<offset::16>> <- sr_off)>>, do: Parser.subtable(ruleset, offset)
                rules |> Enum.map(&Parser.parse_chained_sub_rule2(&1))
              else
                nil
              end
            end)

          {coverage, rulesets, bt_classes, input_classes, la_classes}

        3 ->
          <<backtrack_count::16, backoff::binary-size(backtrack_count)-unit(16), input_count::16,
            input_off::binary-size(input_count)-unit(16), lookahead_count::16,
            lookahead_off::binary-size(lookahead_count)-unit(16), subst_count::16,
            subst_recs::binary-size(subst_count)-unit(32), _::binary>> = details

          back_offsets = for <<(<<x::16>> <- backoff)>>, do: x

          bt_coverage =
            Enum.map(back_offsets, fn cov_off ->
              Parser.parse_coverage(Parser.subtable(table, cov_off))
            end)

          input_offsets = for <<(<<x::16>> <- input_off)>>, do: x

          coverage =
            Enum.map(input_offsets, fn cov_off ->
              Parser.parse_coverage(Parser.subtable(table, cov_off))
            end)

          lookahead_offsets = for <<(<<x::16>> <- lookahead_off)>>, do: x

          la_coverage =
            Enum.map(lookahead_offsets, fn cov_off ->
              Parser.parse_coverage(Parser.subtable(table, cov_off))
            end)

          subst_records = for <<(<<x::16, y::16>> <- subst_recs)>>, do: {x, y}
          {bt_coverage, coverage, la_coverage, subst_records}

        _ ->
          Logger.debug("GSUB 6 - chaining substitution format #{format}")
          nil
      end

    {format, output}
  end

  # GSUB type 8 -- reverse chained contextual substitution (one-for-one)
  def parse_lookup(8, table) do
    # Logger.debug "GSUB 8 - reverse chaining substitution"
    <<1::16, cov_off::16, details::binary>> = table

    <<backtrack_count::16, backoff::binary-size(backtrack_count)-unit(16), lookahead_count::16,
      lookahead_off::binary-size(lookahead_count)-unit(16), subst_count::16,
      subst_recs::binary-size(subst_count)-unit(16), _::binary>> = details

    coverage = Parser.parse_coverage(Parser.subtable(table, cov_off))
    back_offsets = for <<(<<x::16>> <- backoff)>>, do: x

    bt_coverage =
      Enum.map(back_offsets, fn cov_off ->
        Parser.parse_coverage(Parser.subtable(table, cov_off))
      end)

    lookahead_offsets = for <<(<<x::16>> <- lookahead_off)>>, do: x

    la_coverage =
      Enum.map(lookahead_offsets, fn cov_off ->
        Parser.parse_coverage(Parser.subtable(table, cov_off))
      end)

    subst_records = for <<(<<x::16>> <- subst_recs)>>, do: x
    {bt_coverage, coverage, la_coverage, subst_records}
  end

  defp apply_single_sub(g, _coverage, nil), do: g

  defp apply_single_sub(g, coverage, delta) when is_integer(delta) do
    coverloc = find_coverage_index(coverage, g.glyph)
    if coverloc != nil, do: %{g | glyph: g.glyph + delta}, else: g
  end

  defp apply_single_sub(g, coverage, replacements) do
    coverloc = find_coverage_index(coverage, g.glyph)
    if coverloc != nil, do: %{g | glyph: Enum.at(replacements, coverloc)}, else: g
  end

  defp apply_multi_sub(g, coverage, seq) do
    coverloc = find_coverage_index(coverage, g.glyph)

    if coverloc != nil do
      replacements = Enum.at(seq, coverloc)
      new_g = %{g | glyph: hd(replacements)}

      e =
        tl(replacements)
        |> Enum.map(fn x -> %GlyphInfo{glyph: x, tag: g.tag} end)

      [new_g | e]
    else
      g
    end
  end

  defp apply_random_alt(g, coverage, alts) do
    coverloc = find_coverage_index(coverage, g.glyph)

    if coverloc != nil do
      candidates = Enum.at(alts, coverloc)
      %{g | glyph: Enum.random(candidates)}
    else
      g
    end
  end

  defp match_ligature_rule?(glyphs, match, flag, gdef, mfs) do
    candidate =
      glyphs
      |> lig_components(length(match), flag, gdef, mfs)
      |> Enum.map(fn {g, _} -> g.glyph end)

    candidate == match
  end

  defp lig_components(glyphs, n, flag, gdef, mfs) do
    glyphs
    |> Stream.with_index(1)
    |> Stream.reject(fn {g, _} -> should_skip_glyph?(g.glyph, flag, gdef, mfs) end)
    |> Stream.take(n)
  end

  defp apply_ligature(_coverage, _ligatures, _flag, _gdef, _mfs, [], output),
    do: Enum.reverse(output)

  defp apply_ligature(coverage, ligatures, flag, gdef, mfs, [g | glyphs], output) do
    # get the index of a ligature set that might apply
    coverloc = find_coverage_index(coverage, g.glyph)

    {output, glyphs} =
      if coverloc != nil do
        # find first match in this ligature set (if any)
        lig =
          Enum.find(Enum.at(ligatures, coverloc), fn {_replacement, match} ->
            match_ligature_rule?(glyphs, match, flag, gdef, mfs)
          end)

        if lig != nil do
          # IO.puts "LIG: Match #{inspect lig} for #{inspect g}"
          # replace the current glyph
          {rep, m} = lig
          # some kind of acc for skipped glyphs so they can be placed after ligature
          # with appropriate component_i_ds and mark deltas
          # ie; skip marks
          matched =
            [g | glyphs]
            |> lig_components(length(m) + 1, flag, gdef, mfs)
            |> Stream.map(fn {x, _} -> x.codepoints end)
            |> Enum.concat()

          {_, n_total} =
            glyphs
            |> lig_components(length(m), flag, gdef, mfs)
            |> Enum.to_list()
            |> List.last()

          replacement = %{g | glyph: rep, codepoints: matched, is_ligature: true}
          # skip over any matched glyphs
          {skipped, last_component} =
            if n_total != length(m) do
              IO.puts("LIG: Match #{length(m)} glyphs out of #{n_total}")
              # take n_total; if skipped set m_component else increment comp_id
              # TODO: we should only set m_lig_component when is mark; need to review
              {markss, lastcomp_id} =
                glyphs
                |> Stream.take(n_total)
                |> Enum.map_reduce(1, fn g, comp_id ->
                  if should_skip_glyph?(g.glyph, flag, gdef, mfs),
                    do: {%{g | m_lig_component: comp_id}, comp_id},
                    else: {g, comp_id + 1}
                end)

              {markss |> Enum.filter(&should_skip_glyph?(&1.glyph, flag, gdef, mfs)), lastcomp_id}
            else
              {[], length(m) + 1}
            end

          # probably want to prepend earlier ignored glyphs to remaining
          {rmarks, remaining} =
            Enum.slice(glyphs, n_total, length(glyphs))
            |> Enum.split_while(&should_skip_glyph?(&1.glyph, flag, gdef, mfs))

          rmarks = rmarks |> Enum.map(fn m -> %{m | m_lig_component: last_component} end)
          # for remaining while mark set comp_id
          {[replacement | output], skipped ++ rmarks ++ remaining}
        else
          {[g | output], glyphs}
        end
      else
        {[g | output], glyphs}
      end

    apply_ligature(coverage, ligatures, flag, gdef, mfs, glyphs, output)
  end

  defp apply_context_sub1(_coverage, _rulesets, _lookups, [], output), do: output

  defp apply_context_sub1(coverage, rulesets, lookups, [g | glyphs], output) do
    coverloc = find_coverage_index(coverage, g.glyph)

    {o, glyphs} =
      if coverloc != nil do
        ruleset = Enum.at(rulesets, coverloc)
        # find first match in this ruleset
        # TODO: flag might mean we need to filter ignored categories
        # ie; skip marks
        rule =
          Enum.find(ruleset, fn {input, _} ->
            Enum.take(glyphs, length(input)) |> Enum.map(& &1.glyph) == input
          end)

        if rule != nil do
          {matched, subst_records} = rule
          input = [g | Enum.take(glyphs, length(matched))]
          # IO.puts "GSUB5.1 rule = #{inspect input}"
          replaced =
            subst_records
            |> Enum.reduce(input, fn {input_loc, lookup_index}, acc ->
              candidate = Enum.at(acc, input_loc)
              lookup = Enum.at(lookups, lookup_index)
              [replacement | _] = parse_and_apply(lookup, nil, lookups, nil, [candidate])
              # IO.puts "GSUB5.1 replace = #{inspect candidate} -> #{inspect replacement}"
              List.replace_at(acc, input_loc, replacement)
            end)

          # skip over any matched glyphs
          # TODO: handle flags correctly
          # probably want to prepend earlier ignored glyphs to remaining
          # IO.puts "GSUB5.1 replaced = #{inspect replaced}"
          remaining = Enum.slice(glyphs, length(matched), length(glyphs))
          {replaced, remaining}
        else
          {[g], glyphs}
        end
      else
        {[g], glyphs}
      end

    output = output ++ o
    apply_context_sub1(coverage, rulesets, lookups, glyphs, output)
  end

  # class-based context
  defp apply_context_sub2(_coverage, _rulesets, _classes, _lookups, [], output), do: output

  defp apply_context_sub2(coverage, rulesets, classes, lookups, [g | glyphs], output) do
    coverloc = find_coverage_index(coverage, g.glyph)
    ruleset = Enum.at(rulesets, classify_glyph(g.glyph, classes))

    {o, glyphs} =
      if coverloc != nil and ruleset != nil do
        # Logger.debug "GSUB5.2 rule = #{inspect ruleset}"
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
          input = [g | Enum.take(glyphs, length(matched))]

          replaced =
            subst_records
            |> Enum.reduce(input, fn {input_loc, lookup_index}, acc ->
              candidate = Enum.at(acc, input_loc)
              lookup = Enum.at(lookups, lookup_index)
              [replacement | _] = parse_and_apply(lookup, nil, lookups, nil, [candidate])
              List.replace_at(acc, input_loc, replacement)
            end)

          # skip over any matched glyphs
          # TODO: handle flags correctly
          # probably want to prepend earlier ignored glyphs to remaining
          remaining = Enum.slice(glyphs, length(matched), length(glyphs))
          {replaced, remaining}
        else
          {[g], glyphs}
        end
      else
        {[g], glyphs}
      end

    output = output ++ o
    apply_context_sub2(coverage, rulesets, classes, lookups, glyphs, output)
  end

  # handle class-based format for context chaining
  defp apply_chaining_context_sub2(_coverage, _rulesets, _classes, _lookups, [], output),
    do: output

  defp apply_chaining_context_sub2(
         coverage,
         rulesets,
         {bt_classes, input_classes, la_classes},
         lookups,
         [g | glyphs],
         output
       ) do
    coverloc = find_coverage_index(coverage, g.glyph)
    ruleset = Enum.at(rulesets, classify_glyph(g.glyph, input_classes))

    {o, glyphs} =
      if coverloc != nil and ruleset != nil do
        # find first match
        rule =
          Enum.find(ruleset, fn rule ->
            does_ccs2match(rule, {bt_classes, input_classes, la_classes}, output, g, glyphs)
          end)

        # apply substitutions to input
        if rule != nil do
          {_, im, _, subst_records} = rule
          input_extra = length(im) - 1
          input = [g | Enum.take(glyphs, input_extra)]

          replaced =
            subst_records
            |> Enum.reduce(input, fn {input_loc, lookup_index}, acc ->
              candidate = Enum.at(acc, input_loc)
              lookup = Enum.at(lookups, lookup_index)
              [replacement | _] = parse_and_apply(lookup, nil, lookups, nil, [candidate])
              List.replace_at(acc, input_loc, replacement)
            end)

          # Logger.debug "GSUB6.2 rule = #{inspect input} =>  #{inspect replaced}"
          {replaced, Enum.drop(glyphs, input_extra)}
        else
          {[g], glyphs}
        end
      else
        {[g], glyphs}
      end

    output = output ++ o

    apply_chaining_context_sub2(
      coverage,
      rulesets,
      {bt_classes, input_classes, la_classes},
      lookups,
      glyphs,
      output
    )
  end

  defp does_ccs2match(rule, {bt_classes, input_classes, la_classes}, output, g, glyphs) do
    {bt_match, input_match, la_match, _} = rule
    backtrack = length(bt_match)
    input_extra = length(input_match) - 1
    lookahead = length(la_match)

    # not enough backtracking or lookahead to even attempt match
    if length(output) < backtrack or length(glyphs) < lookahead + input_extra do
      false
    else
      # do we match the input
      input = [g] ++ Enum.take(glyphs, input_extra)

      input_matches =
        input
        |> Enum.zip(input_match)
        |> Enum.all?(fn {g, class} -> classify_glyph(g.glyph, input_classes) == class end)

      # do we match backtracking?
      back_matches =
        if backtrack > 0 do
          output
          |> Enum.reverse()
          |> Enum.take(backtrack)
          |> Enum.zip(bt_match)
          |> Enum.all?(fn {g, class} -> classify_glyph(g.glyph, bt_classes) == class end)
        else
          true
        end

      # do we match lookahead
      la_matches =
        if lookahead > 0 do
          glyphs
          |> Enum.drop(input_extra)
          |> Enum.take(lookahead)
          |> Enum.zip(la_match)
          |> Enum.all?(fn {g, class} -> classify_glyph(g.glyph, la_classes) == class end)
        else
          true
        end

      input_matches and back_matches and la_matches
    end
  end

  # handle reverse context chaining (add to output head instead of tail)
  defp apply_reverse_chaining_context(
         _bt_coverage,
         _coverage,
         _la_coverage,
         _subst,
         _,
         [],
         output
       ),
       do: output

  defp apply_reverse_chaining_context(
         bt_coverage,
         coverage,
         la_coverage,
         subst,
         lookups,
         glyphs,
         output
       ) do
    backtrack = length(bt_coverage)
    lookahead = length(la_coverage)
    # we iterate through glyphs backwards
    # grow output at head while shrinking preceding glyphs
    # current is last glyph in list
    g = List.last(glyphs)

    # backtracking is preceding glyphs
    preceding = Enum.slice(glyphs, 0..-2)
    # lookahead is following glyphs (output)

    # enough backtracking or lookahead to even attempt match?
    replacement =
      if length(preceding) < backtrack or length(output) < lookahead do
        g
      else
        # do we match input gylph?
        coverloc = find_coverage_index(coverage, g.glyph)
        input_matches = coverloc != nil

        # do we match backtracking?
        back_matches =
          if backtrack > 0 do
            preceding
            |> Enum.reverse()
            |> Enum.take(backtrack)
            |> Enum.zip(bt_coverage)
            |> Enum.all?(fn {g, cov} -> find_coverage_index(cov, g.glyph) != nil end)
          else
            true
          end

        # do we match lookahead?
        la_matches =
          if lookahead > 0 do
            output
            |> Enum.take(lookahead)
            |> Enum.zip(la_coverage)
            |> Enum.all?(fn {g, cov} -> find_coverage_index(cov, g.glyph) != nil end)
          else
            true
          end

        if input_matches and back_matches and la_matches do
          %{g | glyph: Enum.at(subst, coverloc)}
        else
          g
        end
      end

    output = [replacement | output]

    apply_reverse_chaining_context(
      bt_coverage,
      coverage,
      la_coverage,
      subst,
      lookups,
      preceding,
      output
    )
  end

  # handle coverage-based format for context chaining
  defp apply_chaining_context_sub3(
         _bt_coverage,
         _coverage,
         _la_coverage,
         _subst,
         _,
         [],
         _pga,
         output
       ),
       do: output

  defp apply_chaining_context_sub3(
         bt_coverage,
         coverage,
         la_coverage,
         subst_records,
         lookups,
         [g | glyphs],
         tag,
         output
       ) do
    backtrack = length(bt_coverage)
    input_extra = length(coverage) - 1
    lookahead = length(la_coverage)

    oo =
      cond do
        # not enough backtracking or lookahead to even attempt match
        length(output) < backtrack or length(glyphs) < lookahead + input_extra ->
          [g]

        # filter to only process when tag applies for current glyph
        tag != nil and g.tag != tag ->
          [g]

        true ->
          # do we match the input
          input = [g] ++ Enum.take(glyphs, input_extra)

          input_matches =
            input
            |> Enum.zip(coverage)
            |> Enum.all?(fn {g, cov} -> find_coverage_index(cov, g.glyph) != nil end)

          # do we match backtracking?
          back_matches =
            if backtrack > 0 do
              output
              |> Enum.reverse()
              |> Enum.take(backtrack)
              |> Enum.zip(bt_coverage)
              |> Enum.all?(fn {g, cov} -> find_coverage_index(cov, g.glyph) != nil end)
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
              |> Enum.all?(fn {g, cov} -> find_coverage_index(cov, g.glyph) != nil end)
            else
              true
            end

          if input_matches and back_matches and la_matches do
            replaced =
              subst_records
              |> Enum.reduce(input, fn {input_loc, lookup_index}, acc ->
                candidate = Enum.at(acc, input_loc)
                lookup = Enum.at(lookups, lookup_index)
                [replacement | _] = parse_and_apply(lookup, nil, lookups, nil, [candidate])
                List.replace_at(acc, input_loc, replacement)
              end)

            replaced
          else
            [g]
          end
      end

    output = output ++ oo

    apply_chaining_context_sub3(
      bt_coverage,
      coverage,
      la_coverage,
      subst_records,
      lookups,
      glyphs,
      tag,
      output
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

  # TODO: does this make sense in regard to GSUB? indices might not be sufficient if not one-to-one SUB!
  def filter_glyphs(glyphs, flag, gdef, mfs) do
    skipped =
      glyphs
      |> Enum.with_index()
      |> Enum.filter(fn {g, _} -> should_skip_glyph?(g.glyph, flag, gdef, mfs) end)

    filtered =
      glyphs
      |> Enum.reject(fn g -> should_skip_glyph?(g.glyph, flag, gdef, mfs) end)

    {filtered, skipped}
  end

  def unfilter_glyphs(glyphs, skipped) do
    # Logger.debug("unfilter #{inspect glyphs}, #{inspect skipped}")
    skipped
    |> Enum.reduce(glyphs, fn {g, i}, acc -> List.insert_at(acc, i, g) end)
  end

  # returns true when the lookup flag is set to a value
  def should_skip_glyph?(g, flag, gdef, mfs) do
    # decompose the flag
    <<attachment_type::8, _::3, use_mark_filtering_set::1, ignore_mark::1, ignore_lig::1,
      ignore_base::1, _rtl::1>> = <<flag::16>>

    cond do
      # short circuit - if no flags, we aren't skipping anything
      flag == 0 ->
        false

      # skip if ignore is set and we match the corresponding GDEF class
      ignore_base == 1 and classify_glyph(g, gdef.classes) == 1 ->
        true

      ignore_lig == 1 and classify_glyph(g, gdef.classes) == 2 ->
        true

      ignore_mark == 1 and classify_glyph(g, gdef.classes) == 3 ->
        true

      # skip if mark not in mark glyph set
      use_mark_filtering_set == 1 and classify_glyph(g, gdef.classes) == 3 and
          find_coverage_index(g, Enum.at(gdef.mark_sets, mfs)) == nil ->
        true

      # skip if mark doesn't match a non-zero attachment type
      attachment_type != 0 and classify_glyph(g, gdef.classes) == 3 and
          classify_glyph(g, gdef.attachments) != attachment_type ->
        true

      # default is DO NOT SKIP
      true ->
        false
    end
  end
end
