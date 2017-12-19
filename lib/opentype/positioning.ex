defmodule OpenType.Positioning do
  alias OpenType.Parser
  require Logger


  # add two positions together, treat nils as zeroed structure
  # category std_width, kern, pos can be used to optimize PDF output
  defp addPos(p, nil), do: p
  defp addPos(nil, {a,b,c,d}), do: {:pos, a, b, c, d}
  defp addPos(p, {0,0,0,0}), do: p
  defp addPos({:std_width, a,b,c,d}, {0,0,g,0}), do: {:kern, a, b, c+g, d}
  defp addPos({:std_width, a,b,c,d}, {e,f,g,h}), do: {:pos, a+e, b+f, c+g, d+h}
  defp addPos({type, a,b,c,d}, {e,f,g,h}), do: {type, a+e, b+f, c+g, d+h}
  
  # ==============================================
    # GPOS glyph positioning
    # Used for kerning, optical alignment, diacratics, etc
    # if a lookup type is as-yet unsupported
    # simply passes through the input
  # ==============================================

  # parse a lookup table
  def parse_lookup_table(index, lookups) do
    lookup = lookups |> Enum.at(index)
    parsed = case lookup do
      # extended lookup table, need to return actual lookup type
      {9, flag, offsets, table, mfs} -> 
        {actual_type, output} = parse_lookup(9, offsets, table)
        {:parsed, actual_type, flag, mfs, output}
      # standard lookup table
      {n, flag, offsets, table, mfs} -> {:parsed, n, flag, mfs, parse_lookup(n, offsets, table)}
      # already parsed (or unparsable), ignore
      _ -> lookup
    end

    # replace with parsed content and return
    if parsed != lookup do
      lookups |> List.replace_at(index, parsed)
    else
      lookups
    end
  end

  defp parse_and_apply({:parsed, type, flag, mfs, data}, gdef, lookups, isRTL, input) do
    apply_lookup({:parsed, type, flag, mfs, data}, gdef, lookups, isRTL, input)
  end

  defp parse_and_apply({9, flag, offsets, data, mfs}, gdef, lookups, isRTL, input) do
    {actual_type, output} = parse_lookup(9, offsets, data)
    val = {:parsed, actual_type, flag, mfs, output}
    apply_lookup(val, gdef, lookups, isRTL, input)
  end

  defp parse_and_apply({type, flag, offsets, data, mfs}, gdef, lookups, isRTL, input) do
    val = {:parsed, type, flag, mfs, parse_lookup(type, offsets, data)}
    apply_lookup(val, gdef, lookups, isRTL, input)
  end

  def apply_lookup({:parsed, type, flag, mfs, data}, gdef, lookups, isRTL, {glyphs, pos}) when is_list(data) do
    Enum.reduce(data, {glyphs, pos}, fn (subdata, input) -> apply_lookup({:parsed, type, flag, mfs, subdata}, gdef, lookups, isRTL, input) end)
  end

  def apply_lookup({:parsed, 1, _flag, _mfs, data}, _gdef, _lookups, _isRTL, {glyphs, pos}) do
    {fmt, coverage, values} = data

    adjusted = case fmt do
    1 ->
      Enum.map(glyphs, fn g ->
        coverloc = findCoverageIndex(coverage, g.glyph)
        if coverloc != nil, do: values, else: nil
      end)
    2 ->
      Enum.map(glyphs, fn g ->
        coverloc = findCoverageIndex(coverage, g.glyph)
        if coverloc != nil, do: Enum.at(values, coverloc), else: nil
      end)
    end
    positioning = Enum.zip(pos, adjusted) |> Enum.map(fn {v1, v2} -> addPos(v1,v2) end)
    {glyphs, positioning}
  end

  def apply_lookup({:parsed, 2, _flag, _mfs, data}, _gdef, _lookups, _isRTL, {glyphs, pos}) do
    {fmt, values} = data
    kerning = case fmt do
      1 ->
        {coverage, pairSets} = values
        applyKerning(coverage, pairSets, glyphs, [])
      2 ->
        #apply the kerning
        #classify both glyphs
        #get pairSet[c1][c2]
        #position
        {class1, class2, pairSets} = values
        applyKerning2(class1, class2, pairSets, glyphs, [])
    end
    positioning = Enum.zip(pos, kerning) |> Enum.map(fn {v1, v2} -> addPos(v1,v2) end)
    {glyphs, positioning}
  end

  def apply_lookup({:parsed, 3, flag, mfs, data}, gdef, _lookups, isRTL, {glyphs, pos}) do
    {coverage, anchorPairs} = data

    # filter the glyphs
    g = filter_glyphs(glyphs, flag, gdef, mfs) |> Enum.to_list

    # align entry/exit points
    {p, d_glyphs} = applyCursive(coverage, anchorPairs, flag, mfs, gdef, isRTL, [], g, pos)

    g2 = d_glyphs
         |> Enum.reduce(glyphs, fn ({x, i}, acc) -> if Enum.at(acc, i).cursiveDelta == 0, do: List.replace_at(acc, i, x), else: acc end)

    {g2, p}
  end
  def apply_lookup({:parsed, 4, flag, mfs, data}, gdef, _lookups, _isRTL, {glyphs, pos}) do
    {markCoverage, baseCoverage, baseArray, markArray} = data

    # filter the glyphs
    g = filter_glyphs(glyphs, flag, gdef, mfs) |> Enum.to_list

    # align attachment points
    {adjusted, d_glyphs} = applyMarkToBase(markCoverage, baseCoverage, 
                               baseArray, markArray, 
                               # skip flags and GDEF info
                               flag, mfs, gdef, 
                               [], g, pos)

    g2 = d_glyphs
         |> Enum.reduce(glyphs, fn ({x, i}, acc) -> if Enum.at(acc, i).markDelta == 0, do: List.replace_at(acc, i, x), else: acc end)

    # apply the adjustments to positioning
    # positioning = Enum.zip(pos, adjusted) |> Enum.map(fn {v1, v2} -> addPos(v1,v2) end)
    {g2, adjusted}
  end
  def apply_lookup({:parsed, 5, _flag, _mfs, data}, _gdef, _lookups, _isRTL, {glyphs, pos}) do
    {_markCoverage, _baseCoverage, _baseArray, _markArray} = data

    Logger.debug "GPOS 5 - mark to ligature"
    # for this to work, we need to know which ligature component to
    # attach the mark to -- needs to be set during GSUB 4 processing!
    # in the absense of such info we could work backwards through the components
    # until we find one with an attachment point for the current mark class
    # see https://bugzilla.gnome.org/show_bug.cgi?id=437633 for a torture test
    # where a 'calt' liga + subsequent 'liga' moves target component for
    # mark that is itself a ligature!
    # TODO: we now have a struct that capture this for us!
    {glyphs, pos}
  end
  def apply_lookup({:parsed, 6, _flag, _mfs, data}, _gdef, _lookups, _isRTL, {glyphs, pos}) do
    # same as format 4, except "base" is another mark
    {_markCoverage, _baseCoverage, _baseArray, _markArray} = data

    # adjusted = applyMarkToBase(markCoverage, baseCoverage, baseArray, markArray, flag, mfs, gdef, [hd(glyphs)], tl(glyphs), pos)
    #Logger.debug "MKMK #{inspect glyphs} #{inspect adjusted}"
    # positioning = Enum.zip(pos, adjusted) |> Enum.map(fn {v1, v2} -> addPos(v1,v2) end)
    # {glyphs, positioning, c, m}
    {glyphs, pos}
  end
  def apply_lookup({:parsed, 7, _flag, _mfs, data}, gdef, lookups, _isRTL, {glyphs, pos}) do
    {format, val} = data
    pos = case format do
      1 ->
        Logger.debug "GPOS 7.1 - contextual positioning"
        pos
      2 ->
        {coverage, rulesets, classes} = val
        _positioning = applyContextPos2(coverage, rulesets, classes, gdef, lookups, glyphs, pos, [])
        pos
      3 ->
        Logger.debug "GPOS 7.3 - contextual positioning"
        pos
      _ ->
        Logger.debug "GPOS 7 - contextual positioning format #{format}"
        pos
    end
    {glyphs, pos}
  end
  def apply_lookup({:parsed, 8, flag, mfs, data}, gdef, lookups, _isRTL, {glyphs, pos}) do
    {format, val} = data
    pos = case format do
      1 ->
        Logger.debug "GPOS 8.1 - chained contextual positioning"
        pos
      2 ->
        Logger.debug "GPOS 8.2 - chained contextual positioning"
        pos
      3 ->
        {btCoverage, coverage, laCoverage, posRecords} = val

        g = filter_glyphs(glyphs, flag, gdef, mfs) |> Enum.to_list
        applyChainingContextPos3(btCoverage, coverage, laCoverage, posRecords, gdef, lookups, g, pos, [])
      _ ->
        Logger.debug "GPOS 8 - chained contextual positioning format #{format}"
        pos
    end
    {glyphs, pos}
  end

  def parse_lookup(9, offsets, data) do
    # GPOS type 9 -- extended table
    subtables = offsets
            |> Enum.map(fn x ->
            <<1::16, lt::16, off::32>> = binary_part(data, x, 8)
            {lt, Parser.subtable(data, x + off)}
              end)
    # spec says extended type is identical for all subtables
    {actual_type, _} = hd(subtables)
    output = subtables
             |> Enum.map(fn {type, table} -> parse_lookup(type, table) end)
    {actual_type, output}
  end

  # parse a lookup (which is expected to have multiple subtables)
  def parse_lookup(type, offsets, data) do
    offsets
    |> Enum.map(fn o -> parse_lookup(type, Parser.subtable(data, o)) end)
  end


  def parse_lookup(1, data) do
    <<fmt::16, covOff::16, valueFormat::16, rest::binary>> = data
    coverage = Parser.parseCoverage(Parser.subtable(data, covOff))
    valSize = Parser.valueRecordSize(valueFormat)
    values = case fmt do
    1 ->
      <<val::binary-size(valSize), _::binary>> = rest
      Parser.readPositioningValueRecord(valueFormat, val)
    2 ->
      <<nVals::16, _::binary>> = rest
      recs = binary_part(rest, 16, nVals * valSize)
      for << <<val::binary-size(valSize)>> <- recs >>, do: Parser.readPositioningValueRecord(valueFormat, val)
    end
    {fmt, coverage, values}
  end

  def parse_lookup(2, data) do
    <<fmt::16, covOff::16, record1::16, record2::16, rest::binary>> = data
    kerning = case fmt do
      1 ->
        # FMT 1 - identifies individual glyphs
        # pair set table
        <<nPairs::16, pairOff::binary-size(nPairs)-unit(16), _::binary>> = rest
        pairsetOffsets = for << <<x::16>> <- pairOff >>, do: x
        # coverage table
        coverage = Parser.parseCoverage(Parser.subtable(data, covOff))
        # parse the pair sets
        pairSets = Enum.map(pairsetOffsets, fn off -> Parser.parsePairSet(data, off, record1, record2) end)
        {coverage, pairSets}
      2 ->
        #FMT 2
        # offset to classdef, offset to classdef
        # nClass1Records, nClass2Records
        <<class1Off::16, class2Off::16, nClass1Records::16, nClass2Records::16, records::binary>> = rest

        #read in the class definitions
        class1 = Parser.parseGlyphClass(Parser.subtable(data, class1Off))
        class2 = Parser.parseGlyphClass(Parser.subtable(data, class2Off))
        # fmt==1, startglyph, nglyphs, array of ints (each int is a class)
        # fmt==2, nRanges, {startGlyph, endGlyph, class}

        #read in the actual positioning pairs
        sizeA = Parser.valueRecordSize(record1)
        sizeB = Parser.valueRecordSize(record2)
        class2size = sizeA + sizeB
        class1size = nClass2Records * class2size
        c1recs = binary_part(records, 0, nClass1Records * class1size)
        c1Recs = for << <<c2recs::binary-size(class1size)>> <- c1recs >>, do: c2recs
        pairSets = Enum.map(c1Recs, fn c2recs ->
          c2Recs = for << <<c2Rec::binary-size(class2size)>> <- c2recs>>, do: c2Rec
          c2Recs
          |> Enum.map(fn c2Rec -> for << <<v1::binary-size(sizeA), v2::binary-size(sizeB)>> <- c2Rec >>, do: {v1, v2} end)
          |> Enum.map(fn [{v1, v2}] -> {Parser.readPositioningValueRecord(record1, v1), Parser.readPositioningValueRecord(record2, v2)} end)
        end)

        {class1, class2, pairSets}
    end
    {fmt, kerning}
  end

  def parse_lookup(3, data) do
    <<_fmt::16, coverageOff::16, nAnchorPairs::16, nrecs::binary-size(nAnchorPairs)-unit(32), _::binary>> = data
    coverage = Parser.parseCoverage(Parser.subtable(data, coverageOff))
    records = for << <<entryAnchor::16, exitAnchor::16>> <- nrecs >>, do: {entryAnchor, exitAnchor}
    anchorPairs = records
                  |> Enum.map(fn {entryAnchor, exitAnchor} ->
                    entryAnchor = if entryAnchor != 0, do: Parser.parseAnchor(Parser.subtable(data, entryAnchor)), else: nil
                    exitAnchor = if exitAnchor != 0, do: Parser.parseAnchor(Parser.subtable(data, exitAnchor)), else: nil
                    {entryAnchor, exitAnchor}
                  end)
    {coverage, anchorPairs}
  end

  def parse_lookup(4, data) do
    <<_fmt::16, markCoverageOff::16, baseCoverageOff::16, nClasses::16, 
    markArrayOffset::16, baseArrayOffset::16, _::binary>> = data
    
    # coverage definitions
    markCoverage = Parser.parseCoverage(Parser.subtable(data, markCoverageOff))
    baseCoverage = Parser.parseCoverage(Parser.subtable(data, baseCoverageOff))

    # baseArray table
    baseTbl = Parser.subtable(data, baseArrayOffset)
    <<nRecs::16, records::binary>> = baseTbl
    # 2 bytes per class
    recordSize = nClasses * 2
    records = binary_part(records, 0, nRecs * recordSize)
    records = for << <<record::binary-size(recordSize)>> <- records >>, do: record
    # each record is array of offsets
    baseArray = records
              |> Enum.map(fn r -> for << <<offset::16>> <- r>>, do: offset end)
              |> Enum.map(&Enum.map(&1, fn o -> 
                                Parser.parseAnchor(Parser.subtable(baseTbl, o)) end) 
                        )

    # markArray table
    markArrayTbl = Parser.subtable(data, markArrayOffset)
    markArray = Parser.parseMarkArray(markArrayTbl)

    {markCoverage, baseCoverage, baseArray, markArray}
  end

  def parse_lookup(5, data) do
    # same as format 4, except "base" is a ligature with (possibly) multiple anchors
    <<_fmt::16, markCoverageOff::16, baseCoverageOff::16, nClasses::16, 
    markArrayOffset::16, baseArrayOffset::16, _::binary>> = data

    markCoverage = Parser.parseCoverage(Parser.subtable(data, markCoverageOff))
    baseCoverage = Parser.parseCoverage(Parser.subtable(data, baseCoverageOff))

    markArrayTbl = Parser.subtable(data, markArrayOffset)
    markArray = Parser.parseMarkArray(markArrayTbl)
    # base array table
    baseTbl = Parser.subtable(data, baseArrayOffset)
    <<nRecs::16, records::binary-size(nRecs)-unit(16), _::binary>> = baseTbl
    # array of offsets to ligature attach tables
    la = for << <<off::16>> <- records >>, do: Parser.subtable(baseTbl, off)
    componentSize = nClasses * 2
    # each component is array of offsets (size == size of mark array) to anchor tables
    #  -- one for each mark class including class 0; may be NULL
    baseArray = la
    |> Enum.map(fn laTbl ->
      <<nComponents::16, recs::binary>> = laTbl
      recs = binary_part(recs, 0, nComponents * componentSize)
      comps = for << <<record::binary-size(componentSize)>> <- recs >>, do: record
      comps
      |> Enum.map(fn r -> for << <<offset::16>> <- r>>, do: offset end)
      |> Enum.map(&Enum.map(&1, fn o -> 
      if o != 0, do: Parser.parseAnchor(binary_part(laTbl, o, 6)), else: nil
      end))
      end)
    {markCoverage, baseCoverage, baseArray, markArray}
  end

  def parse_lookup(6, data) do
    # same format at GPOS 4
    parse_lookup(4, data)
  end

  def parse_lookup(7, data) do
    <<format::16, details::binary>> = data
    val = case format do
      1 ->
        Logger.debug "GPOS 7.1 - contextual positioning"
        nil
      2 ->
        <<covOff::16, 
          classDefOff::16,
          nRulesets::16, 
          srsOff::binary-size(nRulesets)-unit(16),
          _::binary>> = details
        coverage = Parser.parseCoverage(Parser.subtable(data, covOff))
        classes = Parser.parseGlyphClass(Parser.subtable(data, classDefOff))
        srs = for << <<offset::16>> <- srsOff >>, do: if offset != 0, do: Parser.subtable(data, offset), else: nil
        rulesets =  srs
                    |> Enum.map(fn ruleset ->
                      if ruleset != nil do
                        <<nRules::16, srOff::binary-size(nRules)-unit(16), _::binary>> = ruleset
                        rules = for << <<offset::16>> <- srOff >>, do: Parser.subtable(ruleset, offset)
                        rules |> Enum.map(&Parser.parseContextSubRule1(&1))
                      else
                        nil
                      end
                    end)
        {coverage, rulesets, classes}
      3 ->
        Logger.debug "GPOS 7.3 - contextual positioning"
        nil
      _ ->
        Logger.debug "GPOS 7 - contextual positioning format #{format}"
        nil
    end
    {format, val}
  end

  def parse_lookup(8, data) do
    <<format::16, details::binary>> = data
    val = case format do
      1 ->
        Logger.debug "GPOS 8.1 - chained contextual positioning"
        nil
      2 ->
        Logger.debug "GPOS 8.2 - chained contextual positioning"
        nil
      3 ->
        <<backtrackCount::16, backoff::binary-size(backtrackCount)-unit(16),
          inputCount::16, inputOff::binary-size(inputCount)-unit(16),
          lookaheadCount::16, lookaheadOff::binary-size(lookaheadCount)-unit(16),
          substCount::16, substRecs::binary-size(substCount)-unit(32),
          _::binary>> = details
        
        # parse the coverage tables and positioning records  
        backOffsets = for << <<x::16>> <- backoff >>, do: x
        btCoverage = Enum.map(backOffsets, fn covOff -> Parser.parseCoverage(Parser.subtable(data, covOff)) end)
        inputOffsets = for << <<x::16>> <- inputOff >>, do: x
        coverage = Enum.map(inputOffsets, fn covOff -> Parser.parseCoverage(Parser.subtable(data, covOff)) end)
        lookaheadOffsets = for << <<x::16>> <- lookaheadOff >>, do: x
        laCoverage = Enum.map(lookaheadOffsets, fn covOff -> Parser.parseCoverage(Parser.subtable(data, covOff)) end)

        # index x at which to apply lookup y
        posRecords = for << <<x::16, y::16>> <- substRecs >>, do: {x, y}

        {btCoverage, coverage, laCoverage, posRecords}
      _ ->
        Logger.debug "GPOS 8 - chained contextual positioning format #{format}"
        nil
    end
    {format, val}
  end

  defp applyMarkToBase(_markCoverage, _baseCoverage, _baseArray, _markArray, _lookupFlag, _mfs, _gdef, prev, [], pos), do: {pos, prev}
  defp applyMarkToBase(markCoverage, baseCoverage, baseArray, markArray, lookupFlag, mfs, gdef, [], [{g, gi} | glyphs], pos) do
    applyMarkToBase(markCoverage, baseCoverage, baseArray, markArray, lookupFlag, mfs, gdef, [{g, gi}], glyphs, pos)
  end
  defp applyMarkToBase(markCoverage, baseCoverage, baseArray, markArray, lookupFlag, mfs, gdef, prev, [{g, gi} | glyphs], pos) do
    # should we skip this glyph?
    skipMark = should_skip_glyph(g.glyph, lookupFlag, gdef, mfs)
    markloc = findCoverageIndex(markCoverage, g.glyph)

    # TODO: this code assumes prev is a base
    {base_glyph, prev_i} = if gdef != nil do
      # find a base
      Enum.find(prev, fn {x, _} -> classifyGlyph(x.glyph, gdef.classes) == 1 end)
    else
      hd(prev)
    end

    baseloc = findCoverageIndex(baseCoverage, base_glyph.glyph)

    {mark_offset, delta} = if markloc != nil and baseloc != nil and !skipMark do
      b = Enum.at(baseArray, baseloc)
      {class, {mark_x, mark_y}} = Enum.at(markArray, markloc)
      {base_x, base_y} = Enum.at(b, class)
      # align the anchors
      {off_x, off_y} = {base_x - mark_x, base_y - mark_y}
      index_delta = gi - prev_i
      {{:pos, off_x, off_y, 0, 0}, -index_delta}
    else
      {Enum.at(pos, gi), 0}
    end
    updated = pos |> List.replace_at(gi, mark_offset)
    # for now avoid overwriting
    g = if g.markDelta == 0, do: %{g | markDelta: delta}, else: g
    applyMarkToBase(markCoverage, baseCoverage, baseArray, markArray, lookupFlag, mfs, gdef, [{g, gi} | prev], glyphs, updated)
  end

  defp applyKerning2(_classDef1, _clasDef2, _pairSets, [], output), do: output
  defp applyKerning2(_classDef1, _clasDef2, _pairSets, [_], output), do: output ++ [nil]
  defp applyKerning2(classDef1, classDef2, pairsets, [g1, g2 | glyphs], output) do
    c1 = classifyGlyph(g1.glyph, classDef1)
    c2 = classifyGlyph(g2.glyph, classDef2)
    pair = pairsets |> Enum.at(c1) |> Enum.at(c2)
    {output, glyphs} = if pair != nil do
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
    applyKerning2(classDef1, classDef1, pairsets, glyphs, output)
  end

  defp applyKerning(_coverage, _pairSets, [], output), do: output
  defp applyKerning(_coverage, _pairSets, [_], output), do: output ++ [nil]
  defp applyKerning(coverage, pairSets, [g | glyphs], output) do
    # get the index of a pair set that might apply
    coverloc = findCoverageIndex(coverage, g.glyph)
    {output, glyphs} = if coverloc != nil do
      pairSet = Enum.at(pairSets, coverloc)
      nextChar = hd(glyphs)
      pair = Enum.find(pairSet, fn {g, _, _} -> g == nextChar.glyph end)
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
    applyKerning(coverage, pairSets, glyphs, output)
  end

  defp applyCursive(_coverage, _anchorPairs, _flag, _mfs, _gdef, _isRTL, prev,  [], pos), do: {pos, prev}
  defp applyCursive(_coverage, _anchorPairs, _flag, _mfs, _gdef, _isRTL, prev, [x], pos), do: {pos, [x | prev]}
  defp applyCursive(coverage, anchorPairs, flag, mfs, gdef, isRTL, prev, [{g, gi}, {g2, gi2} | glyphs], pos) do
    # flag now ignored!
    # decompose the flag
    <<_attachmentType::8, _::3, _useMark::1, _ignoreMark::1, _ignoreLig::1, _ignoreBase::1, rtl::1>> = <<flag::16>>


    curloc = findCoverageIndex(coverage, g.glyph)
    nextloc = findCoverageIndex(coverage, g2.glyph)


    p = Enum.at(pos, gi)
    p2 = Enum.at(pos, gi2)

    # if glyphs are covered
    [cur, next, gdelta] = if curloc != nil and nextloc != nil do
      {entryA, _} = Enum.at(anchorPairs, nextloc)
      {_, exitA} = Enum.at(anchorPairs, curloc)
      if exitA != nil and entryA != nil do
        {entry_x, entry_y} = entryA
        {exit_x, exit_y} = exitA
        {_, xOff, _yOff, xAdv, yAdv} = p
        {_, x2Off, y2Off, x2Adv, y2Adv} = p2
        delta_y = if rtl, do: entry_y - exit_y, else: exit_y - entry_y
        index_delta = gi2 - gi


        if isRTL do
          delta_x = exit_x + xOff
          # post-process -- our yOffset needs to be adjusted by next yOffset
          # Logger.debug "GPOS 3 - cursive RTL delta #{inspect delta_x}, #{inspect delta_y}"
          [{:pos, xOff - delta_x, delta_y, xAdv - delta_x, yAdv}, {:pos, x2Off, y2Off, entry_x + x2Off, y2Adv}, index_delta]
        else
          delta_x = entry_x + x2Off
          # post-process -- next needs to adjust yOffset by our yOffset
          #Logger.debug "GPOS 3 - cursive LTR delta #{inspect delta_x}, #{inspect delta_y}"
          [{:pos, xOff, delta_y, exit_x + xOff, yAdv}, {:pos, x2Off - delta_x, y2Off, x2Adv - delta_x, y2Adv}, -index_delta]
        end
      else
        [p, p2, 0]
      end
    else
      [p, p2, 0]
    end
    
    updated = pos |> List.replace_at(gi, cur) |> List.replace_at(gi2, next)
    g = if g.cursiveDelta == 0, do: %{g | cursiveDelta: gdelta}, else: g

    applyCursive(coverage, anchorPairs, flag, mfs, gdef, isRTL, [{g, gi} | prev], [{g2, gi2} | glyphs], updated)
  end

  # adjust the y-offsets of connected glyphs
  def adjustCursiveOffset(positions, deltas) do
    0..length(deltas)-1
    |> Enum.reduce({positions, deltas}, fn (x, {p, d}) -> adjustCursiveOffsets(x, p, d) end)
  end

  # this is recursive since offset accumulates over a run of connected glyphs
  # Urdu in particular shows this in action
  def adjustCursiveOffsets(index, positions, deltas) do
    d = Enum.at(deltas, index)
    if d == 0 do
      {positions, deltas}
    else
      next = index + d
      {p2, d2} = adjustCursiveOffsets(next, positions, List.replace_at(deltas, index, 0))
      {type, xo, yo, xa, ya} = Enum.at(p2, index)
      {_, _, yo2, _, _} = Enum.at(p2, next)

      {List.replace_at(p2, index, {type, xo, yo + yo2, xa, ya}), d2}
    end

  end

  # adjust marks relative to base
  def adjustMarkOffsets(positions, deltas, is_rtl) do
    0..length(deltas)-1
    |> Enum.reduce({positions, deltas}, fn (x, {p, d}) -> adjustMarkOffsets(x, p, d, is_rtl) end)
  end

  def adjustMarkOffsets(index, positions, deltas, is_rtl) do
    d = Enum.at(deltas, index)
    if d == 0 do
      {positions, deltas}
    else
      base = index + d
      {_, box, boy, bax, bay} = Enum.at(positions, base)
      {type, mox, moy, _max, _may} = Enum.at(positions, index)
      adjusted = if is_rtl do
        {type, box + mox, boy + moy, 0, 0}
      else
        # LTR marks subtract the base advance
        {type, box + mox - bax, boy + moy - bay, 0, 0}
      end
      {List.replace_at(positions, index, adjusted), List.replace_at(deltas, index, 0)}
    end
  end

  # class-based context
  defp applyContextPos2(_coverage, _rulesets, _classes, _gdef, _lookups, [], [], output), do: output
  defp applyContextPos2(coverage, rulesets, classes, gdef, lookups, [g | glyphs], [p | pos], output) do
    coverloc = findCoverageIndex(coverage, g.glyph)
    ruleset = Enum.at(rulesets, classifyGlyph(g.glyph, classes))
    {o, glyphs, pos} = if coverloc != nil  and ruleset != nil do
      #find first match in this ruleset
      # TODO: flag might mean we need to filter ignored categories
      # ie; skip marks
      rule = Enum.find(ruleset, fn {input, _} ->
                        candidates = glyphs
                          |> Enum.take(length(input))
                          |> Enum.map(fn g -> classifyGlyph(g.glyph, classes) end)
                         candidates == input 
                       end)
      if rule != nil do
        Logger.debug "GPOS7.2 rule = #{inspect rule}"
        {matched, substRecords} = rule
        input = [g | Enum.take(glyphs, length(matched))]
                |> Enum.zip([p | Enum.take(pos, length(matched))])
        replaced = substRecords
        |> Enum.reduce(input, fn {inputLoc, lookupIndex}, acc ->
          {candidate, candidate_position} = Enum.at(acc, inputLoc)
          lookup = Enum.at(lookups, lookupIndex)
          {_, [adjusted_pos | _]} = parse_and_apply(lookup, gdef, lookups, nil, {[candidate], [candidate_position]})
          List.replace_at(acc, inputLoc, {candidate, adjusted_pos})
        end)
        # skip over any matched glyphs
        # TODO: handle flags correctly
        # probably want to prepend earlier ignored glyphs to remaining
        _remaining = Enum.slice(glyphs, length(matched), length(glyphs))
        _remaining_pos = Enum.slice(pos, length(matched), length(pos))
        Logger.debug("#{inspect input} => #{inspect replaced}")
        #{replaced, remaining, remaining_pos}
        {[nil], glyphs, pos}
      else
        {[nil], glyphs, pos}
      end
    else
      {[nil], glyphs, pos}
    end
    output = output ++ o
    applyContextPos2(coverage, rulesets, classes, gdef, lookups, glyphs, pos, output)
  end

  # handle coverage-based format for context chaining
  defp applyChainingContextPos3(_btCoverage, _coverage, _laCoverage, _subst, _gdef, _lookups, [], pos, _), do: pos
  defp applyChainingContextPos3(btCoverage, coverage, laCoverage, posRecords, gdef, lookups, [{g, index} | glyphs], pos, output) do
    backtrack = length(btCoverage)
    inputExtra = length(coverage) - 1
    lookahead = length(laCoverage)
    #not enough backtracking or lookahead to even attempt match
    oo = if length(output) < backtrack or length(glyphs) < lookahead + inputExtra do
      # positioning unchanged
      pos
    else

      #do we match the input
      input = [{g, index} | Enum.take(glyphs, inputExtra)]
      inputMatches = input
                     |> Enum.zip(coverage)
                     |> Enum.all?(fn {{g, _index}, cov} -> findCoverageIndex(cov, g.glyph) != nil end)

      #do we match backtracking?
      backMatches = if backtrack > 0 do
        output
        |> Enum.take(backtrack)
        |> Enum.zip(btCoverage)
        |> Enum.all?(fn {{g, _index}, cov} -> findCoverageIndex(cov, g.glyph) != nil end)
      else
        true
      end

      #do we match lookahead
      laMatches = if lookahead > 0 do
        glyphs
        |> Enum.drop(inputExtra)
        |> Enum.take(lookahead)
        |> Enum.zip(laCoverage)
        |> Enum.all?(fn {{g, _index}, cov} -> findCoverageIndex(cov, g.glyph) != nil end)
      else
        true
      end

      if inputMatches and backMatches and laMatches do
        posRecords
        |> Enum.reduce(pos, fn ({inputIndex, lookupIndex}, accPos) ->
          lookup = Enum.at(lookups, lookupIndex)
          {g, index} = Enum.at(input, inputIndex)
          candidate_position = Enum.at(accPos, index)
          # we only care about the new pos
          {_, [adjusted_pos | _]} = parse_and_apply(lookup, gdef, lookups, nil, {[g], [candidate_position]})
          List.replace_at(accPos, index, adjusted_pos)
        end)
      else
        pos
      end
    end
    # oo is the possibly updated positioning
    # output will be used for backtracking
    applyChainingContextPos3(btCoverage, coverage, laCoverage, posRecords, gdef, lookups, glyphs, oo, [{g, index} | output])
  end


  # given a glyph, find out the coverage index (can be nil)
  defp findCoverageIndex(cov, g) when is_integer(hd(cov)) do
    Enum.find_index(cov, fn i -> i == g end)
  end
  defp findCoverageIndex(cov, g) when is_tuple(hd(cov)) do
    r = Enum.find(cov, fn {f,l,_} -> f <= g and g <= l end)
    if r != nil do
      {s,_,i} = r
      i + g - s
    else
      nil
    end
  end
  # catch-all
  defp findCoverageIndex(_cov, _g) do
    nil
  end

  defp classifyGlyph(_g, nil), do: 0
  defp classifyGlyph(g, classes) when is_map(classes) do
    Map.get(classes, g, 0)
  end
  defp classifyGlyph(g, ranges) do
    r = Enum.find(ranges, fn {f,l,_} -> f <= g and g <= l end)
    # if no match, class 0
    if r == nil do
      0
    else
      {_,_,class} = r
      class
    end
  end

  # uses the lookup flag to determine which glyphs are ignored
  # this could be shared with GSUB implementation
  def should_skip_glyph(g, flag, gdef, mfs) do
    # decompose the flag
    <<attachmentType::8, _::3, useMarkFilteringSet::1, ignoreMark::1, ignoreLig::1, ignoreBase::1, _rtl::1>> = <<flag::16>>

    # only care about classification if flag set
    # if only RTL flag don't bother classifying
    glyphClass = if flag > 1 do
      classifyGlyph(g, gdef.classes)
    else
      0
    end

    cond do
      # short circuit - if no flags, we aren't skipping anything
      flag == 0 -> false
      # if only RTL flag not skipping anything
      flag == 1 -> false
      # skip if ignore is set and we match the corresponding GDEF class
      ignoreBase == 1 and glyphClass == 1 -> true
      ignoreLig  == 1 and glyphClass == 2 -> true
      ignoreMark == 1 and glyphClass == 3 -> true
      # skip if we have a mark that isn't in specified mark glyph set
      useMarkFilteringSet == 1 and classifyGlyph(g, gdef.classes) == 3 and findCoverageIndex(Enum.at(gdef.mark_sets, mfs), g) == nil -> true
      # skip if we don't match a non-zero attachment type
      attachmentType != 0 and classifyGlyph(g, gdef.attachments) != attachmentType -> true
      # default is DO NOT SKIP
      true -> false
    end
  end

  # then we use / save {pos, index}
  # returns array of {glyph, index} of glyphs that are not ignored (according to lookup flag)
  def filter_glyphs(glyphs, flag, gdef, mfs) do
    glyphs
    |> Stream.with_index
    |> Stream.reject(fn {g, _} -> should_skip_glyph(g.glyph, flag, gdef, mfs) end)
  end

end
