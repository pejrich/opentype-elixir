defmodule OpenType.Parser do
  @moduledoc false
  use Bitwise, only_operators: true
  require Logger

  # extract the TTF (or TTC) version
  def extract_version(ttf, <<version::size(32), data::binary>>) do
    {%{ttf | version: version}, data}
  end

  # read in the font header
  def read_header({%{version: v} = ttf, data}, _full)
      when v in [0x00010000, 0x74727565, 0x4F54544F] do
    <<num_tables::16, _search_range::16, _entry_selector::16, _range_shift::16,
      remainder::binary>> =
      data

    tables = read_tables(remainder, num_tables)
    is_cff = Enum.any?(tables, fn x -> x.name == "CFF " end)
    %{ttf | tables: tables, is_cff: is_cff}
  end

  def read_header({%{version: 0x74727566} = ttf, data}, full_data) do
    # TODO: read in TTC header info, subfont 0
    <<_ttc_version::32, num_subfonts::32, rem::binary>> = data
    # read in 32-bit subfont offsets
    {offsets, _remaining} = read_offset([], rem, num_subfonts)
    subfont = subtable(full_data, offsets[0])

    <<_ttf_version::32, num_tables::16, _search_range::16, _entry_selector::size(16),
      _range_shift::size(16), remainder::binary>> = subfont

    # IO.puts "Subfont 0 has #{num_tables} tables"
    tables = read_tables(remainder, num_tables)
    is_cff = Enum.any?(tables, fn x -> x.name == "CFF " end)
    %{ttf | tables: tables, is_cff: is_cff}
  end

  def read_header({ttf, _data}, _) do
    Logger.error("Unknown font format version")
    ttf
  end

  # raw data for a given table
  def raw_table(ttf, name, data) do
    t = Enum.find(ttf.tables, fn x -> x.name == name end)

    cond do
      t -> binary_part(data, t.offset, t.length)
      true -> nil
    end
  end

  # lookup a table by name
  def lookup_table(ttf, name) do
    t = Enum.find(ttf.tables, fn x -> x.name == name end)

    cond do
      t -> {t.offset, t.length}
      true -> nil
    end
  end

  # is there a particular font table?
  def has_table?(ttf, name) do
    Enum.any?(ttf.tables, fn x -> x.name == name end)
  end

  # use this when the length of the actual subtable is unknown
  def subtable(table, offset) do
    binary_part(table, offset, byte_size(table) - offset)
  end

  # read in the name table and select a name
  def extract_name(ttf, data) do
    raw = raw_table(ttf, "name", data)

    if raw do
      <<_fmt::16, n_records::16, str_offset::16, r::binary>> = raw
      # IO.puts "Name table format #{fmt}"
      recs = read_name_records([], r, n_records)
      # pick best supported platform/encoding
      selected =
        recs
        |> Enum.map(fn r -> {r.platform, r.encoding} end)
        |> find_preferred_encoding

      recs =
        if selected != nil do
          Enum.filter(recs, fn r -> {r.platform, r.encoding} == selected end)
        else
          recs
        end

      # and just parse that one
      names = Enum.map(recs, fn r -> record_to_name(r, str_offset, raw) end)
      # prefer PS name
      name6 =
        case Enum.find(names, fn {id, _} -> id == 6 end) do
          {_, val} -> val
          _ -> nil
        end

      name4 =
        case Enum.find(names, fn {id, _} -> id == 4 end) do
          {_, val} -> val
          _ -> nil
        end

      name1 =
        case Enum.find(names, fn {id, _} -> id == 1 end) do
          {_, val} -> val
          _ -> nil
        end

      ps_name =
        cond do
          name6 -> name6
          name4 -> name4
          name1 -> name1
          true -> "NO-VALID-NAME"
        end

      # replace spaces in ps_name with dashes
      # self.family_name = names[1] or ps_name
      # self.style_name = names[2] or 'Regular'
      # self.full_name = names[4] or ps_name
      # self.unique_font_id = names[3] or ps_name
      %{ttf | name: ps_name}
    else
      %{ttf | name: "NO-VALID-NAME"}
    end
  end

  defmodule FontTable do
    @moduledoc false
    defstruct name: "", checksum: 0, offset: 0, length: 0
  end

  # read in the font tables
  def read_tables(data, num_tables) do
    # each table definition is 16 bytes
    table_defs = binary_part(data, 0, num_tables * 16)

    for <<(<<tag::binary-size(4), checksum::32, offset::32, length::32>> <- table_defs)>>,
      do: %FontTable{name: tag, checksum: checksum, offset: offset, length: length}
  end

  defp read_offset(offsets, data, 0), do: {offsets, data}

  defp read_offset(offsets, <<offset::32, rem::binary>>, count) do
    read_offset([offset | offsets], rem, count - 1)
  end

  defp read_name_records(recs, _data, 0), do: recs

  defp read_name_records(recs, data, n_recs) do
    <<platform::16, encoding::16, language::16, name_id::16, length::16, offset::16,
      remaining::binary>> = data

    r = %{
      platform: platform,
      encoding: encoding,
      lang: language,
      name_id: name_id,
      length: length,
      offset: offset
    }

    read_name_records([r | recs], remaining, n_recs - 1)
  end

  # Platform 3 (Windows) -- encoding 1 (UCS-2) and 10 (UCS-4)
  defp record_to_name(%{platform: 3} = record, offset, data) do
    read_utf16_name(record, offset, data)
  end

  # Platform 0 (Unicode)
  defp record_to_name(%{platform: 0} = record, offset, data) do
    read_utf16_name(record, offset, data)
  end

  # Platform 2 (deprecated; identical to platform 0)
  defp record_to_name(%{platform: 2, encoding: 1} = record, offset, data) do
    read_utf16_name(record, offset, data)
  end

  # ASCII(UTF-8) for most platform/encodings
  defp record_to_name(record, offset, data) do
    raw = binary_part(data, record.offset + offset, record.length)
    {record.name_id, to_string(raw)}
  end

  # handle the unicode (UTF-16BE) names
  defp read_utf16_name(record, offset, data) do
    raw = binary_part(data, record.offset + offset, record.length)
    chars = :unicode.characters_to_list(raw, {:utf16, :big})
    {record.name_id, to_string(chars)}
  end

  def find_preferred_encoding(candidates) do
    # Select a Unicode CMAP by preference
    preferred = [
      # 32-bit Unicode formats
      {3, 10},
      {0, 6},
      {0, 4},
      # 16-bit Unicode formats
      {3, 1},
      {0, 3},
      {0, 2},
      {0, 1},
      {0, 0},
      # Windows symbol font (usually unicode)
      {3, 0}
    ]

    preferred |> Enum.find(fn {plat, enc} -> {plat, enc} in candidates end)
  end

  def extract_metrics(ttf, data) do
    _ = """
    *flags        Font flags
    *ascent       Typographic ascender in 1/1000ths of a point
    *descent      Typographic descender in 1/1000ths of a point
    *cap_height    Cap height in 1/1000ths of a point (0 if not available)
    *bbox         Glyph bounding box [l,t,r,b] in 1/1000ths of a point
    *units_per_em   Glyph units per em
    *italic_angle  Italic angle in degrees ccw
    *stem_v        stem weight in 1/1000ths of a point (approximate)

    default_width   default glyph width in 1/1000ths of a point
    char_widths     dictionary of character widths for every supported UCS character
    code
    """

    raw_head = raw_table(ttf, "head", data)

    {bbox, units_per_em} =
      if raw_head do
        <<_major::16, _minor::16, _rev::32, _checksum_adj::32, 0x5F, 0x0F, 0x3C, 0xF5, _flags::16,
          units_per_em::16, _created::signed-64, _modified::signed-64, minx::signed-16,
          miny::signed-16, maxx::signed-16, maxy::signed-16, _mac_style::16, _lowest_ppem::16,
          _font_direction_hint::signed-16, _glyph_mapping_fmt::signed-16,
          _glyph_data_fmt::signed-16>> = raw_head

        {[minx, miny, maxx, maxy], units_per_em}
      else
        {[-100, -100, 100, 100], 1000}
      end

    raw_os2 = raw_table(ttf, "OS/2", data)

    measured =
      if raw_os2 do
        # https://www.microsoft.com/typography/otspec/os2.htm
        # match version 0 struct, extract additional fields as needed
        # us_width_class = Condensed < Normal < Expanded
        # fs_type = flags that control embedding
        # unicode range 1-4 are bitflags that identify charsets
        # sel_flags = italic, underscore, bold, strikeout, outlined...
        # TODO: conform to fs_type restrictions
        <<os2ver::16, _avg_char_width::signed-16, us_weight_class::16, _us_width_class::16,
          _fs_type::16, _sub_x_size::signed-16, _sub_y_size::signed-16, _sub_x_offset::signed-16,
          _sub_y_offset::signed-16, _super_x_size::signed-16, _super_y_size::signed-16,
          _super_x_offset::signed-16, _super_y_offset::signed-16, _strikeout_size::signed-16,
          _strikeout_pos::signed-16, family_class::signed-16, _panose::80, _unicode_range1::32,
          _unicode_range2::32, _unicode_range3::32, _unicode_range4::32, _vendor_id::32,
          _sel_flags::16, _first_char::16, _last_char::16, typo_ascend::signed-16,
          typo_descend::signed-16, typo_line_gap::signed-16, _win_ascent::16, _win_descent::16,
          v0rest::binary>> = raw_os2

        Logger.debug("OS/2 ver #{os2ver} found")

        # os2ver 1 or greater has code page range fields
        v1rest =
          if os2ver > 0 do
            <<_cp1::32, _cp2::32, v1rest::binary>> = v0rest
            v1rest
          else
            nil
          end

        # if we have a v2 or higher struct we can read out
        # the x_height and cap_height
        if os2ver > 1 and v1rest do
          <<x_height::signed-16, cap_height::signed-16, default_char::16, break_char::16,
            max_content::16, _v2rest::binary>> = v1rest

          %{
            ttf
            | ascent: typo_ascend,
              descent: typo_descend,
              line_gap: typo_line_gap,
              cap_height: cap_height,
              max_content: max_content,
              break_char: break_char,
              default_char: default_char,
              x_height: x_height,
              us_weight_class: us_weight_class,
              family_class: family_class
          }
        else
          cap_height = 0.7 * units_per_em

          %{
            ttf
            | ascent: typo_ascend,
              descent: typo_descend,
              cap_height: cap_height,
              us_weight_class: us_weight_class,
              family_class: family_class
          }
        end

        # for osver > 4 also fields:
        # lower_optical_point_size::16, upper_optical_point_size::16
      else
        Logger.debug("No OS/2 info, synthetic data")

        %{
          ttf
          | ascent: Enum.at(bbox, 3),
            descent: Enum.at(bbox, 1),
            cap_height: Enum.at(bbox, 3),
            us_weight_class: 500
        }
      end

    # There's no way to get stem_v from a TTF file short of analyzing actual outline data
    # This fuzzy formula is taken from pdflib sources, but we could just use 0 here
    stem_v = 50 + trunc(measured.us_weight_class / 65.0 * (measured.us_weight_class / 65.0))

    [min_x, min_y, max_x, max_y] = bbox

    measured = %{
      measured
      | bbox: %{measured.bbox | min_x: min_x, min_y: min_y, max_x: max_x, max_y: max_y}
    }

    extract_more_metrics(
      %{measured | units_per_em: units_per_em, stem_v: stem_v},
      data
    )
  end

  defp extract_more_metrics(ttf, data) do
    # TODO: these should be const enum somewhere
    flag_fixed = 0b0001
    flag_serif = 0b0010
    flag_symbolic = 0b0100
    flag_script = 0b1000
    flag_italic = 0b1000000
    # flag_allcaps = 1 <<< 16
    # flag_smallcaps = 1 <<< 17
    flag_forcebold = 1 <<< 18

    # flags, italic angle, default width
    raw_post = raw_table(ttf, "post", data)

    {itals, fixed, forcebold, italic_angle} =
      if raw_post do
        <<_ver_major::16, _ver_minor::16, italic_mantissa::signed-16, italic_fraction::16,
          _underline_position::signed-16, _underline_thickness::signed-16, is_fixed_pitch::32,
          _rest::binary>> = raw_post

        # this is F2DOT14 format defined in OpenType standard
        italic_angle = italic_mantissa + italic_fraction / 16384.0

        # if SEMIBOLD or heavier, set forcebold flag
        forcebold = if ttf.us_weight_class >= 600, do: flag_forcebold, else: 0

        # a non-zero angle sets the italic flag
        itals = if italic_angle != 0, do: flag_italic, else: 0

        # mark it fixed pitch if needed
        fixed = if is_fixed_pitch > 0, do: flag_fixed, else: 0
        {itals, fixed, forcebold, italic_angle}
      else
        {0, 0, 0, 0}
      end

    # SERIF and SCRIPT can be derived from s_family_class in OS/2 table
    class = ttf.family_class >>> 8
    serif = if Enum.member?(1..7, class), do: flag_serif, else: 0
    script = if class == 10, do: flag_script, else: 0
    flags = flag_symbolic ||| itals ||| forcebold ||| fixed ||| serif ||| script

    # hhea
    raw_hhea = raw_table(ttf, "hhea", data)

    if raw_hhea do
      <<_ver_major::16, _ver_minor::16, _ascender::signed-16, _descender::signed-16,
        _linegap::signed-16, _advance_width_max::16, _min_left_bearing::signed-16,
        _min_right_bearing::signed-16, _x_max_extent::signed-16, _caret_slope_rise::16,
        _caret_slope_run::16, _caret_offset::signed-16, _reserved::64,
        _metric_data_format::signed-16, num_metrics::16>> = raw_hhea

      # maxp
      # number of glyphs -- will need to subset if more than 255
      # hmtx (glyph widths)
      raw_hmtx = raw_table(ttf, "hmtx", data)
      gw = get_glyph_widths(raw_hmtx, [], num_metrics)

      %{
        ttf
        | italic_angle: italic_angle,
          flags: flags,
          glyph_widths: gw,
          default_width: Enum.at(gw, 0)
      }
    else
      ttf
    end
  end

  defp get_glyph_widths(_, acc, 0), do: Enum.reverse(acc)

  defp get_glyph_widths(<<width::16, _::16, rest::binary>>, acc, i),
    do: get_glyph_widths(rest, [width | acc], i - 1)

  # mark what portion of the font is embedded
  # this may get more complex when we do proper subsetting
  def mark_embedded_part(ttf, data) do
    %{ttf | embed: data}
  end

  # cmap header
  def extract_c_map(ttf, data) do
    raw_cmap = raw_table(ttf, "cmap", data)

    if raw_cmap do
      # version, num_tables
      <<_version::16, numtables::16, cmaptables::binary>> = raw_cmap
      # read in tableoffsets (plat, enc, offset)
      {cmapoffsets, _cmapdata} = read_c_map_offsets([], cmaptables, numtables)

      # find the best available format
      selected =
        cmapoffsets
        |> Enum.map(fn {plat, enc, _} -> {plat, enc} end)
        |> find_preferred_encoding

      # if we found a preferred format, locate it
      {plat, enc, off} =
        if selected != nil do
          Enum.find(cmapoffsets, fn {plat, enc, _} -> {plat, enc} == selected end)
        else
          # no preferred format available, just handle the first one
          hd(cmapoffsets)
        end

      # we need the table's offset and length to find subtables
      {raw_off, raw_len} = lookup_table(ttf, "cmap")
      raw_cmap = binary_part(data, raw_off + off, raw_len - off)
      cid2gid = read_c_map_data(plat, enc, raw_cmap, %{})

      # reverse the lookup as naive ToUnicode map
      gid2cid = Enum.map(cid2gid, fn {k, v} -> {v, k} end) |> Map.new()
      %{ttf | :cid2gid => cid2gid, :gid2cid => gid2cid}
    else
      ttf
    end
  end

  # read in the platform, encoding, offset triplets
  defp read_c_map_offsets(tables, data, 0) do
    {tables, data}
  end

  defp read_c_map_offsets(tables, data, n_tables) do
    <<platform::16, encoding::16, offset::32, remaining::binary>> = data
    t = {platform, encoding, offset}
    read_c_map_offsets([t | tables], remaining, n_tables - 1)
  end

  # read CMap format 4 (5.2.1.3.3: Segment mapping to delta values)
  # this is the most useful one for the majority of modern fonts
  # used for Windows Unicode mappings (platform 3 encoding 1) for BMP
  defp read_c_map_data(
         _platform,
         _encoding,
         <<4::16, _length::16, _lang::16, subdata::binary>>,
         cmap
       ) do
    <<double_segments::16, _search_range::16, _entry_selector::16, _range_shift::16,
      segments::binary>> = subdata

    # IO.puts "READ UNICODE TABLE #{platform} #{encoding}"
    segment_count = div(double_segments, 2)
    # segment end values
    {endcodes, ec_done} = read_segment_data([], segments, segment_count)
    # reserved::16
    <<_reserved::16, skip_res::binary>> = ec_done
    # segment start values
    {startcodes, start_done} = read_segment_data([], skip_res, segment_count)
    # segment delta values
    {deltas, delta_done} = read_signed_segment_data([], start_done, segment_count)
    # segment range offset values
    {offsets, _glyph_data} = read_segment_data([], delta_done, segment_count)
    # combine the segment data we've read in
    segs =
      List.zip([startcodes, endcodes, deltas, offsets])
      |> Enum.reverse()

    # generate the character-to-glyph map
    # TODO: also generate glyph-to-character map
    segs
    |> Enum.with_index()
    |> Enum.reduce(cmap, fn {x, index}, acc -> map_segment(x, acc, index, delta_done) end)
  end

  # read CMap format 12 (5.2.1.3.7 Segmented coverage)
  # This is required by Windows fonts (Platform 3 encoding 10) that have UCS-4 characters
  # and is a SUPERSET of the data stored in format 4
  defp read_c_map_data(
         _platform,
         _encoding,
         <<12::16, _::16, _length::32, _lang::32, groups::32, subdata::binary>>,
         cmap
       ) do
    read_c_map12_entry([], subdata, groups)
    |> Enum.reduce(cmap, fn {s, e, g}, acc -> map_c_map12_entry(s, e, g, acc) end)
  end

  # unknown formats we ignore for now
  defp read_c_map_data(_platform, _encoding, <<_fmt::16, _subdata::binary>>, cmap) do
    # IO.inspect {"READ", fmt, platform, encoding}
    cmap
  end

  defp map_c_map12_entry(startcode, endcode, glyphindex, charmap) do
    offset = glyphindex - startcode

    startcode..endcode
    |> Enum.reduce(charmap, fn i, acc -> Map.put_new(acc, i, i + offset) end)
  end

  defp read_c_map12_entry(entries, _, 0), do: entries

  defp read_c_map12_entry(entries, data, count) do
    <<s::32, e::32, g::32, remaining::binary>> = data
    read_c_map12_entry([{s, e, g} | entries], remaining, count - 1)
  end

  defp map_segment({0xFFFF, 0xFFFF, _, _}, charmap, _, _) do
    charmap
  end

  defp map_segment({first, last, delta, 0}, charmap, _, _) do
    first..last
    |> Enum.reduce(charmap, fn x, acc -> Map.put_new(acc, x, x + delta &&& 0xFFFF) end)
  end

  defp map_segment({first, last, delta, offset}, charmap, segment_index, data) do
    first..last
    |> Enum.reduce(charmap, fn x, acc ->
      offsetx = (x - first) * 2 + offset + 2 * segment_index

      g =
        case binary_part(data, offsetx, 2) do
          <<0::16>> -> 0
          <<glyph::16>> -> glyph + delta
        end

      Map.put_new(acc, x, g &&& 0xFFFF)
    end)
  end

  defp read_segment_data(vals, data, 0) do
    {vals, data}
  end

  defp read_segment_data(vals, <<v::16, rest::binary>>, remaining) do
    read_segment_data([v | vals], rest, remaining - 1)
  end

  defp read_signed_segment_data(vals, data, 0) do
    {vals, data}
  end

  defp read_signed_segment_data(vals, <<v::signed-16, rest::binary>>, remaining) do
    read_segment_data([v | vals], rest, remaining - 1)
  end

  def extract_features(ttf, data) do
    {sub_s, sub_f, sub_l} =
      if has_table?(ttf, "GSUB"), do: extract_off_header("GSUB", ttf, data), else: {[], [], []}

    {pos_s, pos_f, pos_l} =
      if has_table?(ttf, "GPOS"), do: extract_off_header("GPOS", ttf, data), else: {[], [], []}

    # read in definitions
    gdef = raw_table(ttf, "GDEF", data)
    definitions = if gdef != nil, do: extract_glyph_definition_table(gdef), else: nil

    %{
      ttf
      | substitutions: {sub_s, sub_f, sub_l},
        positions: {pos_s, pos_f, pos_l},
        definitions: definitions
    }
  end

  # returns script/language map, feature list, lookup tables
  defp extract_off_header(table, ttf, data) do
    raw = raw_table(ttf, table, data)

    if raw == nil do
      Logger.debug("No #{table} table")
    end

    <<version_maj::16, version_min::16, script_list_off::16, feature_list_off::16,
      lookup_list_off::16, _::binary>> = raw

    # if 1.1, also feature_variations::u32
    if {version_maj, version_min} != {1, 0} do
      Logger.debug("#{table} Header #{version_maj}.#{version_min}")
    end

    lookup_list = subtable(raw, lookup_list_off)
    <<n_lookups::16, ll::binary-size(n_lookups)-unit(16), _::binary>> = lookup_list
    # this actually gives us offsets to lookup tables
    lookups = for <<(<<x::16>> <- ll)>>, do: x

    lookup_tables =
      lookups
      |> Enum.map(fn x -> get_lookup_table(x, lookup_list) end)

    # get the feature array
    feature_list = subtable(raw, feature_list_off)
    features = parse_features(feature_list)

    script_list = subtable(raw, script_list_off)
    <<n_scripts::16, sl::binary-size(n_scripts)-unit(48), _::binary>> = script_list
    scripts = for <<(<<tag::binary-size(4), offset::16>> <- sl)>>, do: {tag, offset}

    scripts =
      scripts
      |> Enum.map(fn {tag, off} -> read_script_table(tag, script_list, off) end)
      |> Map.new()

    {scripts, features, lookup_tables}
  end

  defp extract_glyph_definition_table(table) do
    <<version_maj::16, version_min::16, glyph_class_def::16, _attach_list::16,
      _lig_caret_list::16, mark_attach_class::16, rest::binary>> = table

    # 1.2 also has 16-bit offset to mark_glyph_sets_def
    mark_glyph_sets =
      if version_maj >= 1 and version_min >= 2 do
        <<mark_glyph_sets::16, _::binary>> = rest
        if mark_glyph_sets == 0, do: nil, else: mark_glyph_sets
      else
        nil
      end

    # 1.3 also has 32-bit offset to item_var_store
    # Logger.debug "GDEF #{version_maj}.#{version_min}"

    # predefined classes for use with GSUB/GPOS flags
    # 1 = Base, 2 = Ligature, 3 = Mark, 4 = Component
    glyph_class_def =
      if glyph_class_def > 0, do: parse_glyph_class(subtable(table, glyph_class_def)), else: nil

    # mark attachment class (may be NULL; used with flag in GPOS/GSUB lookups)
    mark_attach_class =
      if mark_attach_class > 0,
        do: parse_glyph_class(subtable(table, mark_attach_class)),
        else: nil

    # mark glyph sets (may be NULL)
    glyph_sets =
      if mark_glyph_sets != nil do
        mgs = subtable(table, mark_glyph_sets)
        <<_fmt::16, n_glyph_sets::16, gsets::binary-size(n_glyph_sets)-unit(32), _::binary>> = mgs
        for <<(<<off::32>> <- gsets)>>, do: parse_coverage(subtable(mgs, off))
      else
        nil
      end

    _mgs = mark_glyph_sets
    %{attachments: mark_attach_class, classes: glyph_class_def, mark_sets: glyph_sets}
  end

  # this should probably be an actual map of tag: [indices]
  def parse_features(data) do
    <<n_features::16, fl::binary-size(n_features)-unit(48), _::binary>> = data
    features = for <<(<<tag::binary-size(4), offset::16>> <- fl)>>, do: {tag, offset}

    features
    |> Enum.map(fn {t, o} -> read_lookup_indices(t, o, data) end)
  end

  # returns {lookup_type, lookup_flags, [subtable offsets], <<raw table bytes>>, mark filtering set}
  defp get_lookup_table(offset, data) do
    tbl = subtable(data, offset)

    <<lookup_type::16, flags::16, nsubtables::16, st::binary-size(nsubtables)-unit(16),
      rest::binary>> = tbl

    # if flag bit for markfilteringset, also mark_filtering_set_index::16
    mfs =
      if flags &&& 0x0010 do
        <<mfs::16, _::binary>> = rest
        mfs
      else
        nil
      end

    subtables = for <<(<<y::16>> <- st)>>, do: y
    {lookup_type, flags, subtables, tbl, mfs}
  end

  defp read_script_table(tag, data, offset) do
    script_table = subtable(data, offset)
    <<default_off::16, n_lang::16, langx::binary-size(n_lang)-unit(48), _::binary>> = script_table
    langs = for <<(<<tag::binary-size(4), offset::16>> <- langx)>>, do: {tag, offset}

    langs =
      langs
      |> Enum.map(fn {tag, offset} -> read_feature_indices(tag, offset, script_table) end)
      |> Map.new()

    langs =
      if default_off == 0 do
        langs
      else
        {_, indices} = read_feature_indices(nil, default_off, script_table)
        Map.put(langs, nil, indices)
      end

    {tag, langs}
  end

  defp read_feature_indices(tag, offset, data) do
    feature_part = subtable(data, offset)

    <<reordering_table::16, req::16, n_features::16, fx::binary-size(n_features)-unit(16),
      _::binary>> = feature_part

    if reordering_table != 0 do
      Logger.debug("Lang #{tag} has a reordering table")
    end

    indices = for <<(<<x::16>> <- fx)>>, do: x
    indices = if req == 0xFFFF, do: indices, else: [req | indices]
    {tag, indices}
  end

  defp read_lookup_indices(tag, offset, data) do
    lookup_part = subtable(data, offset)

    <<feature_params_offset::16, n_lookups::16, fx::binary-size(n_lookups)-unit(16), _::binary>> =
      lookup_part

    if feature_params_offset != 0 do
      Logger.debug("Feature #{tag} has feature params")
    end

    indices = for <<(<<x::16>> <- fx)>>, do: x
    {tag, indices}
  end

  def parse_glyph_class(
        <<1::16, start::16, n_glyphs::16, classes::binary-size(n_glyphs)-unit(16), _::binary>>
      ) do
    classes = for <<(<<class::16>> <- classes)>>, do: class

    classes
    |> Enum.with_index(start)
    |> Map.new(fn {class, glyph} -> {glyph, class} end)
  end

  def parse_glyph_class(
        <<2::16, n_ranges::16, ranges::binary-size(n_ranges)-unit(48), _::binary>>
      ) do
    ranges = for <<(<<first::16, last::16, class::16>> <- ranges)>>, do: {first, last, class}
    ranges
  end

  # parse coverage tables
  def parse_coverage(<<1::16, nrecs::16, glyphs::binary-size(nrecs)-unit(16), _::binary>>) do
    for <<(<<x::16>> <- glyphs)>>, do: x
  end

  def parse_coverage(<<2::16, nrecs::16, ranges::binary-size(nrecs)-unit(48), _::binary>>) do
    for <<(<<startg::16, endg::16, covindex::16>> <- ranges)>>, do: {startg, endg, covindex}
  end

  def parse_alts(table, alt_offset) do
    <<n_alts::16, alts::binary-size(n_alts)-unit(16), _::binary>> = subtable(table, alt_offset)
    for <<(<<x::16>> <- alts)>>, do: x
  end

  def parse_ligature_set(table, ls_offset) do
    <<nrecs::16, ligat::binary-size(nrecs)-unit(16), _::binary>> = subtable(table, ls_offset)
    liga_off = for <<(<<x::16>> <- ligat)>>, do: x

    liga_off
    |> Stream.map(fn x -> subtable(table, ls_offset + x) end)
    |> Stream.map(fn <<g::16, n_comps::16, rest::binary>> -> {g, n_comps - 1, rest} end)
    |> Enum.map(fn {g, n, data} ->
      <<recs::binary-size(n)-unit(16), _::binary>> = data
      gg = for <<(<<x::16>> <- recs)>>, do: x
      {g, gg}
    end)
  end

  def parse_context_sub_rule1(rule) do
    <<n_glyphs::16, subst_count::16, rest::binary>> = rule
    # subtract one since initial glyph handled by coverage
    glyph_count = n_glyphs - 1

    <<input::binary-size(glyph_count)-unit(16), subst_recs::binary-size(subst_count)-unit(32),
      _::binary>> = rest

    input_glyphs = for <<(<<g::16>> <- input)>>, do: g
    subst_records = for <<(<<x::16, y::16>> <- subst_recs)>>, do: {x, y}
    {input_glyphs, subst_records}
  end

  def parse_chained_sub_rule2(rule) do
    <<bt_count::16, bt::binary-size(bt_count)-unit(16), n_glyphs::16, rest::binary>> = rule
    # subtract one since initial glyph handled by coverage
    glyph_count = n_glyphs - 1

    <<input::binary-size(glyph_count)-unit(16), la_count::16, la::binary-size(la_count)-unit(16),
      subst_count::16, subst_recs::binary-size(subst_count)-unit(32), _::binary>> = rest

    backtrack = for <<(<<g::16>> <- bt)>>, do: g
    lookahead = for <<(<<g::16>> <- la)>>, do: g
    input_glyphs = for <<(<<g::16>> <- input)>>, do: g
    subst_records = for <<(<<x::16, y::16>> <- subst_recs)>>, do: {x, y}
    {backtrack, input_glyphs, lookahead, subst_records}
  end

  def parse_anchor(<<_fmt::16, x_coord::signed-16, y_coord::signed-16, _rest::binary>>) do
    # anchor_table (common table)
    # coords are signed!
    # fmt = 1, x_coord::16, y_coord::16
    # fmt = 2, x_coord::16, y_coord::16, index to glyph countour point::16
    # fmt = 3, x_coord::16, y_coord::16, device table offset (for x)::16, device table offset (for y)::16
    {x_coord, y_coord}
  end

  def parse_mark_array(table) do
    <<n_recs::16, records::binary-size(n_recs)-unit(32), _::binary>> = table

    mark_array =
      for <<(<<mark_class::16, anchor_table_offset::16>> <- records)>>,
        do: {mark_class, anchor_table_offset}

    mark_array
    |> Enum.map(fn {c, o} -> {c, parse_anchor(subtable(table, o))} end)
  end

  def parse_pair_set(table, offset, fmt_a, fmt_b) do
    size_a = value_record_size(fmt_a)
    size_b = value_record_size(fmt_b)
    data = binary_part(table, offset, byte_size(table) - offset)
    # value_record_size returns size in bytes
    pair_size = 2 + size_a + size_b
    <<n_pairs::16, pairdata::binary>> = data

    pairs =
      for <<(<<glyph::16, v1::binary-size(size_a), v2::binary-size(size_b)>> <-
               binary_part(pairdata, 0, pair_size * n_pairs))>>,
          do: {glyph, v1, v2}

    pairs =
      pairs
      |> Enum.map(fn {g, v1, v2} ->
        {g, read_positioning_value_record(fmt_a, v1), read_positioning_value_record(fmt_b, v2)}
      end)

    pairs
  end

  # value_record in spec
  def read_positioning_value_record(0, _), do: nil

  def read_positioning_value_record(format, bytes) do
    # format is bitset of fields to read for each records
    {x_place, xprest} = extract_value_record_val(format &&& 0x0001, bytes)
    {y_place, yprest} = extract_value_record_val(format &&& 0x0002, xprest)
    {x_adv, xarest} = extract_value_record_val(format &&& 0x0004, yprest)
    {y_adv, _xyrest} = extract_value_record_val(format &&& 0x0008, xarest)

    # TODO: also offsets to device table
    {x_place, y_place, x_adv, y_adv}
  end

  defp extract_value_record_val(_flag, ""), do: {0, ""}

  defp extract_value_record_val(flag, data) do
    if flag != 0 do
      <<x::signed-16, r::binary>> = data
      {x, r}
    else
      {0, data}
    end
  end

  def value_record_size(format) do
    flags = for <<(x::1 <- <<format>>)>>, do: x
    # record size is 2 bytes per set flag in the format spec
    Enum.count(flags, fn x -> x == 1 end) * 2
  end
end
