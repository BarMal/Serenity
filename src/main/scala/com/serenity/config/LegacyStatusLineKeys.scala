package com.serenity.config

/** The pre-`status.*` config keys, folded into one [[StatusLineConfig]] on load.
  *
  * Four separate mechanisms used to share the bottom row: the always-on "gutter" text, an opt-in cursor info bar with
  * its own segments and placement, a word-count toggle appended to whichever of those was showing, and a mode glyph
  * folded into a corner. Each had its own key. A file written by that version is read here into the one model so the
  * user sees what they saw before, and the keys are reported as deprecated with their `status.*` replacement.
  *
  * Only the keys whose old meaning does not map one-to-one onto a new field are handled here; the colour and alpha keys
  * carry the same value as before and are plain aliases on the `status.*` fields instead.
  */
object LegacyStatusLineKeys:

  private val SegmentKeys   = Set("cursor.info_bar.segments", "cursor.info_bar", "cursor.info.bar", "cursor_info_bar")
  private val PlacementKeys = Set("cursor.info_bar.placement", "cursor.info.bar.placement", "cursor_info_bar_placement")
  private val GutterKeys    = Set("display.gutter", "display_gutter")
  private val WordCountKeys = Set("display.word_count", "display.word.count", "display_word_count")
  private val ModeCornerKeys = Set("widget.mode_tab_corner", "widget_mode_tab_corner")

  val replacements: Map[String, String] =
    (SegmentKeys.map(_ -> "status.segments") ++
      PlacementKeys.map(_ -> "status.placement") ++
      GutterKeys.map(_ -> "status.placement") ++
      WordCountKeys.map(_ -> "status.segments") ++
      ModeCornerKeys.map(_ -> "status.segments")).toMap

  def handles(key: String): Boolean = replacements.contains(key)

  /** Whether an old key's value is one the old format would have refused too -- so the migration report can name it. */
  def rejects(key: String, value: String): Boolean =
    if SegmentKeys.contains(key) then StatusSegment.parseList(value).isEmpty
    else if PlacementKeys.contains(key) then StatusLinePlacement.fromConfigKey(value).isEmpty
    else if GutterKeys.contains(key) || WordCountKeys.contains(key) then FieldCodec.parseBoolean(value).isEmpty
    else if ModeCornerKeys.contains(key) then CornerPosition.fromConfigKey(value).isEmpty
    else false

  /** Applies the legacy keys' combined meaning, unless the file already speaks the current dialect (any
    * `status.segments` or `status.placement` present), in which case the old keys are ignored rather than fought over.
    */
  def applied(config: AppConfig, entries: List[(String, String)]): AppConfig =
    val keyed = entries.toMap
    if keyed.contains("status.segments") || keyed.contains("status.placement") then config
    else
      def firstOf(keys: Set[String]): Option[String] = keys.toList.flatMap(keyed.get).headOption
      resolve(
        base = config.statusLine,
        legacySegments = firstOf(SegmentKeys).flatMap(StatusSegment.parseList),
        legacyPlacement = firstOf(PlacementKeys).flatMap(StatusLinePlacement.fromConfigKey),
        gutterOff = firstOf(GutterKeys).flatMap(FieldCodec.parseBoolean).contains(false),
        wordCount = firstOf(WordCountKeys).flatMap(FieldCodec.parseBoolean).contains(true),
        anyLegacyKeyPresent = (SegmentKeys ++ PlacementKeys ++ GutterKeys ++ WordCountKeys).exists(keyed.contains)
      ).fold(config)(config.withStatusLine)

  /** The old keys' combined meaning as one status line, or `None` when none of them was set.
    *
    *   - An explicit info bar (segments given) keeps its segments and placement -- floating was its default -- with the
    *     mode appended, since the corner glyph always showed it alongside.
    *   - No info bar but the gutter switched off means no status row at all.
    *   - Otherwise the old gutter text (position, language, title) plus the glyph is exactly today's default, pinned.
    *   - The word-count toggle appends its three segments to whichever of those applies.
    */
  def resolve(
    base: StatusLineConfig,
    legacySegments: Option[List[StatusSegment]],
    legacyPlacement: Option[StatusLinePlacement],
    gutterOff: Boolean,
    wordCount: Boolean,
    anyLegacyKeyPresent: Boolean
  ): Option[StatusLineConfig] =
    Option.when(anyLegacyKeyPresent) {
      val wordCountSegments =
        if wordCount then List(StatusSegment.WordCount, StatusSegment.CharCount, StatusSegment.ReadingTime) else Nil
      legacySegments.filter(_.nonEmpty) match
        case Some(segments) =>
          base.copy(
            segments = (segments ++ wordCountSegments :+ StatusSegment.Mode).distinct,
            placement = legacyPlacement.getOrElse(StatusLinePlacement.Floating)
          )
        case None if gutterOff =>
          base.copy(placement = StatusLinePlacement.Off)
        case None =>
          base.copy(
            segments = (StatusLineConfig.defaultSegments ++ wordCountSegments).distinct,
            placement = StatusLinePlacement.Pinned
          )
    }
