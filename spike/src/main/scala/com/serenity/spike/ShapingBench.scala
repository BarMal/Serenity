package com.serenity.spike

import org.jetbrains.skia.{Font, FontEdging, FontMgr, FontStyle, Point, TextLine}
import org.jetbrains.skia.paragraph.{
  FontCollection,
  ParagraphBuilder,
  ParagraphStyle,
  RectHeightMode,
  RectWidthMode,
  TextStyle
}
import org.jetbrains.skia.shaper.{BidiRun, FontRun, LanguageRun, RunHandler, RunInfo, ScriptRun, Shaper, ShapingOptions}

import com.serenity.ui.layout.{SpikeLayoutBridge, TextLayoutSnapshot}

/** Shaping and caret-stop read-back for the 300 lorem paragraphs: Serenity's Java2D `GlyphAdvances` and full wrap
  * against what Skiko exposes (TextLine, Shaper + RunHandler, TextBlob, Paragraph). Each pass covers every paragraph;
  * the reported time is one whole-document pass.
  */
object ShapingBench:

  def run(options: SpikeOptions): Unit =
    val paragraphs = LoremDocument.load(options.document).filter(_.nonEmpty)
    val awtFont    = SerenityWrap().font
    val frc        = TextLayoutSnapshot.defaultFontRenderContext()
    val typeface   = FontMgr.Companion.getDefault().matchFamilyStyle("serif", FontStyle.Companion.getNORMAL())
    val skiaFont   = Font(typeface, Geometry.FontSize)
    skiaFont.setSubpixel(true)
    skiaFont.setEdging(FontEdging.ANTI_ALIAS)
    val shaper     = Shaper.Companion.make()
    val chars      = paragraphs.map(_.length).sum
    Report.note(
      "shaping.setup",
      s"paragraphs=${paragraphs.length} chars=$chars awt_font=${awtFont.getFontName} skia_typeface=${typeface.getFamilyName}"
    )

    def pass(name: String, iterations: Int, extra: String = "")(body: => Unit): Unit =
      (0 until 3).foreach(_ => body)
      Report.result(s"shaping.$name", (0 until iterations).map(_ => Timing.ms(body)), extra)

    pass("serenity.glyph_advances_with_carets", options.shapingIterations, "(per-char caret stops: yes)") {
      paragraphs.foreach(text => SpikeLayoutBridge.glyphAdvanceCarets(text, awtFont, frc))
    }
    pass("serenity.full_wrap", options.shapingIterations, "(rows + per-row caret stops: yes)") {
      paragraphs.zipWithIndex.foreach((text, line) =>
        SpikeLayoutBridge.wrapLine(text, line, Geometry.WrapWidthPx, awtFont, frc)
      )
    }
    pass("skia.textline_make_positions", options.shapingIterations, "(per-glyph x via getPositions)") {
      paragraphs.foreach { text =>
        val line = TextLine.Companion.make(text, skiaFont)
        line.getPositions
        line.close()
      }
    }
    pass("skia.reused_shaper_shapeline_positions", options.shapingIterations, "(one Shaper for all; per-glyph x)") {
      paragraphs.foreach { text =>
        val line = shaper.shapeLine(text, skiaFont)
        line.getPositions
        line.close()
      }
    }
    pass("skia.nominal_glyph_widths", options.shapingIterations, "(getStringGlyphs + getWidths: no shaping, like GlyphAdvances' nominal path)") {
      paragraphs.foreach(text => skiaFont.getWidths(skiaFont.getStringGlyphs(text)))
    }
    pass("skia.textline_carets_per_offset", options.shapingIterations, "(getCoordAtOffset for every offset: 1 JNI call each)") {
      paragraphs.foreach { text =>
        val line = TextLine.Companion.make(text, skiaFont)
        (0 to text.length).foreach(offset => line.getCoordAtOffset(offset))
        line.close()
      }
    }
    pass("skia.shaper_runhandler", options.shapingIterations, "(glyphs + positions + clusters per run)") {
      paragraphs.foreach { text =>
        shaper.shape(text, skiaFont, ShapingOptions.Companion.getDEFAULT(), Float.MaxValue, CollectingRunHandler())
      }
    }
    pass("skia.shaper_textblob_positions_clusters", options.shapingIterations) {
      paragraphs.foreach { text =>
        val blob = shaper.shape(text, skiaFont)
        if blob != null then
          blob.getPositions
          blob.getClusters
          blob.close()
      }
    }
    pass("skia.shaper_textblob_5_char_strings", options.shapingIterations, "(300 calls on \"lorem\": fixed per-call cost)") {
      paragraphs.foreach(_ => Option(shaper.shape("lorem", skiaFont)).foreach(_.close()))
    }
    pass(
      "skia.shaper_explicit_single_runs",
      options.shapingIterations,
      "(caller-supplied font/bidi/script/language runs: no ICU or fallback iterators; LTR Latin assumed)"
    ) {
      import scala.jdk.CollectionConverters.*
      paragraphs.foreach { text =>
        val end = text.length
        shaper.shape(
          text,
          Iterator.single(FontRun(end, skiaFont)).asJava,
          Iterator.single(BidiRun(end, 0)).asJava,
          Iterator.single(ScriptRun(end, "Latn")).asJava,
          Iterator.single(LanguageRun(end, "en")).asJava,
          ShapingOptions.Companion.getDEFAULT(),
          Float.MaxValue,
          CollectingRunHandler()
        )
      }
    }
    val explicitShaper = ExplicitRunShaper(skiaFont)
    pass("skia.explicit_runs_blob_and_carets", options.shapingIterations, "(what the spike renderer uses: TextBlob + per-offset caret array)") {
      paragraphs.foreach(text => explicitShaper.shape(text).close())
    }
    val explicitVsTextLine = paragraphs.map { text =>
      val ours = explicitShaper.shape(text)
      val line = shaper.shapeLine(text, skiaFont)
      val worst = (0 to text.length).map(o => math.abs(ours.coord(o) - line.getCoordAtOffset(o))).max
      ours.close(); line.close()
      worst.toDouble
    }
    val probeText = "Lorem ipsum dolor sit amet."
    val probeOurs = explicitShaper.shape(probeText)
    val probeLine = shaper.shapeLine(probeText, skiaFont)
    Report.note(
      "shaping.parity.explicit_vs_shapeline_probe",
      (0 to probeText.length)
        .map(o => f"$o:${probeOurs.coord(o)}%.1f/${probeLine.getCoordAtOffset(o)}%.1f")
        .mkString(" ")
    )
    paragraphs.headOption.foreach { text =>
      val ours  = explicitShaper.shape(text)
      val line  = shaper.shapeLine(text, skiaFont)
      val worst = (0 to text.length).maxBy(o => math.abs(ours.coord(o) - line.getCoordAtOffset(o)))
      Report.note(
        "shaping.parity.explicit_vs_shapeline_worst_offset",
        f"paragraph0 len=${text.length} offset=$worst char='${text.lift(worst).getOrElse('$')}' " +
          f"ours=${ours.coord(worst)}%.1f shapeline=${line.getCoordAtOffset(worst)}%.1f " +
          f"ours_width=${ours.width}%.1f shapeline_width=${line.getWidth}%.1f"
      )
    }
    Report.result(
      "shaping.parity.explicit_runs_vs_shapeline_max_caret_diff_px",
      explicitVsTextLine,
      "(same Skia font; checks the caret array the renderer reads; px, not ms)"
    )
    val fonts = FontCollection()
    fonts.setDefaultFontManager(FontMgr.Companion.getDefault())
    val textStyle = TextStyle()
    textStyle.setFontSize(Geometry.FontSize)
    textStyle.setFontFamilies(Array("serif"))
    textStyle.setColor(0xff000000)
    val paragraphStyle = ParagraphStyle()
    paragraphStyle.setTextStyle(textStyle)
    def layoutParagraph(text: String) =
      val builder = ParagraphBuilder(paragraphStyle, fonts)
      builder.pushStyle(textStyle)
      builder.addText(text)
      val built = builder.build()
      builder.close()
      built.layout(Geometry.WrapWidthPx.toFloat)
    pass("skia.paragraph_build_layout_cache_on", options.shapingIterations, "(ParagraphCache hits: same 300 texts every pass)") {
      paragraphs.foreach(text => layoutParagraph(text).close())
    }
    fonts.getParagraphCache.setEnabled(false)
    pass("skia.paragraph_build_layout", options.shapingIterations, "(cache off; Skia's own wrap; no per-glyph array exposed)") {
      paragraphs.foreach(text => layoutParagraph(text).close())
    }
    val primitive = Shaper.Companion.makePrimitive()
    pass("skia.primitive_shaper_textblob", options.shapingIterations, "(no HarfBuzz: no kerning/ligatures)") {
      paragraphs.foreach(text => Option(primitive.shape(text, skiaFont)).foreach(_.close()))
    }
    val dontWrap = Shaper.Companion.makeShapeDontWrapOrReorder()
    pass("skia.shape_dont_wrap_textblob", options.shapingIterations, "(HarfBuzz, no line breaking)") {
      paragraphs.foreach(text => Option(dontWrap.shape(text, skiaFont)).foreach(_.close()))
    }
    pass("skia.paragraph_carets_via_rects_for_range", math.max(3, options.shapingIterations / 5), "(getRectsForRange per char)") {
      paragraphs.foreach { text =>
        val laidOut = layoutParagraph(text)
        (0 until text.length).foreach(i =>
          laidOut.getRectsForRange(i, i + 1, RectHeightMode.TIGHT, RectWidthMode.TIGHT)
        )
        laidOut.close()
      }
    }

    parity("logical_serif", paragraphs, awtFont, frc, skiaFont, layoutParagraph)
    val file = java.io.File(options.fontFile)
    if file.isFile then
      val sameAwt  = java.awt.Font.createFont(java.awt.Font.TRUETYPE_FONT, file).deriveFont(Geometry.FontSize)
      val sameSkia = Font(FontMgr.Companion.getDefault().makeFromFile(file.getPath, 0), Geometry.FontSize)
      sameSkia.setSubpixel(true)
      sameSkia.setEdging(FontEdging.ANTI_ALIAS)
      parity("same_font_file", paragraphs, sameAwt, frc, sameSkia, layoutParagraph)
      val linear = sameSkia.makeWithSize(Geometry.FontSize)
      linear.setSubpixel(true)
      linear.setLinearMetrics(true)
      linear.setHinting(org.jetbrains.skia.FontHinting.NONE)
      parity("same_font_file_skia_unhinted_linear", paragraphs, sameAwt, frc, linear, layoutParagraph)
    else Report.note("shaping.parity.same_font_file", s"skipped: ${options.fontFile} not found")
    clusterProbe(shaper, skiaFont)

  /** How far Serenity's Java2D caret stops sit from Skia's for the same text, and whether the two wraps agree. */
  private def parity(
    label: String,
    paragraphs: Vector[String],
    awtFont: java.awt.Font,
    frc: java.awt.font.FontRenderContext,
    skiaFont: Font,
    layoutParagraph: String => org.jetbrains.skia.paragraph.Paragraph
  ): Unit =
    val shaper = Shaper.Companion.make()
    val diffs = paragraphs.flatMap { text =>
      SpikeLayoutBridge.glyphAdvanceCarets(text, awtFont, frc).map { carets =>
        val line  = shaper.shapeLine(text, skiaFont)
        val skia  = (0 to text.length).map(offset => line.getCoordAtOffset(offset))
        val worst = (0 to text.length).map(offset => math.abs(carets(offset) - skia(offset))).max
        val perAdvance = (0 until text.length).map(i =>
          math.abs((carets(i + 1) - carets(i)) - (skia(i + 1) - skia(i)))
        )
        line.close()
        (worst.toDouble, perAdvance.sum.toDouble / text.length, carets(text.length).toDouble, skia(text.length).toDouble)
      }
    }
    Report.result(
      s"shaping.parity.$label.max_caret_diff_px_per_paragraph",
      diffs.map(_._1),
      s"(awt=${awtFont.getFontName} vs skia=${skiaFont.getTypeface.getFamilyName}; logical px, not ms)"
    )
    Report.result(s"shaping.parity.$label.mean_abs_advance_diff_px", diffs.map(_._2), "(per character, logical px)")
    Report.note(
      s"shaping.parity.$label.total_width_ratio",
      f"skia/awt=${diffs.map(_._4).sum / diffs.map(_._3).sum}%.5f over ${diffs.length} paragraphs"
    )
    val rowMismatches = paragraphs.zipWithIndex.count { (text, line) =>
      val serenityRows = SpikeLayoutBridge.wrapLine(text, line, Geometry.WrapWidthPx, awtFont, frc).length
      val laidOut      = layoutParagraph(text)
      val skiaRows     = laidOut.getLineNumber
      laidOut.close()
      serenityRows != skiaRows
    }
    Report.note(
      s"shaping.parity.$label.wrap_row_count_mismatches",
      s"$rowMismatches of ${paragraphs.length} paragraphs (Serenity wrap vs Skia Paragraph at the same width)"
    )

  /** Whether Skia's clusters index UTF-16 units (what Serenity columns are) or UTF-8 bytes. */
  private def clusterProbe(shaper: Shaper, font: Font): Unit =
    val probe   = "aéb中c"
    val handler = CollectingRunHandler()
    shaper.shape(probe, font, ShapingOptions.Companion.getDEFAULT(), Float.MaxValue, handler)
    val blob = shaper.shape(probe, font)
    val blobClusters = Option(blob).map(_.getClusters.mkString(",")).getOrElse("<null blob>")
    Report.note(
      "shaping.cluster_probe",
      s"text=a,e-acute,b,CJK,c utf16_offsets=0,1,2,3,4 utf8_offsets=0,1,3,4,7 " +
        s"runhandler_clusters=${handler.clusters.mkString(",")} textblob_clusters=$blobClusters"
    )

/** Collects what Skia's shaper reports per run: glyph ids, positions and cluster (source offset) per glyph. */
final class CollectingRunHandler extends RunHandler:
  val clusters: scala.collection.mutable.ArrayBuffer[Int] = scala.collection.mutable.ArrayBuffer.empty
  private var glyphCount = 0
  def beginLine(): Unit                          = ()
  def runInfo(info: RunInfo): Unit               = ()
  def commitRunInfo(): Unit                      = ()
  def runOffset(info: RunInfo): Point            = Point(0f, 0f)
  def commitLine(): Unit                         = ()
  def commitRun(info: RunInfo, glyphs: Array[Short], positions: Array[Point], runClusters: Array[Int]): Unit =
    glyphCount += glyphs.length
    if clusters.length < 64 then clusters ++= runClusters.take(64)
