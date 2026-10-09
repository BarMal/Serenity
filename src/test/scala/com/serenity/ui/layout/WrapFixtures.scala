package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.TextAttribute

import scala.util.Random

import com.serenity.state.models.TextVisualLine
import com.serenity.ui.fonts.FontLoader
import org.scalatest.Assertions

/** Layouts, prose and edits for checking [[IncrementalWrap]] against the cold wrap. */
object WrapFixtures extends Assertions:

  val frc = TextLayoutSnapshot.defaultFontRenderContext()

  /** How many random cases each property runs; the default keeps `sbt test` quick. */
  val Cases: Int = sys.env.get("INCREMENTAL_WRAP_CASES").flatMap(_.toIntOption).getOrElse(2000)

  final case class Layout(
      name: String,
      font: Font,
      forceCellLayout: Boolean = false,
      cellMetrics: Option[CellMetrics] = None
  ):
    def charWidthPx: Int = cellMetrics.getOrElse(CellMetrics.fromFont(font)).charWidth.max(1)

  private def derived(font: Font, attributes: (TextAttribute, AnyRef)*): Font =
    font.deriveFont(java.util.Map.copyOf(java.util.Map.ofEntries(attributes.map((k, v) => java.util.Map.entry(k, v))*)))

  val defaultProse: Font = FontLoader.previewTextFont(FontLoader.FontConfig())

  val layouts: Vector[Layout] = Vector(
    Layout("mono-cells", Font(Font.MONOSPACED, Font.PLAIN, 12), forceCellLayout = true),
    Layout("unit-cells", Font(Font.MONOSPACED, Font.PLAIN, 12), forceCellLayout = true, Some(CellMetrics(1, 1, 0))),
    Layout(
      "display-width-cells",
      Font(Font.MONOSPACED, Font.PLAIN, 12),
      forceCellLayout = true,
      Some(CellMetrics(1, 1, 0, true))
    ),
    Layout("sans", Font(Font.SANS_SERIF, Font.PLAIN, 13)),
    Layout("serif", Font(Font.SERIF, Font.PLAIN, 14)),
    Layout("default-prose", defaultProse),
    Layout(
      "liberation-serif-kerned",
      derived(Font("Liberation Serif", Font.PLAIN, 14), TextAttribute.KERNING -> TextAttribute.KERNING_ON)
    ),
    Layout(
      "serif-kerned-ligatured",
      derived(
        Font(Font.SERIF, Font.PLAIN, 15),
        TextAttribute.KERNING   -> TextAttribute.KERNING_ON,
        TextAttribute.LIGATURES -> TextAttribute.LIGATURES_ON
      )
    )
  )

  /** The key the cache wraps `text` under in `layout` at `widthPx`. */
  def keyFor(text: String, layout: Layout, widthPx: Int, baseColumn: Int = 0): WrappedLineKey =
    val cellMetrics = layout.cellMetrics.getOrElse(CellMetrics.fromFont(layout.font))
    val measured    = !layout.forceCellLayout && TextLayoutSnapshot.shouldUseMeasuredLayout(layout.font, frc)
    WrappedLineKey(
      text,
      widthPx,
      TextCaretMeasurement.singleFontResolver(layout.font),
      frc,
      measured,
      cellMetrics,
      baseColumn,
      com.serenity.richtext.ParagraphRole.Body,
      0.0f
    )

  def wrap(
    text: String,
    cache: WrappedLineCache,
    layout: Layout,
    widthPx: Int,
    bufferLine: Int = 0,
    baseColumn: Int = 0,
    maxVisualLines: Int = Int.MaxValue
  ): Vector[TextVisualLine] =
    TextLayoutSnapshot.boundedVisualLinesForText(
      text,
      bufferLine,
      widthPx,
      layout.font,
      frc,
      baseColumn = baseColumn,
      maxVisualLines = maxVisualLines,
      cellMetricsOverride = layout.cellMetrics,
      forceCellLayout = layout.forceCellLayout,
      wrapCache = cache
    )

  /** Every field of every row, floats as raw bits, so equality means byte for byte. */
  def bitwise(rows: Vector[TextVisualLine]): Vector[Any] =
    def bits(x: Float): Int = java.lang.Float.floatToRawIntBits(x)
    rows.map { row =>
      (
        row.bufferLine,
        row.startColumn,
        row.endColumn,
        row.text,
        bits(row.widthPx),
        row.caretStops.map(stop => (stop.column, bits(stop.xPx))),
        bits(row.xOffsetPx),
        row.xSortedCaretStops.map(stop => (stop.column, bits(stop.xPx))),
        row.heightPx,
        row.ascentPx
      )
    }

  def assertSameAsCold(
    actual: Vector[TextVisualLine],
    text: String,
    layout: Layout,
    widthPx: Int,
    bufferLine: Int = 0,
    baseColumn: Int = 0
  ): Unit =
    val cold = wrap(text, WrappedLineCache.Uncached, layout, widthPx, bufferLine, baseColumn)
    if bitwise(actual) != bitwise(cold) then
      val firstDifferent = actual.zip(cold).indexWhere((a, c) => bitwise(Vector(a)) != bitwise(Vector(c)))
      fail(
        s"${layout.name} width=$widthPx baseColumn=$baseColumn text=${text.take(300)}...(${text.length}) " +
          s"rows ${actual.length} vs cold ${cold.length}, first differing row $firstDifferent: " +
          s"${actual.lift(firstDifferent)} vs ${cold.lift(firstDifferent)}"
      )

  // -- prose ----------------------------------------------------------------------------------------------------

  private val lower    = "abcdefghijklmnopqrstuvwxyz"
  private val upper    = lower.toUpperCase
  private val accented = "éèêëàâäîïôöùûüçñáíóúøåæœ"
  private val kernable =
    Vector("AV", "To", "Ty", "fi", "fl", "ffi", "WA", "Yo", "Te", "VA", "LT", "P.", "r,", "ff", "ft")
  private val closers = Vector(",", ".", ";", ":", "!", "?", ")", "]", "\"", "'", "”", "’", "...", "),", ".\"")
  private val openers = Vector("(", "[", "\"", "'", "“", "‘", "$", "#", "@", "-")
  private val cjk     = Vector("日本語", "中文混合", "テキスト", "한국어", "你好世界")
  private val emoji   = Vector("🎉", "👍🏽", "🇬🇧", "👨‍👩‍👧")

  private def pick[A](rnd: Random, options: Seq[A]): A = options(rnd.nextInt(options.length))

  private def letters(rnd: Random, count: Int): String =
    (0 until count).map { _ =>
      rnd.nextInt(20) match
        case 0 => accented(rnd.nextInt(accented.length))
        case 1 => upper(rnd.nextInt(upper.length))
        case _ => lower(rnd.nextInt(lower.length))
    }.mkString

  private def word(rnd: Random): String =
    val base = letters(rnd, 1 + rnd.nextInt(12))
    rnd.nextInt(12) match
      case 0 => base.capitalize
      case 1 => base.toUpperCase
      case 2 => base + pick(rnd, kernable) + letters(rnd, rnd.nextInt(4))
      case _ => base

  private def token(rnd: Random, exotic: Boolean): String =
    rnd.nextInt(40) match
      case 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 => word(rnd)
      case 20 | 21 => word(rnd) + pick(rnd, closers)
      case 22      => pick(rnd, openers) + word(rnd) + pick(rnd, closers)
      case 23      => word(rnd) + "-" + word(rnd)
      case 24      => word(rnd) + "/" + word(rnd)
      case 25      => word(rnd) + "—" + word(rnd)
      case 26      => word(rnd) + " " + word(rnd)
      case 27      => s"${1 + rnd.nextInt(999)},${100 + rnd.nextInt(899)}.${rnd.nextInt(99)}"
      case 28      => s"${rnd.nextInt(100)}.${rnd.nextInt(100)}%"
      case 29 =>
        s"https://${word(rnd).toLowerCase}.example.com/${word(rnd).toLowerCase}/${word(rnd).toLowerCase}?q=${rnd.nextInt(99)}"
      case 30           => "(" + rnd.nextInt(2000) + ")"
      case 31           => word(rnd) + "..." + word(rnd)
      case 32           => word(rnd) * (1 + rnd.nextInt(5))
      case 33 if exotic => pick(rnd, cjk)
      case 34 if exotic => pick(rnd, emoji)
      case 35 if exotic => word(rnd) + pick(rnd, cjk) + word(rnd)
      case _            => word(rnd)

  private def separator(rnd: Random): String =
    rnd.nextInt(100) match
      case n if n < 78 => " "
      case n if n < 88 => "  "
      case n if n < 92 => "\t"
      case n if n < 95 => " "
      case n if n < 97 => " - "
      case _           => "   "

  def paragraph(rnd: Random, targetChars: Int, exotic: Boolean = false): String =
    @annotation.tailrec
    def grow(acc: StringBuilder): String =
      if acc.length >= targetChars then acc.toString
      else grow(acc.append(token(rnd, exotic)).append(separator(rnd)))
    val text = grow(new StringBuilder)
    if rnd.nextInt(4) == 0 then text else text.trim

  /** A snippet to type or paste: word fragments, spaces, punctuation. */
  def snippet(rnd: Random, exotic: Boolean = false): String =
    val length = 1 + rnd.nextInt(40)
    val pieces = (0 until 1 + length / 4).map { _ =>
      rnd.nextInt(10) match
        case 0 | 1 | 2 | 3 => letters(rnd, 1 + rnd.nextInt(6))
        case 4 | 5         => " "
        case 6             => pick(rnd, closers ++ openers)
        case 7             => pick(rnd, kernable)
        case 8             => pick(rnd, Vector("\t", " ", "  ", "-", "/", "—", "1,234.5"))
        case _             => if exotic then pick(rnd, cjk ++ emoji) else letters(rnd, 2)
    }
    pieces.mkString.take(length).padTo(1, ' ')

  // -- edits ---------------------------------------------------------------------------------------------------

  final case class Edit(start: Int, removed: Int, inserted: String):
    def applyTo(text: String): String = text.substring(0, start) + inserted + text.substring(start + removed)

  /** An edit placed in one of the regions that matter to wrapping: the ends, a row boundary and its neighbours, a space
    * next to one, the middle of a long word, or anywhere.
    */
  def edit(rnd: Random, text: String, rows: Vector[TextVisualLine], exotic: Boolean = false): Edit =
    val boundaries = rows.map(_.startColumn).filter(_ > 0)
    val spaces     = text.indices.filter(text(_) == ' ')
    val anchor =
      rnd.nextInt(10) match
        case 0                            => 0
        case 1                            => text.length
        case 2 | 3 if boundaries.nonEmpty => pick(rnd, boundaries) + rnd.nextInt(5) - 2
        case 4 if rows.nonEmpty           => rnd.nextInt(rows.head.endColumn.max(1) + 1)
        case 5 if spaces.nonEmpty         => pick(rnd, spaces) + rnd.nextInt(3) - 1
        case 6 if boundaries.nonEmpty =>
          val boundary = pick(rnd, boundaries)
          text.lastIndexOf(' ', boundary) + 1 + rnd.nextInt(3)
        case _ => rnd.nextInt(text.length + 1)
    val start = anchor.max(0).min(text.length)
    rnd.nextInt(3) match
      case 0 => Edit(start, 0, snippet(rnd, exotic))
      case 1 =>
        val removed = (1 + rnd.nextInt(40)).min(text.length - start)
        Edit(start, removed, "")
      case _ =>
        val removed = (1 + rnd.nextInt(20)).min(text.length - start)
        Edit(start, removed, snippet(rnd, exotic))

  def widthPx(rnd: Random, layout: Layout): Int =
    val columns = rnd.nextInt(6) match
      case 0     => 3 + rnd.nextInt(3)
      case 1     => 6 + rnd.nextInt(10)
      case 2 | 3 => 16 + rnd.nextInt(30)
      case 4     => 46 + rnd.nextInt(60)
      case _     => 100 + rnd.nextInt(120)
    columns * layout.charWidthPx + rnd.nextInt(layout.charWidthPx.max(2))

  /** A paragraph of `rows` rows at `widthPx` for `layout`, give or take a third. */
  def paragraphFor(rnd: Random, layout: Layout, widthPx: Int, rows: Int, exotic: Boolean = false): String =
    @annotation.tailrec
    def attempt(charsPerRow: Double, tries: Int): String =
      val text   = paragraph(rnd, (charsPerRow * rows).toInt.max(1), exotic)
      val actual = wrap(text, WrappedLineCache.Uncached, layout, widthPx).length
      if actual >= rows || tries == 0 then text
      else attempt(charsPerRow * (rows.toDouble / actual.max(1)).min(3.0) * 1.1, tries - 1)
    attempt((widthPx / layout.charWidthPx.toDouble * 0.9).max(3.0), 4)
