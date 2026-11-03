package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.ui.renderer.ShapedTextCache

/** The [[NominalAdvances]] of a font set with ligatures, which shaping may give other glyphs. A row count depends on a
  * run's width and baseline and not on which glyph is drawn, and in a monospaced ligature font no substitution changes
  * either; but that is a property of the font, not a guarantee of the platform, so it is checked rather than assumed.
  * The certificate is granted only if
  *
  *   - the font advances every glyph alike and its layout advance is the sum of its glyphs';
  *   - shaping every pair of printable ASCII, and every triple of symbols and space (where operator ligatures live),
  *     leaves each run at the sum of its characters' advances and on the baseline;
  *   - a battery of code-like lines counted by the table agrees with wrapping them, at widths either side of each
  *     line's own.
  *
  * Any deviation refuses the font: every line is then wrapped, as without a certificate. Runs longer than three
  * characters are not enumerated, so the certificate is evidence, not proof; `UiSceneBuilder` checks it against the
  * rows actually painted.
  */
private[layout] object LigatureCertificate:

  final case class ShapedRun(width: Float, offBaseline: Boolean)

  /** Shapes `chars` as the layout does for a font asking for ligatures; must not keep the array. */
  type Shaper = (Font, FontRenderContext, Array[Char]) => ShapedRun

  /** The rows a wrap gives `text` at a width of the given pixels. */
  type RowsOf = (String, Int) => Int

  private val WidthTolerancePerCharPx = 0.01f
  private val BaselineTolerancePx     = 0.001

  val javaShaper: Shaper = (font, frc, chars) =>
    val glyphs = font.layoutGlyphVector(frc, chars, 0, chars.length, Font.LAYOUT_LEFT_TO_RIGHT)
    val count  = glyphs.getNumGlyphs
    ShapedRun(
      glyphs.getGlyphPosition(count).getX.toFloat,
      (0 to count).exists(index => math.abs(glyphs.getGlyphPosition(index).getY) > BaselineTolerancePx)
    )

  def of(font: Font, frc: FontRenderContext): Option[NominalAdvances] = of(font, frc, javaShaper)

  def of(font: Font, frc: FontRenderContext, shaper: Shaper): Option[NominalAdvances] =
    of(font, frc, shaper, wrapRows(font, frc))

  def of(font: Font, frc: FontRenderContext, shaper: Shaper, rowsOf: RowsOf): Option[NominalAdvances] =
    val shapedText = ShapedTextCache()
    NominalAdvances
      .ofLayoutAdvances(font, frc, shapedText)
      .filter(_ => GlyphAdvances.advancesAreUniform(font, frc, shapedText.probes))
      .filter(keepsWidth(_, font, frc, shaper))
      .filter(agreesWithWrap(_, rowsOf))

  private val Printable    = (0x20 to 0x7e).map(_.toChar).toArray
  private val SymbolsSpace = Printable.filterNot(char => char.isLetterOrDigit)

  private def keepsWidth(table: NominalAdvances, font: Font, frc: FontRenderContext, shaper: Shaper): Boolean =
    runsOf(Printable, 2).forall(faithful(_, table, font, frc, shaper)) &&
      runsOf(SymbolsSpace, 3).forall(faithful(_, table, font, frc, shaper))

  private def runsOf(alphabet: Array[Char], length: Int): Iterator[Array[Char]] =
    val total = math.pow(alphabet.length.toDouble, length.toDouble).toInt
    Iterator.range(0, total).map { index =>
      Array.tabulate(length)(position =>
        alphabet((index / math.pow(alphabet.length.toDouble, (length - 1 - position).toDouble).toInt) % alphabet.length)
      )
    }

  private def faithful(
    chars: Array[Char],
    table: NominalAdvances,
    font: Font,
    frc: FontRenderContext,
    shaper: Shaper
  ): Boolean =
    val run = shaper(font, frc, chars)
    val sum = chars.foldLeft(0.0f)((width, char) => width + table.advanceOf(char))
    !run.offBaseline && math.abs(run.width - sum) <= WidthTolerancePerCharPx * chars.length

  private def agreesWithWrap(table: NominalAdvances, rowsOf: RowsOf): Boolean =
    batteryLines.forall { text =>
      widthsAround(text, table).forall(width => !table.fitsOneRow(text, width) || rowsOf(text, width) == 1)
    }

  private def widthsAround(text: String, table: NominalAdvances): List[Int] =
    val total = text.foldLeft(0.0f)((width, char) => width + table.advanceOf(char))
    List(math.floor(total).toInt - 1, math.floor(total).toInt, math.ceil(total).toInt, math.ceil(total).toInt + 1)
      .map(math.max(1, _))
      .distinct

  private def wrapRows(font: Font, frc: FontRenderContext): RowsOf =
    val cache   = WrappedLineCache.bounded()
    val metrics = CellMetrics.fromFont(font)
    (text, widthPx) =>
      TextLayoutSnapshot
        .boundedVisualLinesForText(text, 0, widthPx, font, frc, cellMetricsOverride = Some(metrics), wrapCache = cache)
        .length
        .max(1)

  private val batteryPool = List(
    "  val result = items.filter(_.size > 3).map(_.name) // fold -> total",
    "case Some(value) if value >= 0 && value != 1 => value |> f >>= g",
    "fn parse<'a>(input: &'a str) -> Result<Vec<Token>, Error> { Ok(vec![]) }",
    "if (a !== b && c?.d ?? e) { return x => y === z ? 1 : 2; } // <!-- -->",
    "main = print $ foldr (\\x acc -> x : acc) [] <$> xs <*> ys >>= return",
    "SELECT a::int, b->>'k' FROM t WHERE x <> 1 AND y <= 2 OR z >= 3; -- note",
    "std::map<int, std::string>::iterator it = m.begin(); it != m.end(); ++it",
    "let f = fun x -> x |> List.map (fun y -> y + 1) |> List.sum (* total *)"
  ).mkString("  ")

  private val batteryLines: List[String] =
    (1 to 200).map(length => batteryPool.drop(length * 7 % batteryPool.length).take(length)).toList
