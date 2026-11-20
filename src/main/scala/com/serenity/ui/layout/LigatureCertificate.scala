package com.serenity.ui.layout

import com.serenity.ui.layout.TextMeasurer.FontRun
import com.serenity.ui.renderer.FontSpec

/** The [[NominalAdvances]] of a font set with ligatures, which shaping gives other glyphs, so its characters are not
  * context-free. A row count depends on a run's width and not on which glyph is drawn, and in a monospaced ligature
  * font no substitution changes the width; but that is a property of the font, not a guarantee of the platform, so it
  * is checked rather than assumed. The certificate is granted only if
  *
  *   - shaping every pair of printable ASCII, and every triple of symbols and space (where operator ligatures live),
  *     leaves each run at the sum of its characters' advances;
  *   - a battery of code-like lines counted by the table agrees with wrapping them, at widths either side of each
  *     line's own.
  *
  * Any deviation refuses the font: every line is then wrapped, as without a certificate. Runs longer than three
  * characters are not enumerated, so the certificate is evidence, not proof; `UiSceneBuilder` checks it against the
  * rows actually painted.
  */
private[layout] object LigatureCertificate:

  /** How wide `text` is shaped as layout shapes it, and whether any glyph of it is set off the baseline. */
  final case class ShapedRun(width: Float, offBaseline: Boolean)

  /** Shapes `text` as the layout does for a font asking for ligatures; it must not keep the text. */
  type Shaper = String => ShapedRun

  /** The rows a wrap gives `text` at a width of the given pixels. */
  type RowsOf = (String, Int) => Int

  private val WidthTolerancePerCharPx = 0.01f
  private val BaselineTolerancePx     = 0.001f

  /** The width the measurer shapes `text` to in `font`, ligatures and all, and whether it sets a glyph off the
    * baseline.
    */
  def measuredShaper(font: FontSpec, measurer: TextMeasurer): Shaper = text =>
    val run = measurer.shape(text, 0, FontRun(0, text.length, font), 0.toByte)
    ShapedRun(run.advancePx, run.ys.exists(y => math.abs(y) > BaselineTolerancePx))

  def of(font: FontSpec, measurer: TextMeasurer): Option[NominalAdvances] =
    of(font, measurer, measuredShaper(font, measurer), wrapRows(font, measurer))

  def of(font: FontSpec, measurer: TextMeasurer, shaper: Shaper, rowsOf: RowsOf): Option[NominalAdvances] =
    NominalAdvances
      .table(font, measurer)
      .filter(keepsWidth(_, shaper))
      .filter(agreesWithWrap(_, rowsOf))

  private val Printable    = (0x20 to 0x7e).map(_.toChar).toArray
  private val SymbolsSpace = Printable.filterNot(char => char.isLetterOrDigit)

  private def keepsWidth(table: NominalAdvances, shaper: Shaper): Boolean =
    runsOf(Printable, 2).forall(faithful(_, table, shaper)) &&
      runsOf(SymbolsSpace, 3).forall(faithful(_, table, shaper))

  private def runsOf(alphabet: Array[Char], length: Int): Iterator[String] =
    val total = math.pow(alphabet.length.toDouble, length.toDouble).toInt
    Iterator.range(0, total).map { index =>
      String(
        Array.tabulate(length)(position =>
          alphabet(
            (index / math.pow(alphabet.length.toDouble, (length - 1 - position).toDouble).toInt) % alphabet.length
          )
        )
      )
    }

  private def faithful(run: String, table: NominalAdvances, shaper: Shaper): Boolean =
    val sum    = run.foldLeft(0.0f)((width, char) => width + table.advanceOf(char))
    val shaped = shaper(run)
    !shaped.offBaseline && math.abs(shaped.width - sum) <= WidthTolerancePerCharPx * run.length

  private def agreesWithWrap(table: NominalAdvances, rowsOf: RowsOf): Boolean =
    batteryLines.forall { text =>
      widthsAround(text, table).forall(width => !table.fitsOneRow(text, width) || rowsOf(text, width) == 1)
    }

  private def widthsAround(text: String, table: NominalAdvances): List[Int] =
    val total = text.foldLeft(0.0f)((width, char) => width + table.advanceOf(char))
    List(math.floor(total).toInt - 1, math.floor(total).toInt, math.ceil(total).toInt, math.ceil(total).toInt + 1)
      .map(math.max(1, _))
      .distinct

  private def wrapRows(font: FontSpec, measurer: TextMeasurer): RowsOf =
    val cache   = WrappedLineCache.bounded(layoutMemos = LayoutMemos(measurerSource = () => Some(measurer)))
    val awt     = font.toAwt
    val metrics = measurer.cellMetrics(font)
    (text, widthPx) =>
      TextLayoutSnapshot
        .boundedVisualLinesForText(text, 0, widthPx, awt, cellMetricsOverride = Some(metrics), wrapCache = cache)
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
