package com.serenity.ui.layout

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, Viewport}
import com.serenity.testkit.TestMeasure
import com.serenity.ui.layout.TextMeasureCorpus.PinnedFont

/** The layout of a fixed multi-line fixture in the bundled monospaced fonts, as the AWT measurement laid it out before
  * layout moved to the Skia measurer (#2660 item 2, step D2): every row's columns, height, ascent and caret xs, and
  * each snapshot's line height. A monospaced font has nothing for the move to change, so these numbers are pinned
  * exactly, floats by their shortest round-tripping text.
  */
private[layout] object MonospacedLayoutPin:

  given Balance = Balance.default

  val Resource = "/text-measure/monospaced-pin.tsv"

  val Labels: Vector[String] = Vector("courier_prime", "courier_prime_bold", "monaspace_neon")

  private val WidthsPx = Vector(240, 400, 100_000)

  val Fixture: String =
    Vector(
      "object Main extends App:",
      "  val value = compute(input, 42) // a plain ASCII code line that is long enough to wrap",
      "\tdef tabbed(x: Int)\t= x * 2\t// tabs between columns",
      "",
      "The quick brown fox jumps over the lazy dog. Typography affects every Wave and Vase; Tomorrow, after " +
        "fifteen office coffees, ffi and fl and fi join, then more words to fill the rows until there are many.",
      "AV To Tj Wa Yo Th: pairs a proportional font would kern, café naïve résumé",
      "if (a != b && c <= d || e >= f) { return x -> y } // operators",
      "antidisestablishmentarianismantidisestablishmentarianismantidisestablishmentarianism"
    ).mkString("\n")

  def fonts: Vector[PinnedFont] = TextMeasureCorpus.fonts.filter(font => Labels.contains(font.label))

  private def snapshot(pinned: PinnedFont, widthPx: Int): TextLayoutSnapshot =
    val buffer = Buffer.fromString(BufferId(1), Fixture)
    TextLayoutSnapshot.fromBuffer(
      buffer.copy(viewport = Viewport(visibleLines = 1000, visibleColumns = 1000)),
      panelWidthPx = widthPx,
      pinned.font,
      wordWrapEnabled = widthPx != WidthsPx.last,
      wrapCache = TestMeasure.wrapCache(),
      fallback = pinned.fallback
    )

  private def floats(values: Seq[Float]): String = values.map(_.toString).mkString(",")

  def entries(): Vector[(String, String)] =
    for
      pinned  <- fonts
      widthPx <- WidthsPx
      entry <-
        val laidOut = snapshot(pinned, widthPx)
        val prefix  = s"${pinned.label}@$widthPx"
        Vector(
          s"$prefix.snapshot" -> Vector(
            laidOut.lineHeightPx,
            laidOut.ascentPx,
            if laidOut.usesMeasuredLayout then 1 else 0,
            laidOut.visualLines.length
          ).mkString(",")
        ) ++ laidOut.visualLines.zipWithIndex.flatMap { (line, index) =>
          Vector(
            s"$prefix.row$index" ->
              Vector(line.bufferLine, line.startColumn, line.endColumn, line.heightPx, line.row.ascentPx).mkString(","),
            s"$prefix.row$index.carets" -> floats(line.row.caretXs.toVector),
            s"$prefix.row$index.width"  -> line.row.widthPx.toString
          )
        }
    yield entry

  def render(entries: Seq[(String, String)]): String =
    "# Monospaced layout pinned by MonospacedLayoutPinSpec, as AWT laid it out before step D2 (#2660 item 2).\n" +
      "# Must not move: a monospaced font has nothing the move to the Skia measurer may change.\n" +
      entries.map((key, values) => s"$key\t$values").mkString("", "\n", "\n")

  def load(): Option[Vector[(String, String)]] =
    Option(getClass.getResourceAsStream(Resource)).map { stream =>
      val text =
        try new String(stream.readAllBytes(), StandardCharsets.UTF_8)
        finally stream.close()
      text.linesIterator
        .filterNot(line => line.isEmpty || line.startsWith("#"))
        .map(line => line.split('\t'))
        .collect { case Array(key, values) => key -> values; case Array(key) => key -> "" }
        .toVector
    }

  def main(args: Array[String]): Unit =
    val target = Paths.get(args.headOption.getOrElse("monospaced-pin.tsv"))
    val _      = Files.write(target, render(entries()).getBytes(StandardCharsets.UTF_8))
