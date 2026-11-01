package com.serenity.ui.layout

import java.awt.Font

import com.serenity.ui.fonts.FontLoader

/** Whether shaping a run of printable ASCII with ligatures asked for ever sets it at a width other than the sum of its
  * characters' nominal advances, or off the baseline (#2447 B2, ligatures-on variant). That is what the row count
  * depends on: the wrap accepts a substitution in a uniform-advance font when the shaped width equals the nominal sum
  * and the glyphs sit on the line. Enumerates every pair and triple of printable ASCII and every 4-gram of the symbol
  * characters. Test-only, no product code.
  */
object LigatureWidthProbe:

  private val frc       = TextLayoutSnapshot.defaultFontRenderContext()
  private val printable = (0x20 to 0x7e).map(_.toChar).toArray
  private val symbols   = printable.filterNot(char => char.isLetterOrDigit || char == ' ')
  private val tolerance = 0.01f

  final private case class Tally(grams: Long, substituted: Long, deviating: Long, examples: List[String])

  private def survey(font: Font, advance: Float, alphabet: Array[Char], length: Int): (Tally, Double) =
    val started = System.nanoTime()
    val chars   = new Array[Char](length)
    var grams, substituted, deviating = 0L
    var examples                      = List.empty[String]
    def visit(position: Int): Unit =
      if position == length then
        grams += 1
        val nominal = font.createGlyphVector(frc, chars)
        val shaped  = font.layoutGlyphVector(frc, chars, 0, length, Font.LAYOUT_LEFT_TO_RIGHT)
        val count   = shaped.getNumGlyphs
        val codesDiffer =
          count != nominal.getNumGlyphs ||
            !java.util.Arrays.equals(nominal.getGlyphCodes(0, count, new Array[Int](count)), shaped.getGlyphCodes(0, count, new Array[Int](count)))
        if codesDiffer then substituted += 1
        val width    = shaped.getGlyphPosition(count).getX.toFloat
        val offLine  = (0 to count).exists(index => math.abs(shaped.getGlyphPosition(index).getY) > 0.001)
        val offWidth = math.abs(width - advance * length) > tolerance * length
        if offLine || offWidth then
          deviating += 1
          if examples.size < 5 then examples = String(chars) :: examples
      else
        alphabet.foreach { char =>
          chars(position) = char
          visit(position + 1)
        }
    visit(0)
    (Tally(grams, substituted, deviating, examples.reverse), (System.nanoTime() - started) / 1e9)

  private def report(label: String, outcome: (Tally, Double)): Unit =
    val (tally, seconds) = outcome
    println(
      f"    $label%-36s n-grams ${tally.grams}%9d  with another glyph ${tally.substituted}%7d  off the width or baseline ${tally.deviating}%5d  ${seconds}%6.1f s" +
        (if tally.examples.nonEmpty then s"  e.g. ${tally.examples.map(example => s"'$example'").mkString(" ")}" else "")
    )

  def main(args: Array[String]): Unit =
    val defaults = FontLoader.FontConfig()
    println(s"java ${System.getProperty("java.version")}, ${System.getProperty("os.name")}; fractionalMetrics=${frc.usesFractionalMetrics}")
    for size <- List(12.0f, 14.0f, 16.0f) do
      val font    = FontLoader.previewCodeFont(defaults.copy(fontSize = size, enableLigatures = true))
      val advance = font.createGlyphVector(frc, "0").getGlyphMetrics(0).getAdvance
      println(f"  bundled code font $size%.1f pt, ligatures ON, advance $advance%.4f px, hasLayoutAttributes=${font.hasLayoutAttributes}")
      report("pairs, all printable ASCII", survey(font, advance, printable, 2))
      report("triples, all printable ASCII", survey(font, advance, printable, 3))
      if size == 12.0f then report("4-grams, symbols only", survey(font, advance, symbols, 4))
