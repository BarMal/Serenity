package com.serenity.ui.layout

import scala.annotation.tailrec

import com.serenity.ui.layout.TextMeasurer.LineFonts
import com.serenity.ui.renderer.FontSpec

/** The advance of each printable ASCII character in a font whose shaping sets none of them otherwise: the measurer
  * finds every character of every pair of them, and of every triple of symbols and space (where operator ligatures
  * live), context-free, so no kerning pair, ligature or contextual substitution moves one. A line written in such
  * characters then measures to the sum of these advances, to the float, which is what the wrap reads for it
  * ([[GlyphAdvances.measure]]), so whether it fits a row can be read off them without wrapping the line. They are
  * fractional and need not agree with the integer cell grid. Runs longer than three characters are not enumerated, so
  * the table is evidence, not proof; `UiSceneBuilder` checks it against the rows actually painted.
  */
final private[serenity] class NominalAdvances private (advances: Array[Float]):

  def advanceOf(char: Char): Float = advances(char - NominalAdvances.FirstPrintable)

  /** The same table with every advance multiplied by `factor`: a certificate that is wrong by construction, for specs
    * of what happens when one is.
    */
  private[serenity] def scaled(factor: Float): NominalAdvances = new NominalAdvances(advances.map(_ * factor))

  /** Whether `text` is one row of `widthPx`: it is printable ASCII and its summed advance clears the width by the
    * margin within which the wrap itself declines to decide and measures the row exactly. Anything else is not said to
    * be one row, however short.
    */
  def fitsOneRow(text: String, widthPx: Int): Boolean =
    @tailrec
    def total(index: Int, sum: Float): Float =
      if index >= text.length then sum
      else
        val slot = text.charAt(index) - NominalAdvances.FirstPrintable
        if slot < 0 || slot >= advances.length then NominalAdvances.NotPrintable
        else total(index + 1, sum + advances(slot))
    val width = total(0, 0.0f)
    width >= 0.0f && width + ParagraphMeasurement.DecisionGuardPx < widthPx.toFloat

private[serenity] object NominalAdvances:

  /** The font a table is measured for; the owner's measurer is the rest of what it depends on. */
  final case class Key(font: FontSpec)

  private val FirstPrintable = 0x20
  private val LastPrintable  = 0x7e
  private val NotPrintable   = -1.0f

  private val Printable    = (FirstPrintable to LastPrintable).map(_.toChar).toVector
  private val SymbolsSpace = Printable.filterNot(_.isLetterOrDigit)

  /** Every pair of printable characters and every triple of symbols and space, in runs of whole pairs and triples. A
    * run is shaped with float positions, which past about 130,000 px cannot tell the hundredth of a pixel that
    * [[GlyphAdvances.ShapingTolerancePx]] allows, so the battery is not one run; each run is short enough to hold the
    * measurer's lock only briefly.
    */
  private lazy val Battery: Vector[String] =
    val pairs   = for first <- Printable; second <- Printable yield s"$first$second"
    val triples = for a <- SymbolsSpace; b <- SymbolsSpace; c <- SymbolsSpace yield s"$a$b$c"
    (pairs ++ triples).grouped(ElementsPerRun).map(_.mkString).toVector

  private val ElementsPerRun = 333

  /** The table for `font`, or `None` when the measurer may set some character at an advance other than its own. */
  def of(font: FontSpec, measurer: TextMeasurer): Option[NominalAdvances] =
    table(font, measurer).filter(_ =>
      Battery.forall(run =>
        measurer.mayMeasureByAdvances(run) &&
          measurer.advances(run, 0, run.length, 0, LineFonts(font, Vector.empty)).isContextFree(0, run.length)
      )
    )

  /** Each printable character's own advance in `font`, whatever shaping may do to a run of them. The caller answers for
    * that: see [[LigatureCertificate]].
    */
  private[layout] def table(font: FontSpec, measurer: TextMeasurer): Option[NominalAdvances] =
    val alphabet = Printable.mkString
    val each     = measurer.advances(alphabet, 0, alphabet.length, 0, LineFonts(font, Vector.empty))
    val advances = Array.tabulate(alphabet.length)(index => each.caretsFrom(index, 1)(1))
    Option.when(advances.forall(_ > 0.0f))(new NominalAdvances(advances))
