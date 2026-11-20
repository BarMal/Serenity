package com.serenity.ui.skia

import org.jetbrains.skia.Font

/** What a font's shaping makes of printable ASCII, read off it once: each character's advance, and for each pair of
  * characters whether the first is set in a glyph of its own and how far the second pulls it. A paragraph of such text
  * in a font that sets it glyph for glyph is then measured by reading the table, where shaping it would tell no more:
  * the table is read off the very shaping and pair measures a stretch is measured by, over every pair of characters.
  * Text with a character or a pair the font does not set that way is left to the stretch's own measure.
  */
final private[skia] class AsciiAdvanceTable private (
    advance: Array[Float],
    alone: Array[Boolean],
    pairMeasurable: Array[Boolean],
    pairKern: Array[Float]
):

  /** Fills the measure of `chars` and says so, or leaves the arrays as they are when a character or a pair of them is
    * not measurable from the table.
    */
  def measure(
    chars: String,
    offset: Int,
    advances: Array[Float],
    kernAfter: Array[Float],
    measurable: Array[Boolean]
  ): Boolean =
    val last = chars.length - 1

    def pairAt(index: Int): Int =
      (chars.charAt(index) - AsciiAdvanceTable.First) * AsciiAdvanceTable.Span +
        (chars.charAt(index + 1) - AsciiAdvanceTable.First)

    @annotation.tailrec
    def allMeasurable(index: Int): Boolean =
      index > last ||
        ((if index == last then alone(chars.charAt(index) - AsciiAdvanceTable.First)
          else pairMeasurable(pairAt(index))) &&
          allMeasurable(index + 1))

    @annotation.tailrec
    def fill(index: Int): Unit =
      if index <= last then
        advances(offset + index) = advance(chars.charAt(index) - AsciiAdvanceTable.First)
        measurable(offset + index) = true
        if index < last then kernAfter(offset + index) = pairKern(pairAt(index))
        fill(index + 1)

    val fits = allMeasurable(0)
    if fits then fill(0)
    fits

private[skia] object AsciiAdvanceTable:

  private val First = ' '.toInt
  private val Last  = '~'.toInt
  private val Span  = Last - First + 1

  /** Below this a stretch is shaped as it is: reading the table of a font nobody has measured yet costs more than a
    * short text.
    */
  val SmallestStretch = 24

  def isPrintableAscii(chars: String): Boolean =
    @annotation.tailrec
    def scan(index: Int): Boolean =
      index >= chars.length || (chars.charAt(index) >= First && chars.charAt(index) <= Last && scan(index + 1))
    scan(0)

  def read(
    font: Font,
    shapeOf: (String, Font) => ShapedGlyphs,
    pairKerning: (Font, Char, Char) => Float
  ): AsciiAdvanceTable =
    val characters = (First to Last).map(_.toChar)
    val probe      = characters.flatMap(first => characters.map(second => s"$first$second")).mkString
    val advances   = new Array[Float](probe.length)
    val kernAfter  = new Array[Float](probe.length)
    val measurable = new Array[Boolean](probe.length)
    StretchAdvances.measure(probe, font, 0, advances, kernAfter, measurable, shapeOf, pairKerning, skipRuns = true)
    val firstOf = (index: Int) => index * Span * 2
    new AsciiAdvanceTable(
      Array.tabulate(Span)(index => advances(firstOf(index))),
      Array.tabulate(Span)(index => measuredAlone(characters(index).toString, font, shapeOf, pairKerning)),
      Array.tabulate(Span * Span)(pair => measurable(pair * 2)),
      Array.tabulate(Span * Span)(pair => kernAfter(pair * 2))
    )

  private def measuredAlone(
    text: String,
    font: Font,
    shapeOf: (String, Font) => ShapedGlyphs,
    pairKerning: (Font, Char, Char) => Float
  ): Boolean =
    val advances   = new Array[Float](1)
    val kernAfter  = new Array[Float](1)
    val measurable = new Array[Boolean](1)
    StretchAdvances.measure(text, font, 0, advances, kernAfter, measurable, shapeOf, pairKerning, skipRuns = true)
    measurable(0)
