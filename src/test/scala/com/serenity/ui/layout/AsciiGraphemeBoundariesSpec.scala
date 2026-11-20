package com.serenity.ui.layout

import java.text.{BreakIterator, StringCharacterIterator}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** ASCII text is cut into graphemes at every character but between a carriage return and a line feed; the boundaries
  * are read off the text without the break iterator then, and must be the ones the iterator finds.
  */
class AsciiGraphemeBoundariesSpec extends AnyFlatSpec with Matchers:

  private def iteratorsBoundaries(text: String): Vector[Int] =
    val breaks = BreakIterator.getCharacterInstance
    breaks.setText(StringCharacterIterator(text))
    Iterator.iterate(breaks.first())(_ => breaks.next()).takeWhile(_ != BreakIterator.DONE).toVector

  "The grapheme boundaries of ASCII text" should "be the break iterator's, controls and line breaks included" in {
    val scratch = new BoundaryScratch
    val random  = new scala.util.Random(11L)
    val pieces  = Vector("a", "Z", " ", "\t", "\r", "\n", "\r\n", "0", "~", "\u0001", "\u007f", "é", "é")

    (1 to 400).foreach { _ =>
      val text = Vector.fill(1 + random.nextInt(40))(pieces(random.nextInt(pieces.length))).mkString
      withClue(text.map(char => f"\\u${char.toInt}%04x").mkString + ": ") {
        ParagraphMeasurement.graphemeBoundaryOffsets(text, scratch).toVector shouldBe iteratorsBoundaries(text)
      }
    }
  }
