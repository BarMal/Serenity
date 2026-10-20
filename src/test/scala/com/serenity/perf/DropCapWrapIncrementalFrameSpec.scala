package com.serenity.perf

import com.serenity.keystroke.events.{DeleteBackward, InsertChar}
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The rows beside a drop cap wrap at the panel width less the indent, so an edit that moves the first row's break, or
  * the break of a row beside the glyph, must repaint every row it moves: each frame drawn after an edit is what a cold
  * frame draws.
  */
class DropCapWrapIncrementalFrameSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  private val scales = List(1.0, 2.0)

  private def words(count: Int, seed: String): String =
    (1 to count).map(n => s"${seed}word$n").mkString(" ")

  private def document(lines: Int): RichTextDocument =
    RichTextDocument(
      (1 to 6).toList.flatMap { chapter =>
        List(
          RichTextParagraph(List(RichTextRun(words(60, s"c$chapter"))), role = ParagraphRole.dropCap(lines)),
          RichTextParagraph(List(RichTextRun(words(40, s"b$chapter"))))
        )
      }
    )

  for
    scale <- scales
    lines <- List(2, 3)
  do
    s"Typing in the rows beside a $lines-line drop cap at ${scale}x" should "draw what a cold frame draws at every step" in {
      val session = Session(document(lines), scale, baseConfig)
      List(0, 1, 12, 30, 60, 110).foreach { column =>
        session.placeCursor(0, column)
        "abc".foreach(char => session(InsertChar(char), s"type '$char' at column $column"))
      }
      session.mismatches shouldBe empty
    }

    s"Deleting across the break of the first row of a $lines-line drop cap at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(document(lines), scale, baseConfig)
        session.placeCursor(0, 40)
        (1 to 30).foreach(n => session(DeleteBackward, s"delete $n"))
        "wide wide wide wide".foreach(char => session(InsertChar(char), s"retype '$char'"))
        session.mismatches shouldBe empty
      }
