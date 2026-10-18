package com.serenity.perf

import scala.util.Random

import com.serenity.keystroke.events.{DeleteBackward, Enter, Event, InsertChar}
import com.serenity.richtext.{RichTextDocument, RichTextParagraph, RichTextRun}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The window alternates between two images, so the screen shows the frame before the one a surface preserves pixels
  * from. An edit that goes to a state B and back to A must repaint the rows B changed, even though A differs from the
  * image's own previous frame only by the edit that took it back. Frames are compared with cold ones as
  * [[ColdFrameHarness]] describes.
  */
class StaleBackBufferFrameSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  private val scales = List(1.0, 2.0)

  private def words(count: Int, seed: String): String =
    (1 to count).map(n => s"${seed}word$n").mkString(" ")

  private def body(wordCount: Int, seed: String): RichTextParagraph =
    RichTextParagraph(List(RichTextRun(words(wordCount, seed))))

  private def prose: RichTextDocument =
    RichTextDocument((1 to 8).toList.map(n => body(40 + 7 * n, s"p$n")))

  for scale <- scales do
    s"Typing two characters then deleting one across a paragraph at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(prose, scale)
        session.markModified()
        (1 to 60).foreach { position =>
          session.placeCursor(0, position * 5)
          session(InsertChar('W'), s"type W at $position")
          session(InsertChar('W'), s"type W again at $position")
          session(DeleteBackward, s"delete at $position")
        }
        session.mismatches shouldBe empty
      }

  for scale <- scales do
    s"Splitting a paragraph with two Enters then deleting both at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(prose, scale)
        session.markModified()
        List(0, 1, 2).foreach { line =>
          List(30, 120, 210, 300).foreach { column =>
            session.placeCursor(line, column)
            session(Enter, s"first Enter at $line:$column")
            session(Enter, s"second Enter at $line:$column")
            session(DeleteBackward, s"first delete at $line:$column")
            session(DeleteBackward, s"second delete at $line:$column")
          }
        }
        session.mismatches shouldBe empty
      }

  /** Typing leans towards a delete straight after, so runs often go A, B, A. */
  private def randomEdit(random: Random): (Event, String) =
    if random.nextInt(12) == 0 then Enter -> "Enter"
    else if random.nextInt(5) < 2 then
      val char = "WWiM m".charAt(random.nextInt(6))
      InsertChar(char) -> s"type '$char'"
    else DeleteBackward -> "delete"

  for
    scale <- scales
    seed  <- 1 to 6
  do
    s"A random run of edits at seed $seed and ${scale}x" should "draw what a cold frame draws at every step" in {
      val random  = Random(seed)
      val session = Session(prose, scale)
      session.markModified()
      (1 to 40).foreach { round =>
        session.placeCursor(0, 20 + random.nextInt(260))
        (1 to 8).foreach { _ =>
          val (event, label) = randomEdit(random)
          session(event, s"round $round: $label")
        }
      }
      session.mismatches shouldBe empty
    }
