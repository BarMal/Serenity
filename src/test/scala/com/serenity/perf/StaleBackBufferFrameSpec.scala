package com.serenity.perf

import com.serenity.keystroke.events.{DeleteBackward, Enter, InsertChar}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The window alternates between two images, so the screen shows the frame before the one a surface preserves pixels
  * from. An edit that goes to a state B and back to A must repaint the rows B changed, even though A differs from the
  * image's own previous frame only by the edit that took it back. Frames are compared with cold ones as
  * [[ColdFrameHarness]] describes.
  */
class StaleBackBufferFrameSpec extends AnyFlatSpec with Matchers with StaleBackBufferScenarios:

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
