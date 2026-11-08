package com.serenity.perf

import com.serenity.keystroke.events.InsertChar
import com.serenity.richtext.{RichTextDocument, RichTextParagraph, RichTextRun}
import com.serenity.testkit.FrameDiff
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A step of an incremental frame spec compares the frame with a cold one. Both are read as pixel arrays, so a step
  * that matches builds no image; an image is built when a spec asks to see the screen or a cold frame.
  */
class ColdFrameHarnessSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  private val document = RichTextDocument(
    List("alpha", "beta").map(seed =>
      RichTextParagraph(List(RichTextRun((1 to 40).map(n => s"${seed}word$n").mkString(" "))))
    )
  )

  "Steps whose frames match the cold frame" should "build no image" in {
    val session = Session(document, 1.0)
    session(InsertChar('Q'), "edit")
    session(InsertChar('R'), "edit again")
    session.mismatches shouldBe empty
    session.imagesBuilt shouldBe 0
  }

  "The screen after a step" should "be one image showing what a cold frame draws" in {
    val session = Session(document, 2.0)
    session(InsertChar('Q'), "edit")
    val shown = session.screen
    session.imagesBuilt shouldBe 1
    FrameDiff.differing(shown, session.coldFrame()) shouldBe empty
  }

  "A drawn frame with one pixel changed" should "differ from the cold frame at that pixel alone" in {
    val session = Session(document, 1.0)
    session(InsertChar('Q'), "edit")
    val state = session.currentState
    val exact = coldFramePixelsWith(state, 1.0)(_.clone())
    differingPixels(exact, state, 1.0) shouldBe empty
    val changed = exact.clone()
    changed(2 * windowWidth + 5) = changed(2 * windowWidth + 5) ^ 0x000001
    differingPixels(changed, state, 1.0) shouldBe Seq((5, 2))
  }
