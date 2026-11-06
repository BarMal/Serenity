package com.serenity.perf

import com.serenity.keystroke.events.InsertChar
import com.serenity.richtext.{RichTextDocument, RichTextParagraph, RichTextRun}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A frame that differs from the cold one is easier to debug when the failure says which rows the planner drew. */
class ColdFrameHarnessFailureSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  private val document = RichTextDocument(
    List("alpha", "beta").map(seed =>
      RichTextParagraph(List(RichTextRun((1 to 40).map(n => s"${seed}word$n").mkString(" "))))
    )
  )

  "A step's plan" should "name the rows an edit made the planner draw" in {
    val session = Session(document, 1.0)
    session(InsertChar('Q'), "edit")
    session.planOfLastFrame.toList.flatMap(_.dirtyRowsByPane.values.flatten) should not be empty
  }

  "The failure for a deliberately mismatching frame" should "report the dirty and preserved rows of that step" in {
    val session = Session(document, 1.0)
    session(InsertChar('Q'), "edit")
    session.mismatches shouldBe empty
    val shown = session.screen
    shown.setRGB(5, 2, shown.getRGB(5, 2) ^ 0x000001)
    val diff = differing(shown, session.currentState, 1.0)
    diff shouldBe Seq((5, 2))
    val failure = failureLine(1, "edit", diff, session.planOfLastFrame)
    failure should include("step 1 (edit)")
    failure should include("1 px differ")
    failure should include("dirty rows")
    failure should include("pane 0:")
    failure should include("preserved y")
  }
