package com.serenity.input

import java.awt.{Point, Rectangle}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the platform is told about the editor while an input method composes (#2008): where to put its candidate
  * window, and that there is no committed text it may read back or take away.
  */
class ImeRequestsSpec extends AnyFlatSpec with Matchers:

  private def requests(caret: Option[Rectangle], origin: Point = new Point(100, 200)) =
    ImeRequests.create(() => caret, () => origin, _ => ())

  "ImeRequests.getTextLocation" should "be the caret, moved from canvas to screen coordinates" in {
    requests(Some(new Rectangle(10, 20, 2, 16))).getTextLocation(null) shouldBe new Rectangle(110, 220, 2, 16)
  }

  it should "be a thin rectangle at the canvas origin before any caret has been drawn" in {
    val location = requests(None).getTextLocation(null)

    (location.x, location.y) shouldBe (100, 200)
    location.width should be > 0
    location.height should be > 0
  }

  "ImeRequests" should "hold no committed text and have nothing to cancel" in {
    val asked = requests(None)

    asked.getCommittedTextLength shouldBe 0
    asked.getCommittedText(0, 0, null).getEndIndex shouldBe 0
    asked.getSelectedText(null).getEndIndex shouldBe 0
    asked.cancelLatestCommittedText(null) shouldBe null
    asked.getInsertPositionOffset shouldBe 0
    asked.getLocationOffset(5, 5) shouldBe null
  }

  it should "tell an observer what the platform asked" in {
    val seen  = scala.collection.mutable.ListBuffer.empty[String]
    val asked = ImeRequests.create(() => None, () => new Point(0, 0), seen += _)
    val _     = asked.getTextLocation(null)

    seen.toList.exists(_.contains("getTextLocation")) shouldBe true
  }
