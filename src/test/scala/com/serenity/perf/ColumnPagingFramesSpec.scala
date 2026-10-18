package com.serenity.perf

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{ColumnLeft, ColumnRight, DeleteBackward, Enter, InsertChar}
import com.serenity.state.manager.ColumnPagingFixture
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Columns are filled by pixel height, so an edit that makes a row taller or shorter -- a heading that wraps to a
  * second row, a paragraph above one that gains a row -- moves rows between columns and pages, not just down the
  * column. Every frame drawn incrementally must still equal a cold frame, at both device scales. Frames are compared
  * with cold ones as [[ColdFrameHarness]] describes.
  */
class ColumnPagingFramesSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  private val columnConfig: AppConfig =
    baseConfig
      .withColumnMode(true)
      .withColumnCount(Some(2))
      .withWordWrap(true)
      .withHeadingsConfig(ColumnPagingFixture.spacing)

  private val headingLine = 0
  private val bodyLine    = 3

  for scale <- List(1.0, 2.0) do
    s"Typing a heading onto a second row in a two-column pane at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(ColumnPagingFixture.document, scale, columnConfig)
        session.markModified()
        session.placeCursor(headingLine, 9)
        "abcdefghijklmnopqrstuvwx".foreach(char => session(InsertChar(char), s"type '$char' in the heading"))
        (1 to 24).foreach(n => session(DeleteBackward, s"delete $n from the heading"))
        session.mismatches shouldBe empty
      }

    s"Typing in a paragraph that pushes a heading into the next column at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(ColumnPagingFixture.document, scale, columnConfig)
        session.markModified()
        session.placeCursor(bodyLine, 30)
        (1 to 40).foreach(n => session(InsertChar('w'), s"type $n in the body"))
        (1 to 40).foreach(n => session(DeleteBackward, s"delete $n from the body"))
        session.mismatches shouldBe empty
      }

    s"Splitting a paragraph twice in a two-column pane at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(ColumnPagingFixture.document, scale, columnConfig)
        session.markModified()
        session.placeCursor(bodyLine, 40)
        session(Enter, "split the paragraph")
        session(Enter, "split it again")
        session.mismatches shouldBe empty
      }

    s"Moving by column through a two-column pane at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(ColumnPagingFixture.document, scale, columnConfig)
        (1 to 8).foreach(n => session(ColumnRight, s"column right $n"))
        (1 to 8).foreach(n => session(ColumnLeft, s"column left $n"))
        session.mismatches shouldBe empty
      }
