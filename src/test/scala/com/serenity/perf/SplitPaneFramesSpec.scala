package com.serenity.perf

import com.serenity.config.{AppConfig, Gutter, TextAreaInsets}
import com.serenity.keystroke.events.{DeleteBackward, Enter, InsertChar, PageDown, ScrollDown, ScrollUp}
import com.serenity.richtext.RichTextDocument
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Each split pane carries its own gutters and line-number rails (#2570), so an edit in one pane, a scroll of another,
  * a change of the active pane and the opening of a split must leave the window holding what a cold frame draws, at
  * both device scales. Frames are compared with cold ones as [[ColdFrameHarness]] describes.
  */
class SplitPaneFramesSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  private def lines(prefix: String, count: Int): String =
    (1 to count).map(n => s"$prefix line $n " + "lorem ipsum dolor sit amet " * (1 + n % 3)).mkString("\n")

  private val first  = RichTextDocument.fromPlainText(lines("first", 140))
  private val second = lines("second", 12000)

  private val railed: AppConfig =
    baseConfig.withLineNumbers(true).withTextAreaInsets(TextAreaInsets.percent(left = 0.10, right = 0.05))

  private val cellsAndAuto: AppConfig =
    baseConfig
      .withLineNumbers(true)
      .withTextAreaInsets(TextAreaInsets(Gutter.Cells(3), Gutter.Auto, Gutter.Percent(0.0), Gutter.Percent(0.0)))

  private val configs = List("percent gutters" -> railed, "cell and auto gutters" -> cellsAndAuto)

  for
    scale          <- List(1.0, 2.0)
    (name, config) <- configs
    stacked        <- List(false, true)
  do
    val arrangement = if stacked then "stacked" else "side by side"

    s"Typing in one pane and scrolling another, $arrangement with $name at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(first, scale, config)
        session.markModified()
        session.split(second, stacked, "open a split showing a longer document")
        session.focusPane(0, "focus the first pane")
        (1 to 12).foreach(n => session(InsertChar('w'), s"type $n in the first pane"))
        session.focusPane(1, "focus the second pane")
        session(ScrollDown(7), "scroll the second pane")
        session(PageDown, "page the second pane")
        session(ScrollUp(3), "scroll the second pane back")
        (1 to 6).foreach(n => session(InsertChar('w'), s"type $n in the second pane"))
        session.focusPane(0, "focus the first pane again")
        session(Enter, "split a line in the first pane")
        session(DeleteBackward, "join it again")
        session.mismatches shouldBe empty
      }

    s"Toggling line numbers with two panes open, $arrangement with $name at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(first, scale, config)
        session.markModified()
        session.split(second, stacked, "open a split showing a longer document")
        session.change("hide line numbers")(state =>
          state.copy(persisted = state.persisted.copy(config = state.persisted.config.withLineNumbers(false)))
        )
        session.change("show line numbers")(state =>
          state.copy(persisted = state.persisted.copy(config = state.persisted.config.withLineNumbers(true)))
        )
        session.focusPane(0, "focus the first pane")
        session(ScrollDown(5), "scroll the first pane")
        session.mismatches shouldBe empty
      }
