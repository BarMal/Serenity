package com.serenity.ui.tui

import com.serenity.config.AppConfig
import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutEngine, LayoutRect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import PopupAnchorFixtures.*

/** A floating surface hangs off the cell the caret is painted on: below it, or above it, whatever blank rows a
  * heading's space put between the caret and the top of the pane, and whichever word-wrapped row the caret is on.
  */
class PopupAnchorTuiSpec extends AnyFlatSpec with Matchers:

  private val belowHeadings     = CursorPosition(4, 5)
  private val onContinuationRow = CursorPosition(4, 58)

  private def popupRect(
    placement: SurfacePlacement,
    config: AppConfig,
    cursor: CursorPosition
  ): (LayoutRect, Int, Int) =
    val state  = stateOf(config, cursor, List(floating(placement, cursor)))
    val screen = renderWithCaret(state)
    val layout = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val stack = placement match
      case SurfacePlacement.AboveCursor => layout.aboveCursorOverlayStack
      case _                            => layout.belowCursorOverlayStack
    val rect = stack.headOption.map(_._2).getOrElse(fail(s"no popup laid out:\n${screen.render}"))
    (rect, screen.cursor.row, screen.cursor.col)

  /** The rows a popup leaves between itself and the caret, whichever side it is on: measured below the caret in a
    * document with no heading space, where there is always room.
    */
  private def gapRows: Int =
    val (rect, caretRow, _) =
      popupRect(SurfacePlacement.BelowCursor, AppConfig.conventional.withLineNumbers(false), CursorPosition(0, 3))
    rect.y - caretRow - 1

  "A popup below the caret" should "start under the caret row when headings above it carry space" in {
    val (rect, caretRow, _) = popupRect(SurfacePlacement.BelowCursor, spacedHeadings, belowHeadings)

    rect.y shouldBe caretRow + 1 + gapRows
  }

  "A popup above the caret" should "end over the caret row when headings above it carry space" in {
    val (rect, caretRow, _) = popupRect(SurfacePlacement.AboveCursor, spacedHeadings, belowHeadings)

    rect.bottom + gapRows shouldBe caretRow
  }

  "A popup" should "follow the caret onto a word-wrapped continuation row" in {
    val (rect, caretRow, _) =
      popupRect(SurfacePlacement.BelowCursor, AppConfig.conventional.withLineNumbers(false), onContinuationRow)

    rect.y shouldBe caretRow + 1 + gapRows
  }

  it should "follow the caret onto a continuation row beneath spaced headings" in {
    val (rect, caretRow, _) = popupRect(SurfacePlacement.BelowCursor, spacedHeadings, onContinuationRow)

    rect.y shouldBe caretRow + 1 + gapRows
  }
