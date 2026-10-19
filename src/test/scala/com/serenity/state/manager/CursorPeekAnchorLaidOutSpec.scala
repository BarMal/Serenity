package com.serenity.state.manager

import com.serenity.config.AppConfig
import com.serenity.state.models.*
import com.serenity.ui.layout.ScreenPosition
import com.serenity.ui.tui.PopupAnchorFixtures.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The cursor peek freezes the on-screen cell of the caret it was summoned from, so that cell has to be the one the
  * caret is painted on -- beneath heading space, and on a word-wrapped continuation row.
  */
class CursorPeekAnchorLaidOutSpec extends AnyFlatSpec with Matchers:

  private def resolvedAnchor(config: AppConfig, cursor: CursorPosition): (Option[ScreenPosition], ScreenPosition) =
    val state = stateOf(config, cursor)
    val peeking = state.copy(runtime =
      state.runtime.copy(pointerGesture = state.runtime.pointerGesture.copy(cursorPeekAnchor = Some(cursor)))
    )
    val caret = renderWithCaret(state).cursor
    (
      CursorPeekAnchorResolution.resolve(peeking).runtime.pointerGesture.cursorPeekResolvedAnchor,
      ScreenPosition(caret.col, caret.row)
    )

  "The cursor-peek anchor" should "be the caret's painted cell beneath headings that carry space" in {
    val (anchor, caret) = resolvedAnchor(spacedHeadings, CursorPosition(4, 5))

    anchor shouldBe Some(caret)
  }

  it should "be the caret's painted cell on a word-wrapped continuation row" in {
    val (anchor, caret) = resolvedAnchor(AppConfig.conventional.withLineNumbers(false), CursorPosition(4, 58))

    anchor shouldBe Some(caret)
  }
