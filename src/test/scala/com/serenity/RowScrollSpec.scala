package com.serenity

import com.serenity.keystroke.events.{ScrollDown, ScrollUp, TextEntryEvent}
import com.serenity.state.components.{ComponentResult, EditorPaneComponent}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2479: a notch of the wheel or trackpad moves on-screen rows, not logical lines. A document that is one or a few
  * very long lines, such as an RTF source, wraps into many rows and used to have nowhere to scroll: the wheel stepped
  * whole lines and put the row within the line back to 0.
  */
class RowScrollSpec extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  private val oneLongLine  = Vector("word " * 4000)
  private val shortLines   = Vector.tabulate(120)(line => s"line $line")
  private val wrappingPair = Vector("word " * 1500, "word " * 1500)

  private def withFixtureFonts(state: AppState): AppState =
    val fonts = state.persisted.config.editorConfig.fontConfig.copy(
      codeFontFamily = codeFont.getFamily,
      fontSize = codeFont.getSize2D,
      textFontFamily = textFont.getFamily,
      textFontSize = textFont.getSize2D
    )
    state.copy(persisted = state.persisted.copy(config = state.persisted.config.withFontConfig(fonts)))

  private def prose(lines: Vector[String]): AppState =
    val state = withFixtureFonts(stateOf(lines.mkString("\n"), CursorPosition(0, 0)))
    val content =
      Session().settle(state).paneLayouts.getOrElse(paneId, fail("expected a pane layout")).contentRect
    val buffer = state.persisted.buffers(bufferId)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(
          bufferId,
          buffer.copy(viewport = buffer.viewport.copy(visibleLines = content.height, visibleColumns = content.width))
        )
      )
    )

  private def scrolled(state: AppState, event: TextEntryEvent): AppState =
    new EditorPaneComponent(paneId).processEvent(event, state) match
      case ComponentResult.ReducerUpdate(result) => result.state
      case other                                 => fail(s"expected a reduction, got $other")

  private def viewportOf(state: AppState): Viewport = state.persisted.buffers(bufferId).viewport

  private def top(state: AppState): (Int, Int) =
    val viewport = viewportOf(state)
    (viewport.topLine, viewport.topVisualLine)

  "A wheel notch in a document of one long line" should "move that many rows down within the line" in {
    top(scrolled(prose(oneLongLine), ScrollDown(3))) shouldBe ((0, 3))
  }

  it should "move that many rows back up" in {
    val down = scrolled(prose(oneLongLine), ScrollDown(5))

    top(scrolled(down, ScrollUp(2))) shouldBe ((0, 3))
  }

  it should "stop at the first row when scrolled above it" in {
    val down = scrolled(prose(oneLongLine), ScrollDown(5))

    top(scrolled(down, ScrollUp(100))) shouldBe ((0, 0))
  }

  it should "come to rest at the end of the line, and stay there" in {
    val atEnd = scrolled(prose(oneLongLine), ScrollDown(1_000_000))

    top(atEnd)._2 should be > 5
    top(scrolled(atEnd, ScrollDown(1_000_000))) shouldBe top(atEnd)
  }

  "A wheel notch in a document of wrapping paragraphs" should "step through the rows of a paragraph before the next" in {
    val state = prose(wrappingPair)

    top(scrolled(state, ScrollDown(1))) shouldBe ((0, 1))
  }

  it should "carry on into the next paragraph at the end of the first, and come back" in {
    val state = prose(wrappingPair)
    val atEnd = scrolled(state, ScrollDown(1_000_000))
    val toTop = scrolled(atEnd, ScrollUp(1_000_000))

    top(atEnd)._1 shouldBe 1
    top(toTop) shouldBe ((0, 0))
  }

  "A wheel notch in a document of short lines" should "still move one line a row" in {
    top(scrolled(prose(shortLines), ScrollDown(4))) shouldBe ((4, 0))
  }
