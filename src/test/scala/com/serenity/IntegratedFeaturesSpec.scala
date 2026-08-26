package com.serenity

import com.serenity.keystroke.events.{InsertChar, TabKey}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.rope.Balance
import com.serenity.state.components.ComponentResult
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntegratedFeaturesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Integrated Text Editor Features" should "handle tab insertion, underscore rendering, and theme support together" in {
    val translator = new TextEntryTranslator()
    val tabEvent   = translator.translate(KeyStrokeInfo(InputKey.Tab, None, Set.empty))
    tabEvent shouldBe TabKey

    val underscoreEvent = translator.translate(KeyStrokeInfo(InputKey.Character, Some('_'), Set.empty))
    underscoreEvent shouldBe InsertChar('_')

    val bufferId = BufferId(1)
    val cursor   = CursorPosition(0, 0)
    val buffer =
      Buffer
        .fromString(bufferId, "function test_func() {\n\treturn 'hello_world';\n}")
        .copy(editing = EditingState(cursors = List(cursor)))
    val paneId = PaneId(1)
    val pane   = EditorPane(paneId, Some(bufferId), Viewport.default, List.empty, 0)
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      layout = Layout.empty.copy(editorPanes = Map(paneId -> pane)),
      theme = Theme.dark
    )

    state.theme.name shouldBe "dark"
    state.theme.syntaxColors should contain key com.serenity.ui.theme.SyntaxElement.Keyword
    state.theme.syntaxColors should contain key com.serenity.ui.theme.SyntaxElement.String

    val surface = new MockRenderSurface(80, 24)
    noException should be thrownBy
      Renderer.render(state, cursorVisible = true, surface, ViewportSize(80, 24))

    buffer.document.content.collect() should include("test_func")
    buffer.document.content.collect() should include("\t")
    buffer.document.content.collect() should include("hello_world")
  }

  it should "allow switching between light and dark themes" in {
    val state = AppState.empty.copy(theme = Theme.light)

    state.theme.name shouldBe "light"
    state.theme.foregroundColor should not be Theme.dark.foregroundColor
    state.theme.backgroundColor should not be Theme.dark.backgroundColor

    val darkState = state.copy(theme = Theme.dark)
    darkState.theme.name shouldBe "dark"
  }

  it should "preserve tab and underscore characters through edit operations" in {
    import com.serenity.state.components.EditorPaneComponent

    val bufferId = BufferId(1)
    val cursor   = CursorPosition(0, 5)
    val buffer   = Buffer.fromString(bufferId, "hello").copy(editing = EditingState(cursors = List(cursor)))
    val paneId   = PaneId(1)
    val pane     = EditorPane(paneId, Some(bufferId), Viewport.default, List.empty, 0)
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      layout = Layout.empty.copy(editorPanes = Map(paneId -> pane)),
      theme = Theme.dark
    )

    val component = new EditorPaneComponent(paneId)

    val tabResult = component.processEvent(InsertChar('\t'), state)
    tabResult should not be ComponentResult.noChange

    val stateAfterTab = tabResult match
      case ComponentResult.ReducerUpdate(reducerResult) => reducerResult.state
      case _                                            => fail("Expected state change")

    val underscoreResult = component.processEvent(InsertChar('_'), stateAfterTab)
    val finalState = underscoreResult match
      case ComponentResult.ReducerUpdate(reducerResult) => reducerResult.state
      case _                                            => fail("Expected state change")

    val finalBuffer = finalState.buffers(bufferId)
    finalBuffer.document.content.collect() shouldBe "hello\t_"
  }
