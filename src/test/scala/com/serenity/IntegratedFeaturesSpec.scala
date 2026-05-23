package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.googlecode.lanterna.input.KeyType
import com.googlecode.lanterna.screen.{Screen, TerminalScreen}
import com.googlecode.lanterna.terminal.virtual.DefaultVirtualTerminal
import com.serenity.keystroke.events.{InsertChar, TextEntryEvent}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.rope.Balance
import com.serenity.state.components.ComponentResult
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntegratedFeaturesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Integrated Text Editor Features" should "handle tab insertion, underscore rendering, and theme support together" in {
    // Test tab insertion through translator
    val translator   = new TextEntryTranslator()
    val tabKeyStroke = com.googlecode.lanterna.input.KeyStroke(KeyType.Tab, false, false, false)
    val tabEvent     = translator.translate(tabKeyStroke)
    tabEvent shouldBe InsertChar('\t')

    // Test underscore character handling
    val underscoreKeyStroke = com.googlecode.lanterna.input.KeyStroke('_', false, false, false)
    val underscoreEvent     = translator.translate(underscoreKeyStroke)
    underscoreEvent shouldBe InsertChar('_')

    // Create state with theme
    val bufferId = BufferId(1)
    val cursor   = CursorPosition(0, 0)
    val buffer   = Buffer.fromString(bufferId, "function test_func() {\n\treturn 'hello_world';\n}").copy(cursors = List(cursor))
    val paneId   = PaneId(1)
    val pane     = EditorPane(paneId, Some(bufferId), Viewport.default, List.empty, 0)
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      layout = Layout.empty.copy(editorPanes = Map(paneId -> pane)),
      theme = Theme.dark
    )

    // Verify theme is applied
    state.theme.name shouldBe "dark"
    state.theme.syntaxColors should contain key com.serenity.ui.theme.SyntaxElement.Keyword
    state.theme.syntaxColors should contain key com.serenity.ui.theme.SyntaxElement.String

    // Test rendering with all features (virtual screen test)
    val virtualTerminal = new DefaultVirtualTerminal(com.googlecode.lanterna.TerminalSize.ONE)
    virtualTerminal.setTerminalSize(com.googlecode.lanterna.TerminalSize(80, 24))
    val screen = new TerminalScreen(virtualTerminal)

    // Should render without exception
    noException should be thrownBy
      Renderer.render(state, cursorVisible = true, screen)

    // Verify content includes both tabs and underscores
    buffer.content.collect() should include("test_func")
    buffer.content.collect() should include("\t")
    buffer.content.collect() should include("hello_world")
  }

  it should "allow switching between light and dark themes" in {
    val state = AppState.empty.copy(theme = Theme.light)

    state.theme.name shouldBe "light"
    state.theme.foregroundColor should not be Theme.dark.foregroundColor
    state.theme.backgroundColor should not be Theme.dark.backgroundColor

    // Switch to dark theme
    val darkState = state.copy(theme = Theme.dark)
    darkState.theme.name shouldBe "dark"
  }

  it should "preserve tab and underscore characters through edit operations" in {
    import com.serenity.state.components.EditorPaneComponent

    val bufferId = BufferId(1)
    val cursor   = CursorPosition(0, 5) // At end of "hello"
    val buffer   = Buffer.fromString(bufferId, "hello").copy(cursors = List(cursor))
    val paneId   = PaneId(1)
    val pane     = EditorPane(paneId, Some(bufferId), Viewport.default, List.empty, 0)
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      layout = Layout.empty.copy(editorPanes = Map(paneId -> pane)),
      theme = Theme.dark
    )

    val component = new EditorPaneComponent(paneId)

    // Insert tab
    val tabResult = component.processEvent(InsertChar('\t'), state)
    tabResult should not be ComponentResult.noChange

    val stateAfterTab = tabResult match
      case ComponentResult.StateChange(update) => update(state)
      case _                                   => fail("Expected state change")

    // Insert underscore
    val underscoreResult = component.processEvent(InsertChar('_'), stateAfterTab)
    val finalState = underscoreResult match
      case ComponentResult.StateChange(update) => update(stateAfterTab)
      case _                                   => fail("Expected state change")

    val finalBuffer = finalState.buffers(bufferId)
    finalBuffer.content.collect() shouldBe "hello\t_"
  }
