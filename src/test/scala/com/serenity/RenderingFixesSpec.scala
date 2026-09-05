package com.serenity

import com.serenity.animation.AnimationState
import com.serenity.keystroke.events.ToggleSyntaxHighlighting
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, EditorPaneComponent}
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout
import com.serenity.ui.renderer.CharacterRenderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RenderingFixesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Syntax highlighting toggle" should "work correctly" in {
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, "test content")
    val paneId   = PaneId(1)
    val cursor   = CursorPosition(0, 0)
    val pane     = EditorPane(paneId, Some(bufferId), Viewport.default, List(cursor), 0)
    val state = AppState.empty.copy(
      persisted = AppState.empty.persisted.copy(
        buffers = Map(bufferId -> buffer),
        layout = Layout.empty.copy(editorPanes = Map(paneId -> pane)),
        config = com.serenity.config.AppConfig.default.withSyntaxHighlighting(false)
      )
    )

    val component = new EditorPaneComponent(paneId)

    // Initially syntax highlighting should be off
    state.syntaxHighlightingEnabled shouldBe false

    // Toggle it on
    val result = component.processEvent(ToggleSyntaxHighlighting, state)
    result should not be ComponentResult.noChange

    result match
      case ComponentResult.StateChange(update) =>
        val newState = update(state)
        newState.syntaxHighlightingEnabled shouldBe true

        // Toggle it off again
        val result2 = component.processEvent(ToggleSyntaxHighlighting, newState)
        result2 match
          case ComponentResult.StateChange(update2) =>
            val finalState = update2(newState)
            finalState.syntaxHighlightingEnabled shouldBe false
          case _ => fail("Expected StateChange result")
      case _ => fail("Expected StateChange result")
  }

  "Tab character rendering" should "expand to proper width" in {
    val surface = new MockRenderSurface(80, 24)
    CharacterRenderer.renderStringPlain(surface, 0, 0, "a\tb")

    surface.putStringCalls.map(_.s) shouldBe List("a", "   ", "b")
  }

  "Underscore character" should "render visibly" in {
    val surface = new MockRenderSurface(80, 24)
    CharacterRenderer.renderStringPlain(surface, 0, 0, "test_underscore")

    surface.putStringCalls.map(_.s) shouldBe List("test_underscore")
  }

  "Default syntax highlighting" should "be off" in {
    val state = AppState.empty
    state.syntaxHighlightingEnabled shouldBe false
  }

  "Character rendering" should "handle special cases" in {
    val surface = new MockRenderSurface(80, 24)
    CharacterRenderer.renderStringPlain(surface, 0, 0, "a_b\tc")

    surface.putStringCalls.map(_.s) shouldBe List("a_b", " ", "c")
  }

  it should "batch contiguous printable runs in plain rendering" in {
    val surface = new MockRenderSurface(80, 24)

    CharacterRenderer.renderStringPlain(surface, 0, 0, "abc")

    surface.putStringCalls shouldBe List(surface.PutStringCall(0, 0, "abc"))
  }

  it should "batch contiguous printable runs in animated plain rendering when cell colors match" in {
    val surface = new MockRenderSurface(80, 24)

    CharacterRenderer.renderStringWithAnimationPlain(
      surface,
      0,
      0,
      "abc",
      Theme.default,
      AnimationState.empty
    )

    surface.putStringCalls shouldBe List(surface.PutStringCall(0, 0, "abc"))
  }

  /** A run's screen column and its buffer column advance at different rates once a wide glyph is in it: the glyph takes
    * two cells but one buffer column (`CharWidth`/#1269). Grouping walked the run with a single `Char` index and used
    * it for both, so every colour group after a wide glyph started one cell early and read the wrong animation cell
    * (#1271).
    */
  it should "keep screen and buffer columns apart when a wide glyph precedes an animated cell" in {
    val surface = new MockRenderSurface(80, 24)
    // Buffer columns: 0 is the wide glyph, 1 is 'a', 2 is 'b'. Only 'b' is animated, so it must be its own group,
    // painted at screen column 3 -- the wide glyph's two cells plus 'a'.
    val animations = AnimationState.empty.addCompletedCharacter('b', 2, 0, java.awt.Color.RED)

    CharacterRenderer.renderStringWithAnimationPlain(
      surface,
      0,
      0,
      "漢ab",
      Theme.default,
      animations
    )

    surface.putStringCalls shouldBe List(
      surface.PutStringCall(0, 0, "漢a"),
      surface.PutStringCall(3, 0, "b")
    )
  }

  it should "keep a surrogate pair whole when only part of the run is animated" in {
    val surface = new MockRenderSurface(80, 24)
    // "\uD83D\uDE00" is one codepoint over two chars. The animation names column 1 -- the pair's trailing half, which
    // is not a character position at all. Walking by `Char` found it there and split the glyph across two colour
    // groups, painting two broken halves; walking by codepoint looks the pair up once, at the column it starts on.
    val animations = AnimationState.empty.addCompletedCharacter('x', 1, 0, java.awt.Color.RED)

    CharacterRenderer.renderStringWithAnimationPlain(
      surface,
      0,
      0,
      "\uD83D\uDE00x",
      Theme.default,
      animations
    )

    // One call, one whole glyph: the pair is looked up once at the column it starts on, so an animation keyed to its
    // trailing half addresses no character and changes nothing. Before, this painted "?" at column 0, "?" at column 1
    // and "x" at 2 -- the glyph torn in half.
    surface.putStringCalls shouldBe List(surface.PutStringCall(0, 0, "\uD83D\uDE00x"))
  }

  it should "batch contiguous runs in syntax-highlighted rendering when the styled segment is uniform" in {
    val surface = new MockRenderSurface(80, 24)

    CharacterRenderer.renderStringWithAnimation(
      surface,
      0,
      0,
      "abc",
      Theme.default,
      AnimationState.empty,
      syntaxHighlightingEnabled = true
    )

    surface.putStringCalls shouldBe List(surface.PutStringCall(0, 0, "abc"))
  }

  it should "apply style hooks for syntax-highlighted keyword runs" in {
    val surface = new MockRenderSurface(80, 24)

    CharacterRenderer.renderStringWithAnimation(
      surface,
      0,
      0,
      "if",
      Theme.default,
      AnimationState.empty,
      syntaxHighlightingEnabled = true,
      language = Some(LanguageId.Scala)
    )

    surface.styleCalls should contain(surface.StyleCall("enable", com.serenity.ui.theme.TextStyle.bold))
    surface.styleCalls should contain(surface.StyleCall("disable", com.serenity.ui.theme.TextStyle.bold))
  }

  it should "apply underline style hooks for markdown link runs" in {
    val surface = new MockRenderSurface(80, 24)

    CharacterRenderer.renderStringWithAnimation(
      surface,
      0,
      0,
      "See [guide](docs.md)",
      Theme.default,
      AnimationState.empty,
      syntaxHighlightingEnabled = true,
      language = Some(LanguageId.Markdown)
    )

    surface.styleCalls should contain(surface.StyleCall("enable", com.serenity.ui.theme.TextStyle.underlined))
    surface.styleCalls should contain(surface.StyleCall("disable", com.serenity.ui.theme.TextStyle.underlined))
  }
