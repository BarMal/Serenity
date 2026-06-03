package com.serenity

import com.serenity.animation.AnimationState
import com.serenity.keystroke.events.{InsertChar, ToggleSyntaxHighlighting}
import com.serenity.rope.Balance
import com.serenity.state.components.ComponentResult
import com.serenity.state.components.EditorPaneComponent
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
      buffers = Map(bufferId -> buffer),
      layout = Layout.empty.copy(editorPanes = Map(paneId -> pane)),
      config = com.serenity.config.AppConfig.default.withSyntaxHighlighting(false)
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

  it should "preserve a continuous run for proportional text even when a single buffer cell is animated" in {
    val surface = new MockRenderSurface(80, 24)
    val animated =
      AnimationState.empty.addCharacterAnimation('l', 2, 0, Theme.default.background, Theme.default.foreground, steps = 4)

    CharacterRenderer.renderStringWithAnimationPlain(
      surface,
      0,
      0,
      "hello",
      Theme.default,
      animated,
      preserveContinuousRuns = true
    )

    surface.putStringCalls shouldBe List(surface.PutStringCall(0, 0, "hello"))
  }
