package com.serenity.state.manager

import com.serenity.TestWorkspaceTrees
import com.serenity.keystroke.events.*
import com.serenity.state.components.ComponentResult
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, PanelContent}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1940: an unhandled key bubbles from a focused surface to the editor pane, unless the surface is modal. */
class FocusScopesSpec extends AnyFlatSpec with Matchers:

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(0)

  private val outline = UiSurface.fromPanelContent(SurfaceId("outline"), PanelContent.Outline(Nil))

  private val prompt = UiSurface(
    SurfaceId("prompt"),
    SurfaceContent.ModalWorkflow(Modal.TextPrompt(TextPrompt.gotoLine("7"))),
    SurfacePresentation.Floating(None, SurfacePlacement.AboveCursor)
  )

  private val peek = UiSurface(
    SurfaceId("peek"),
    SurfaceContent.QuickInfo("hint"),
    SurfacePresentation.Floating(None, SurfacePlacement.AboveCursor),
    dismissOnMove = true
  )

  private def stateWith(surfaces: List[UiSurface], focus: Focus): AppState =
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> Buffer.fromString(bufferId, "hello")),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = focus
      ),
      runtime = AppState.initial.runtime.copy(uiSurfaces = surfaces)
    )

  "SurfaceFocusPolicy" should "make floating LSP peeks peeks, dialogs modal and docked panels focusable" in {
    peek.focusPolicy shouldBe SurfaceFocusPolicy.Peek
    prompt.focusPolicy shouldBe SurfaceFocusPolicy.Modal
    outline.focusPolicy shouldBe SurfaceFocusPolicy.Focusable
  }

  "FocusScopes.bubbleTarget" should "send a focusable surface's unhandled key on to the active editor pane" in {
    val state = stateWith(List(outline), Focus.Surface(outline.id))

    FocusScopes.bubbleTarget(Focus.Surface(outline.id), ComponentResult.unhandled, state) shouldBe Some(paneId)
    FocusScopes.bubbleTarget(
      Focus.Surface(outline.id),
      ComponentResult.composite(ComponentResult.dismiss, ComponentResult.unhandled),
      state
    ) shouldBe Some(paneId)
  }

  it should "keep a modal surface's unhandled key where it is" in {
    val state = stateWith(List(prompt), Focus.Surface(prompt.id))

    FocusScopes.bubbleTarget(Focus.Surface(prompt.id), ComponentResult.unhandled, state) shouldBe None
  }

  it should "not bubble a key the surface handled" in {
    val state = stateWith(List(outline), Focus.Surface(outline.id))

    FocusScopes.bubbleTarget(Focus.Surface(outline.id), ComponentResult.noChange, state) shouldBe None
    FocusScopes.bubbleTarget(Focus.Surface(outline.id), ComponentResult.dismiss, state) shouldBe None
  }

  "FocusScopes.asEditorEvent" should "restate a surface's own spelling of a key as the editor's" in {
    FocusScopes.asEditorEvent(ModalInsertChar('x')) shouldBe Some(InsertChar('x'))
    FocusScopes.asEditorEvent(ModalNavigate(Direction.Down)) shouldBe Some(MoveDown)
    FocusScopes.asEditorEvent(PeekInputEvent.Navigate(Direction.Left)) shouldBe Some(MoveLeft)
    FocusScopes.asEditorEvent(MoveRight) shouldBe Some(MoveRight)
  }

  it should "drop a key the surface's spelling has already lost" in {
    FocusScopes.asEditorEvent(PeekInputEvent.OtherInput) shouldBe None
  }

  "FocusScopes.peekKeyOutcome" should "close an unfocused peek on a passing key and still pass the key on" in {
    val state = stateWith(List(peek), Focus.EditorPane(paneId))

    FocusScopes.peekKeyOutcome(InsertChar('x'), state) match
      case FocusScopes.PeekKeyOutcome.PassedOn(after) => after.runtime.uiSurfaces shouldBe empty
      case other                                      => fail(s"Expected the key to pass on, got $other")
  }

  it should "consume Escape to close an unfocused peek" in {
    val state = stateWith(List(peek), Focus.EditorPane(paneId))

    FocusScopes.peekKeyOutcome(Escape, state) match
      case FocusScopes.PeekKeyOutcome.Consumed(after) => after.runtime.uiSurfaces shouldBe empty
      case other                                      => fail(s"Expected Escape to be consumed, got $other")
  }

  it should "leave every key to a modal surface" in {
    val state = stateWith(List(peek, prompt), Focus.Surface(prompt.id))

    FocusScopes.peekKeyOutcome(Escape, state) shouldBe FocusScopes.PeekKeyOutcome.PassedOn(state)
  }

end FocusScopesSpec
