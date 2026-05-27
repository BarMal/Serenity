package com.serenity

import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.serenity.input.FocusedInputTranslator
import com.serenity.keystroke.events.{Direction, ModalNextField, ModalSubmit, NewLine, PanelInputEvent, PeekInputEvent, RunnerSubmit, ToggleCommandRunner}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{DirectoryTreeData, PanelContent, PanelPosition}
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FocusedInputTranslatorSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(0)

  private val editorState = AppState.initial.copy(
    layout = Layout(
      editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
      activeEditorPaneId = Some(paneId)
    ),
    focus = Focus.EditorPane(paneId)
  )

  "FocusedInputTranslator" should "treat Enter as newline in editor focus" in {
    val translator = FocusedInputTranslator.forState(editorState)

    translator.translate(new KeyStroke(KeyType.Enter)) shouldBe NewLine
  }

  it should "treat Enter as submit in command-runner focus while preserving global hotkeys" in {
    val commandRunnerState = editorState.copy(
      focus = Focus.Surface(SurfaceId("command-runner")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(com.serenity.command.CommandRunner.empty),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      )
    )
    val translator = FocusedInputTranslator.forState(commandRunnerState)

    translator.translate(new KeyStroke(KeyType.Enter)) shouldBe RunnerSubmit
    translator.translate(new KeyStroke('p', true, false, false)) shouldBe ToggleCommandRunner
  }

  it should "treat Enter and Tab as modal form actions in modal focus" in {
    val modalState = editorState.copy(
      focus = Focus.Surface(SurfaceId("file-modal")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("file-modal"),
          SurfaceContent.ModalWorkflow(
            Modal.FileWorkflow(FileWorkflowState(mode = FileWorkflowMode.Open))
          ),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      )
    )
    val translator = FocusedInputTranslator.forState(modalState)

    translator.translate(new KeyStroke(KeyType.Enter)) shouldBe ModalSubmit
    translator.translate(new KeyStroke(KeyType.Tab)) shouldBe ModalNextField
  }

  it should "treat pinned panel focus as panel-local navigation and focus-return input" in {
    val pinnedState = editorState.copy(
      focus = Focus.Surface(SurfaceId("left-panel")),
      uiSurfaces = List(
        UiSurface.fromPanelContent(
          SurfaceId("left-panel"),
          PanelContent.DirectoryTree(DirectoryTreeData(java.nio.file.Paths.get("/repo")), None),
          PanelPosition.Left,
          24
        )
      )
    )
    val translator = FocusedInputTranslator.forState(pinnedState)

    translator.translate(new KeyStroke(KeyType.ArrowUp)) shouldBe PanelInputEvent.Navigate(Direction.Up)
    translator.translate(new KeyStroke('x', false, false, false)) shouldBe PanelInputEvent.ReturnFocus
  }

  it should "treat floating peek focus as dismiss-oriented local input" in {
    val peekState = editorState.copy(
      focus = Focus.Surface(SurfaceId("peek")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("peek"),
          SurfaceContent.QuickInfo("map"),
          SurfacePresentation.Floating(None, SurfacePlacement.AboveCursor)
        )
      )
    )
    val translator = FocusedInputTranslator.forState(peekState)

    translator.translate(new KeyStroke(KeyType.ArrowDown)) shouldBe PeekInputEvent.Navigate(Direction.Down)
    translator.translate(new KeyStroke(KeyType.Enter)) shouldBe PeekInputEvent.Accept
  }
