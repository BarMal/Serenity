package com.serenity

import com.serenity.command.CommandRunner
import com.serenity.config.*
import com.serenity.document.RenderedComment
import com.serenity.input.FocusedInputTranslator
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
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

    translator.translate(KeyStrokeInfo(InputKey.Enter, None, Set.empty)) shouldBe NewLine
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

    translator.translate(KeyStrokeInfo(InputKey.Enter, None, Set.empty)) shouldBe RunnerSubmit
    translator.translate(KeyStrokeInfo(InputKey.Character, Some('p'), Set(Modifier.Ctrl))) shouldBe ToggleCommandRunner
  }

  it should "treat Ctrl+Backspace and Ctrl+Delete as word deletion in editor focus" in {
    val translator = FocusedInputTranslator.forState(editorState)

    translator.translate(KeyStrokeInfo(InputKey.Backspace, None, Set(Modifier.Ctrl))) shouldBe DeleteWordBackward
    translator.translate(KeyStrokeInfo(InputKey.Delete, None, Set(Modifier.Ctrl))) shouldBe DeleteWordForward
  }

  it should "treat PageUp, PageDown, Ctrl+Home, and Ctrl+End as file navigation in editor focus" in {
    val translator = FocusedInputTranslator.forState(editorState)

    translator.translate(KeyStrokeInfo(InputKey.PageUp, None, Set.empty)) shouldBe PageUp
    translator.translate(KeyStrokeInfo(InputKey.PageDown, None, Set.empty)) shouldBe PageDown
    translator.translate(KeyStrokeInfo(InputKey.Home, None, Set(Modifier.Ctrl))) shouldBe MoveToStartOfFile
    translator.translate(KeyStrokeInfo(InputKey.End, None, Set(Modifier.Ctrl))) shouldBe MoveToEndOfFile
  }

  it should "treat Shift-arrow keys as selection extension in editor focus" in {
    val translator = FocusedInputTranslator.forState(editorState)
    val shift      = Set(Modifier.Shift)

    translator.translate(KeyStrokeInfo(InputKey.ArrowLeft, None, shift)) shouldBe ExtendSelectionLeft
    translator.translate(KeyStrokeInfo(InputKey.ArrowRight, None, shift)) shouldBe ExtendSelectionRight
    translator.translate(KeyStrokeInfo(InputKey.ArrowUp, None, shift)) shouldBe ExtendSelectionUp
    translator.translate(KeyStrokeInfo(InputKey.ArrowDown, None, shift)) shouldBe ExtendSelectionDown
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

    translator.translate(KeyStrokeInfo(InputKey.Enter, None, Set.empty)) shouldBe ModalSubmit
    translator.translate(KeyStrokeInfo(InputKey.Tab, None, Set.empty)) shouldBe ModalNextField
    translator.translate(KeyStrokeInfo(InputKey.Backspace, None, Set(Modifier.Ctrl))) shouldBe ModalDeleteWordBackward
    translator.translate(KeyStrokeInfo(InputKey.Delete, None, Set(Modifier.Ctrl))) shouldBe ModalDeleteWordForward
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

    translator.translate(KeyStrokeInfo(InputKey.ArrowUp, None, Set.empty)) shouldBe PanelInputEvent.Navigate(
      Direction.Up
    )
    translator.translate(KeyStrokeInfo(InputKey.Character, Some('x'), Set.empty)) shouldBe PanelInputEvent.ReturnFocus
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

    translator.translate(KeyStrokeInfo(InputKey.ArrowDown, None, Set.empty)) shouldBe PeekInputEvent.Navigate(
      Direction.Down
    )
    translator.translate(KeyStrokeInfo(InputKey.Enter, None, Set.empty)) shouldBe PeekInputEvent.Accept
  }

  it should "treat comment lens focus as form editing input" in {
    val commentState = editorState.copy(
      focus = Focus.Surface(SurfaceId("comment-lens")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("comment-lens"),
          SurfaceContent.CommentLens(
            CommentLensState(
              RenderedComment(0, "Review this", "Review this"),
              "Review this",
              11,
              Some(DocumentComment(CursorPosition(0, 0), CursorPosition(0, 6), "Review this"))
            )
          ),
          SurfacePresentation.Floating(None, SurfacePlacement.AboveCursor)
        )
      )
    )
    val translator = FocusedInputTranslator.forState(commentState)

    translator.translate(KeyStrokeInfo(InputKey.Character, Some('!'), Set.empty)) shouldBe ModalInsertChar('!')
    translator.translate(KeyStrokeInfo(InputKey.Enter, None, Set.empty)) shouldBe ModalSubmit
    translator.translate(KeyStrokeInfo(InputKey.Escape, None, Set.empty)) shouldBe ModalDismiss
  }

  it should "route Tab and Shift+Tab to command-runner category navigation" in {
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

    translator.translate(KeyStrokeInfo(InputKey.Tab, None, Set.empty)) shouldBe RunnerNextCategory
    translator.translate(KeyStrokeInfo(InputKey.ReverseTab, None, Set.empty)) shouldBe RunnerPreviousCategory
  }

  it should "treat submenu focus as command-runner input rather than peek input" in {
    val runner = CommandRunner.empty
    val submenuState = editorState.copy(
      focus = Focus.Surface(SurfaceId("command-runner-submenu")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        ),
        UiSurface(
          SurfaceId("command-runner-submenu"),
          SurfaceContent.CommandPaletteSubmenu(runner, "settings-animation", previewOnly = false),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      )
    )
    val translator = FocusedInputTranslator.forState(submenuState)

    translator.translate(KeyStrokeInfo(InputKey.Enter, None, Set.empty)) shouldBe RunnerSubmit
    translator.translate(KeyStrokeInfo(InputKey.Escape, None, Set.empty)) shouldBe RunnerDismiss
    translator.translate(KeyStrokeInfo(InputKey.Backspace, None, Set(Modifier.Ctrl))) shouldBe RunnerDeleteWordBackward
    translator.translate(KeyStrokeInfo(InputKey.Delete, None, Set(Modifier.Ctrl))) shouldBe RunnerDeleteWordForward
  }

  it should "keep routing input to the command runner while its surfaces remain open" in {
    val leakedFocusState = editorState.copy(
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(com.serenity.command.CommandRunner.empty),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      )
    )
    val translator = FocusedInputTranslator.forState(leakedFocusState)

    translator.translate(KeyStrokeInfo(InputKey.Escape, None, Set.empty)) shouldBe RunnerDismiss
    translator.translate(KeyStrokeInfo(InputKey.Enter, None, Set.empty)) shouldBe RunnerSubmit
  }

  it should "route Ctrl+Tab and Ctrl+Shift+Tab to pane navigation regardless of focus" in {
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
    val runnerTranslator = FocusedInputTranslator.forState(commandRunnerState)
    val editorTranslator = FocusedInputTranslator.forState(editorState)

    runnerTranslator.translate(KeyStrokeInfo(InputKey.Tab, None, Set(Modifier.Ctrl))) shouldBe NextTab
    editorTranslator.translate(KeyStrokeInfo(InputKey.Tab, None, Set(Modifier.Ctrl))) shouldBe NextTab

    runnerTranslator.translate(KeyStrokeInfo(InputKey.ReverseTab, None, Set(Modifier.Ctrl))) shouldBe PreviousTab
    editorTranslator.translate(KeyStrokeInfo(InputKey.ReverseTab, None, Set(Modifier.Ctrl))) shouldBe PreviousTab
  }

  it should "respect configured global hotkey overrides in focused routing" in {
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
    val configuredState = commandRunnerState.copy(
      config = commandRunnerState.config.withHotkeyOverride(
        HotkeyAction.ToggleCommandRunner,
        "ctrl+k"
      )
    )
    val translator = FocusedInputTranslator.forState(configuredState)

    translator.translate(KeyStrokeInfo(InputKey.Character, Some('k'), Set(Modifier.Ctrl))) shouldBe ToggleCommandRunner
    translator
      .translate(KeyStrokeInfo(InputKey.Character, Some('p'), Set(Modifier.Ctrl)))
      .isInstanceOf[
        com.serenity.keystroke.events.UnhandledEvent[?]
      ] shouldBe true
  }

  it should "respect configured command-runner keymap overrides" in {
    val commandRunnerState = editorState.copy(
      focus = Focus.Surface(SurfaceId("command-runner")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(com.serenity.command.CommandRunner.empty),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      ),
      config = editorState.config.withCommandRunnerKeyOverride(CommandRunnerKeyAction.Submit, "ctrl+enter")
    )
    val translator = FocusedInputTranslator.forState(commandRunnerState)

    translator
      .translate(KeyStrokeInfo(InputKey.Enter, None, Set.empty))
      .isInstanceOf[
        com.serenity.keystroke.events.UnhandledEvent[?]
      ] shouldBe true
    translator.translate(KeyStrokeInfo(InputKey.Enter, None, Set(Modifier.Ctrl))) shouldBe RunnerSubmit
  }

  it should "respect configured modal keymap overrides" in {
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
      ),
      config = editorState.config.withModalKeyOverride(ModalKeyAction.Dismiss, "ctrl+escape")
    )
    val translator = FocusedInputTranslator.forState(modalState)

    translator
      .translate(KeyStrokeInfo(InputKey.Escape, None, Set.empty))
      .isInstanceOf[
        com.serenity.keystroke.events.UnhandledEvent[?]
      ] shouldBe true
    translator.translate(KeyStrokeInfo(InputKey.Escape, None, Set(Modifier.Ctrl))) shouldBe ModalDismiss
  }

  it should "respect configured panel keymap overrides" in {
    val panelState = editorState.copy(
      focus = Focus.Surface(SurfaceId("left-panel")),
      uiSurfaces = List(
        UiSurface.fromPanelContent(
          SurfaceId("left-panel"),
          PanelContent.DirectoryTree(DirectoryTreeData(java.nio.file.Paths.get("/repo")), None),
          PanelPosition.Left,
          24
        )
      ),
      config = editorState.config.withPanelKeyOverride(PanelKeyAction.Activate, "ctrl+enter")
    )
    val translator = FocusedInputTranslator.forState(panelState)

    translator
      .translate(KeyStrokeInfo(InputKey.Enter, None, Set.empty))
      .isInstanceOf[
        com.serenity.keystroke.events.UnhandledEvent[?]
      ] shouldBe true
    translator.translate(KeyStrokeInfo(InputKey.Enter, None, Set(Modifier.Ctrl))) shouldBe PanelInputEvent.Activate
  }

  it should "respect configured peek keymap overrides" in {
    val peekState = editorState.copy(
      focus = Focus.Surface(SurfaceId("peek")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("peek"),
          SurfaceContent.QuickInfo("map"),
          SurfacePresentation.Floating(None, SurfacePlacement.AboveCursor)
        )
      ),
      config = editorState.config.withPeekKeyOverride(PeekKeyAction.Dismiss, "ctrl+escape")
    )
    val translator = FocusedInputTranslator.forState(peekState)

    translator
      .translate(KeyStrokeInfo(InputKey.Escape, None, Set.empty))
      .isInstanceOf[
        com.serenity.keystroke.events.UnhandledEvent[?]
      ] shouldBe true
    translator.translate(KeyStrokeInfo(InputKey.Escape, None, Set(Modifier.Ctrl))) shouldBe PeekInputEvent.Dismiss
  }
