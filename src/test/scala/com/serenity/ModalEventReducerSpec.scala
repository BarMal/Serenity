package com.serenity

import com.serenity.keystroke.events.{Enter, InsertChar, MoveDown, ReverseTabKey, TabKey}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId, CloseScope, CloseWorkflowChoice, CloseWorkflowState, CursorPosition, FileWorkflowField, FileWorkflowMode, FileWorkflowState, FileWorkflowSuggestion, FindState, Focus, Modal, ModalType, PaneId, ReplaceWorkflowField, ReplaceWorkflowState, SurfaceContent, SurfaceId, SurfacePlacement, SurfacePresentation, UiSurface}
import com.serenity.state.reducers.{AppEffect, ModalEventReducer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModalEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "ModalEventReducer" should "append digits in goto line mode" in {
    val initialState = AppState.initial.copy(
      uiSurfaces = List(
        UiSurface(
          SurfaceId("goto-line"),
          SurfaceContent.ModalWorkflow(Modal.GotoLine("1")),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      ),
      focus = Focus.Surface(SurfaceId("goto-line"))
    )

    val updatedState = ModalEventReducer.reduce(ModalType.GotoLine, InsertChar('2'), initialState).state

    updatedState.modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.GotoLine("12")))
  }

  it should "jump to the requested line and dismiss the goto line modal" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      uiSurfaces = List(
        UiSurface(
          SurfaceId("goto-line"),
          SurfaceContent.ModalWorkflow(Modal.GotoLine("3")),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      ),
      focus = Focus.Surface(SurfaceId("goto-line")),
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial.buffers(bufferId).copy(content = com.serenity.rope.Rope("a\nb\nc\nd"))
      )
    )

    val updatedState = ModalEventReducer.reduce(ModalType.GotoLine, Enter, initialState).state

    updatedState.modalSurface shouldBe None
    updatedState.focus shouldBe Focus.EditorPane(paneId)
    updatedState.buffers(bufferId).cursors.head shouldBe CursorPosition(2, 0)
  }

  it should "compute find results and move the cursor to the first hit" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      uiSurfaces = List(
        UiSurface(
          SurfaceId("find"),
          SurfaceContent.ModalWorkflow(Modal.Find("needle", Nil, 0)),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      ),
      focus = Focus.Surface(SurfaceId("find")),
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial.buffers(bufferId).copy(content = com.serenity.rope.Rope("x\nneedle here\ny\nneedle again"))
      )
    )

    val updatedState = ModalEventReducer.reduce(ModalType.Find, Enter, initialState).state

    updatedState.modalSurface shouldBe None
    updatedState.focus shouldBe Focus.EditorPane(paneId)
    updatedState.findState shouldBe Some(FindState("needle", List(1, 3), 0))
    updatedState.buffers(bufferId).cursors.head shouldBe CursorPosition(1, 0)
  }

  it should "update filename and path fields independently in file workflow mode" in {
    val initialState = AppState.initial.copy(
      uiSurfaces = List(
        UiSurface(
          SurfaceId("file-workflow"),
          SurfaceContent.ModalWorkflow(
            Modal.FileWorkflow(
              FileWorkflowState(mode = FileWorkflowMode.SaveAs)
            )
          ),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      ),
      focus = Focus.Surface(SurfaceId("file-workflow"))
    )

    val withFilenameResult = ModalEventReducer.reduce(ModalType.FileWorkflow, InsertChar('n'), initialState)
    withFilenameResult.effects shouldBe List(AppEffect.RefreshFileWorkflow(SurfaceId("file-workflow")))
    val withFilename = withFilenameResult.state
    withFilename.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.FileWorkflow(
          FileWorkflowState(mode = FileWorkflowMode.SaveAs, filename = "n")
        )
      )
    )

    val withPathFieldFocusResult = ModalEventReducer.reduce(ModalType.FileWorkflow, TabKey, withFilename)
    withPathFieldFocusResult.effects shouldBe List(AppEffect.RefreshFileWorkflow(SurfaceId("file-workflow")))
    val withPathFieldFocus = withPathFieldFocusResult.state
    val withPathResult = ModalEventReducer.reduce(ModalType.FileWorkflow, InsertChar('/'), withPathFieldFocus)
    withPathResult.effects shouldBe List(AppEffect.RefreshFileWorkflow(SurfaceId("file-workflow")))
    val withPath = withPathResult.state

    withPath.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.FileWorkflow(
          FileWorkflowState(
            mode = FileWorkflowMode.SaveAs,
            filename = "n",
            path = "/",
            activeField = FileWorkflowField.Path
          )
        )
      )
    )
  }

  it should "cycle file workflow focus between filename and path with tab and reverse-tab" in {
    val initialState = AppState.initial.copy(
      uiSurfaces = List(
        UiSurface(
          SurfaceId("file-workflow"),
          SurfaceContent.ModalWorkflow(
            Modal.FileWorkflow(
              FileWorkflowState(mode = FileWorkflowMode.Open)
            )
          ),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      ),
      focus = Focus.Surface(SurfaceId("file-workflow"))
    )

    val pathFocusedResult = ModalEventReducer.reduce(ModalType.FileWorkflow, TabKey, initialState)
    pathFocusedResult.effects shouldBe List(AppEffect.RefreshFileWorkflow(SurfaceId("file-workflow")))
    val pathFocused = pathFocusedResult.state
    pathFocused.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.FileWorkflow(
          FileWorkflowState(mode = FileWorkflowMode.Open, activeField = FileWorkflowField.Path)
        )
      )
    )

    val filenameFocusedResult = ModalEventReducer.reduce(ModalType.FileWorkflow, ReverseTabKey, pathFocused)
    filenameFocusedResult.effects shouldBe List(AppEffect.RefreshFileWorkflow(SurfaceId("file-workflow")))
    val filenameFocused = filenameFocusedResult.state
    filenameFocused.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.FileWorkflow(
          FileWorkflowState(mode = FileWorkflowMode.Open, activeField = FileWorkflowField.Filename)
        )
      )
    )
  }

  it should "move through path suggestions and accept the selected suggestion in file workflow mode" in {
    val initialWorkflow = FileWorkflowState(
      mode = FileWorkflowMode.Open,
      path = "/tmp",
      activeField = FileWorkflowField.Path,
      suggestions = List(
        FileWorkflowSuggestion("/tmp/alpha", isDirectory = true),
        FileWorkflowSuggestion("/tmp/beta", isDirectory = true)
      )
    )
    val initialState = AppState.initial.copy(
      uiSurfaces = List(
        UiSurface(
          SurfaceId("file-workflow"),
          SurfaceContent.ModalWorkflow(Modal.FileWorkflow(initialWorkflow)),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      ),
      focus = Focus.Surface(SurfaceId("file-workflow"))
    )

    val moved = ModalEventReducer.reduce(ModalType.FileWorkflow, MoveDown, initialState).state
    moved.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.FileWorkflow(initialWorkflow.copy(selectedSuggestionIndex = 1))
      )
    )

    val acceptedResult = ModalEventReducer.reduce(ModalType.FileWorkflow, Enter, moved)
    acceptedResult.effects shouldBe List(AppEffect.RefreshFileWorkflow(SurfaceId("file-workflow")))
    val accepted = acceptedResult.state
    accepted.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.FileWorkflow(
          initialWorkflow.copy(
            path = s"/tmp/beta${java.io.File.separator}",
            selectedSuggestionIndex = 1
          )
        )
      )
    )
  }

  it should "queue file workflow submission when enter is pressed without an active suggestion to accept" in {
    val initialWorkflow = FileWorkflowState(
      mode = FileWorkflowMode.SaveAs,
      filename = "notes.scala",
      path = "/tmp/project"
    )
    val initialState = AppState.initial.copy(
      uiSurfaces = List(
        UiSurface(
          SurfaceId("file-workflow"),
          SurfaceContent.ModalWorkflow(Modal.FileWorkflow(initialWorkflow)),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      ),
      focus = Focus.Surface(SurfaceId("file-workflow"))
    )

    val result = ModalEventReducer.reduce(ModalType.FileWorkflow, Enter, initialState)

    result.state shouldBe initialState
    result.effects shouldBe List(AppEffect.SubmitFileWorkflow(SurfaceId("file-workflow")))
  }

  it should "edit replace workflow fields, switch active field, and queue submission" in {
    val initialWorkflow = ReplaceWorkflowState()
    val initialState = AppState.initial.copy(
      uiSurfaces = List(
        UiSurface(
          SurfaceId("replace-workflow"),
          SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(initialWorkflow)),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      ),
      focus = Focus.Surface(SurfaceId("replace-workflow"))
    )

    val withFind = ModalEventReducer.reduce(ModalType.ReplaceWorkflow, InsertChar('n'), initialState).state
    withFind.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          initialWorkflow.copy(findText = "n")
        )
      )
    )

    val withReplacementField = ModalEventReducer.reduce(ModalType.ReplaceWorkflow, TabKey, withFind).state
    withReplacementField.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          initialWorkflow.copy(findText = "n", activeField = ReplaceWorkflowField.ReplaceWith)
        )
      )
    )

    val withReplacement = ModalEventReducer.reduce(ModalType.ReplaceWorkflow, InsertChar('x'), withReplacementField).state
    withReplacement.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          initialWorkflow.copy(
            findText = "n",
            replacementText = "x",
            activeField = ReplaceWorkflowField.ReplaceWith
          )
        )
      )
    )

    val submitted = ModalEventReducer.reduce(ModalType.ReplaceWorkflow, Enter, withReplacement)
    submitted.state shouldBe withReplacement
    submitted.effects shouldBe List(AppEffect.SubmitReplaceWorkflow(SurfaceId("replace-workflow")))
  }

  it should "cycle close workflow choices and queue close workflow submission on enter" in {
    val initialWorkflow = CloseWorkflowState(
      scope = CloseScope.Current,
      currentBufferId = BufferId(0),
      currentBufferLabel = "notes.scala"
    )
    val initialState = AppState.initial.copy(
      uiSurfaces = List(
        UiSurface(
          SurfaceId("close-workflow"),
          SurfaceContent.ModalWorkflow(Modal.CloseWorkflow(initialWorkflow)),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      ),
      focus = Focus.Surface(SurfaceId("close-workflow"))
    )

    val moved = ModalEventReducer.reduce(ModalType.CloseWorkflow, TabKey, initialState).state
    moved.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.CloseWorkflow(initialWorkflow.copy(selectedChoice = CloseWorkflowChoice.Discard))
      )
    )

    val result = ModalEventReducer.reduce(ModalType.CloseWorkflow, Enter, moved)
    result.state shouldBe moved
    result.effects shouldBe List(AppEffect.SubmitCloseWorkflow(SurfaceId("close-workflow")))
  }
