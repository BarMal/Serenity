package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalEventReducer, WorkflowEffect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModalFileWorkflowReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "ModalEventReducer" should "emit CreateFileWorkflowDirectories only for a save-as workflow with missing directories" in {
    val saveAsWithMissingDirs = ModalDialog(
      SurfaceId("save-as"),
      Modal.FileWorkflow(
        FileWorkflowState(
          mode = FileWorkflowMode.SaveAs,
          filename = "notes.scala",
          path = "/tmp/project/new/nested",
          missingPathSegments = List("new", "nested")
        )
      ),
      ModalPlacement.Centered
    )
    val readyState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Modal),
      runtime = AppState.initial.runtime.copy(modalStack = List(saveAsWithMissingDirs))
    )

    val result = ModalEventReducer.reduce(ModalType.FileWorkflow, ModalCreateDirectory, readyState)
    result.effects shouldBe List(
      AppEffect.Workflow(WorkflowEffect.CreateFileWorkflowDirectories(SurfaceId("save-as")))
    )

    val openWithMissingDirs = ModalDialog(
      SurfaceId("open"),
      Modal.FileWorkflow(
        FileWorkflowState(
          mode = FileWorkflowMode.Open,
          path = "/tmp/project/new/nested",
          missingPathSegments = List("new", "nested")
        )
      ),
      ModalPlacement.Centered
    )
    val openState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Modal),
      runtime = AppState.initial.runtime.copy(modalStack = List(openWithMissingDirs))
    )
    ModalEventReducer.reduce(ModalType.FileWorkflow, ModalCreateDirectory, openState).effects shouldBe Nil

    val saveAsWithoutMissingDirs = ModalDialog(
      SurfaceId("save-as-clean"),
      Modal.FileWorkflow(FileWorkflowState(mode = FileWorkflowMode.SaveAs)),
      ModalPlacement.Centered
    )
    val cleanState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Modal),
      runtime = AppState.initial.runtime.copy(modalStack = List(saveAsWithoutMissingDirs))
    )
    ModalEventReducer.reduce(ModalType.FileWorkflow, ModalCreateDirectory, cleanState).effects shouldBe Nil
  }

  it should "update filename and path fields independently in file workflow mode" in {
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(SurfaceId("file-workflow"))),
      runtime = AppState.initial.runtime.copy(
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
        )
      )
    )

    val withFilenameResult = ModalEventReducer.reduce(ModalType.FileWorkflow, InsertChar('n'), initialState)
    withFilenameResult.effects shouldBe List(
      AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(SurfaceId("file-workflow")))
    )
    val withFilename = withFilenameResult.state
    withFilename.modalSurface.flatMap {
      _.content match
        case SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)) => Some(workflow)
        case _                                                          => None
    } shouldBe defined
    withFilename.modalSurface.flatMap {
      _.content match
        case SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)) => Some(workflow)
        case _                                                          => None
    }.get shouldBe a[SaveAsFileWorkflowState]
    withFilename.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.FileWorkflow(
          FileWorkflowState(mode = FileWorkflowMode.SaveAs, filename = "n")
        )
      )
    )

    // SaveAs now cycles Filename -> Format -> Path, so it takes two tabs to reach Path.
    val withFormatFieldFocusResult = ModalEventReducer.reduce(ModalType.FileWorkflow, TabKey, withFilename)
    withFormatFieldFocusResult.effects shouldBe List(
      AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(SurfaceId("file-workflow")))
    )
    val withPathFieldFocusResult =
      ModalEventReducer.reduce(ModalType.FileWorkflow, TabKey, withFormatFieldFocusResult.state)
    withPathFieldFocusResult.effects shouldBe List(
      AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(SurfaceId("file-workflow")))
    )
    val withPathFieldFocus = withPathFieldFocusResult.state
    val withPathResult     = ModalEventReducer.reduce(ModalType.FileWorkflow, InsertChar('/'), withPathFieldFocus)
    withPathResult.effects shouldBe List(
      AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(SurfaceId("file-workflow")))
    )
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

  it should "keep Open's Path focus with tab and reverse-tab when there are no suggestions" in {
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(SurfaceId("file-workflow"))),
      runtime = AppState.initial.runtime.copy(
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
        )
      )
    )

    val tabResult = ModalEventReducer.reduce(ModalType.FileWorkflow, TabKey, initialState)
    tabResult.effects shouldBe List(
      AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(SurfaceId("file-workflow")))
    )
    tabResult.state.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.FileWorkflow(
          FileWorkflowState(mode = FileWorkflowMode.Open, activeField = FileWorkflowField.Path)
        )
      )
    )

    val reverseTabResult = ModalEventReducer.reduce(ModalType.FileWorkflow, ReverseTabKey, tabResult.state)
    reverseTabResult.effects shouldBe List(
      AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(SurfaceId("file-workflow")))
    )
    reverseTabResult.state.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.FileWorkflow(
          FileWorkflowState(mode = FileWorkflowMode.Open, activeField = FileWorkflowField.Path)
        )
      )
    )
  }

  it should "move through path suggestions and accept the selected suggestion in file workflow mode with tab" in {
    val initialWorkflow = OpenFileWorkflowState(
      path = "/tmp",
      activeField = FileWorkflowField.Path,
      suggestions = List(
        FileWorkflowSuggestion("/tmp/alpha", isDirectory = true),
        FileWorkflowSuggestion("/tmp/beta", isDirectory = true)
      )
    )
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(SurfaceId("file-workflow"))),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("file-workflow"),
            SurfaceContent.ModalWorkflow(Modal.FileWorkflow(initialWorkflow)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val moved = ModalEventReducer.reduce(ModalType.FileWorkflow, ModalNavigate(Direction.Down), initialState).state
    moved.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.FileWorkflow(initialWorkflow.copy(selectedSuggestionIndex = 1))
      )
    )

    val acceptedResult = ModalEventReducer.reduce(ModalType.FileWorkflow, TabKey, moved)
    acceptedResult.effects shouldBe List(
      AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(SurfaceId("file-workflow")))
    )
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

  it should "cycle the save-as format field with up/down when it is active" in {
    val initialWorkflow = SaveAsFileWorkflowState(
      filename = "notes.txt",
      activeField = FileWorkflowField.Format
    )
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(SurfaceId("file-workflow"))),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("file-workflow"),
            SurfaceContent.ModalWorkflow(Modal.FileWorkflow(initialWorkflow)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val down = ModalEventReducer.reduce(ModalType.FileWorkflow, ModalNavigate(Direction.Down), initialState).state
    down.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(Modal.FileWorkflow(initialWorkflow.cycleFormat(1)))
    )

    val up = ModalEventReducer.reduce(ModalType.FileWorkflow, ModalNavigate(Direction.Up), initialState).state
    up.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(Modal.FileWorkflow(initialWorkflow.cycleFormat(-1)))
    )
  }

  it should "keep the existing suggestion-cycling behavior for up/down on other fields, and for open workflows" in {
    val filenameActiveWorkflow = SaveAsFileWorkflowState(
      filename = "notes",
      activeField = FileWorkflowField.Filename,
      suggestions = List(FileWorkflowSuggestion("notes.txt"), FileWorkflowSuggestion("notes.md"))
    )
    val filenameActiveState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(SurfaceId("file-workflow"))),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("file-workflow"),
            SurfaceContent.ModalWorkflow(Modal.FileWorkflow(filenameActiveWorkflow)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val filenameActiveDown =
      ModalEventReducer.reduce(ModalType.FileWorkflow, ModalNavigate(Direction.Down), filenameActiveState).state
    filenameActiveDown.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(Modal.FileWorkflow(filenameActiveWorkflow.moveSuggestion(1)))
    )

    val openWorkflow = OpenFileWorkflowState(
      path = "/tmp",
      activeField = FileWorkflowField.Path,
      suggestions = List(
        FileWorkflowSuggestion("/tmp/alpha", isDirectory = true),
        FileWorkflowSuggestion("/tmp/beta", isDirectory = true)
      )
    )
    val openState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(SurfaceId("file-workflow"))),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("file-workflow"),
            SurfaceContent.ModalWorkflow(Modal.FileWorkflow(openWorkflow)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val openDown = ModalEventReducer.reduce(ModalType.FileWorkflow, ModalNavigate(Direction.Down), openState).state
    openDown.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(Modal.FileWorkflow(openWorkflow.moveSuggestion(1)))
    )
  }

  it should "queue file workflow submission when enter is pressed even if suggestions are present" in {
    val initialWorkflow = OpenFileWorkflowState(
      path = "/tmp",
      activeField = FileWorkflowField.Path,
      suggestions = List(
        FileWorkflowSuggestion("/tmp/alpha", isDirectory = true)
      )
    )
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(SurfaceId("file-workflow"))),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("file-workflow"),
            SurfaceContent.ModalWorkflow(Modal.FileWorkflow(initialWorkflow)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val result = ModalEventReducer.reduce(ModalType.FileWorkflow, Enter, initialState)

    result.state shouldBe initialState
    result.effects shouldBe List(AppEffect.Workflow(WorkflowEffect.SubmitFileWorkflow(SurfaceId("file-workflow"))))
  }

  it should "accept the selected filename suggestion with tab in open workflow mode" in {
    val initialWorkflow = OpenFileWorkflowState(
      filename = "be",
      path = "/tmp",
      activeField = FileWorkflowField.Filename,
      suggestions = List(
        FileWorkflowSuggestion("beta.scala", isDirectory = false)
      )
    )
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(SurfaceId("file-workflow"))),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("file-workflow"),
            SurfaceContent.ModalWorkflow(Modal.FileWorkflow(initialWorkflow)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val result = ModalEventReducer.reduce(ModalType.FileWorkflow, TabKey, initialState)

    result.effects shouldBe List(AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(SurfaceId("file-workflow"))))
    result.state.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.FileWorkflow(initialWorkflow.copy(filename = "beta.scala"))
      )
    )
  }

  it should "queue file workflow submission when enter is pressed without suggestions" in {
    val initialWorkflow = SaveAsFileWorkflowState(
      filename = "notes.scala",
      path = "/tmp/project"
    )
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(SurfaceId("file-workflow"))),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("file-workflow"),
            SurfaceContent.ModalWorkflow(Modal.FileWorkflow(initialWorkflow)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val result = ModalEventReducer.reduce(ModalType.FileWorkflow, Enter, initialState)

    result.state shouldBe initialState
    result.effects shouldBe List(AppEffect.Workflow(WorkflowEffect.SubmitFileWorkflow(SurfaceId("file-workflow"))))
  }
