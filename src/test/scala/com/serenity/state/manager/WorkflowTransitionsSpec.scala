package com.serenity.state.manager

import java.nio.file.Paths

import com.serenity.rope.Balance
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The file, close and session workflow decisions as plain functions of the state (#1697). */
class WorkflowTransitionsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val openDialog =
    FileWorkflowState(mode = FileWorkflowMode.Open, path = "/work/src/", activeField = FileWorkflowField.Path)

  private def withDialog(workflow: FileWorkflowState): (AppState, SurfaceId) =
    val shown = ModalStateReducer.show(Modal.FileWorkflow(workflow), AppState.initial).state
    (shown, shown.topModal.map(_.id).getOrElse(fail("no dialog")))

  private val listing =
    FileWorkflowListing(List(FileWorkflowSuggestion("/work/src/main", isDirectory = true)), missingPathSegments = Nil)

  "A directory listing" should "fill the dialog it was taken for, clearing its status" in {
    val (state, id) =
      withDialog(openDialog.updated(statusMessage = Some("File not found"), selectedSuggestionIndex = 4))

    val listed = FileWorkflowTransitions.withListing(state, id, openDialog, listing)

    val dialog = FileWorkflowTransitions.fileDialog(listed, id).getOrElse(fail("dialog gone"))
    dialog.suggestions shouldBe listing.suggestions
    dialog.selectedSuggestionIndex shouldBe 0
    dialog.statusMessage shouldBe None
  }

  it should "be dropped once the dialog shows another path" in {
    val (state, id) = withDialog(openDialog.updated(path = "/work/test/"))

    FileWorkflowTransitions.withListing(state, id, openDialog, listing) shouldBe state
  }

  it should "be dropped once the dialog has closed" in {
    val (state, id) = withDialog(openDialog)
    val closed      = state.dismissTopModal

    FileWorkflowTransitions.withListing(closed, id, openDialog, listing) shouldBe closed
  }

  "An Open target" should "browse into a directory" in {
    val (state, id) = withDialog(openDialog)

    val browsed =
      FileWorkflowTransitions.withTargetResolved(state, id, openDialog, FileWorkflowTarget.Directory(Paths.get("/a")))

    FileWorkflowTransitions.fileDialog(browsed, id).map(_.path) shouldBe Some("/a" + java.io.File.separator)
  }

  it should "report a missing file and keep the dialog open" in {
    val (state, id) = withDialog(openDialog)

    val reported =
      FileWorkflowTransitions.withTargetResolved(state, id, openDialog, FileWorkflowTarget.Missing(Paths.get("/a")))

    FileWorkflowTransitions.fileDialog(reported, id).flatMap(_.statusMessage) shouldBe Some("File not found: /a")
  }

  it should "close the dialog once its file has loaded" in {
    val (state, id) = withDialog(openDialog)

    val opened =
      FileWorkflowTransitions.withTargetResolved(
        state,
        id,
        openDialog,
        FileWorkflowTarget.ReadableFile(Paths.get("/a"))
      )

    opened.runtime.modalStack shouldBe empty
  }

  private val close = new CloseWorkflowTransitions(identity)

  /** Buffers 0 and `second`, both unsaved, with `second` shown. */
  private def twoUnsavedBuffers: (AppState, BufferId) =
    val creation = EditorTransitions.bufferCreated(AppState.initial, "second", None)
    val first    = creation.created.persisted.buffers(BufferId(0))
    val dirty = creation.created.copy(persisted =
      creation.created.persisted.copy(buffers =
        creation.created.persisted.buffers
          .updated(BufferId(0), first.copy(document = first.document.copy(isDirty = true)))
      )
    )
    (EditorState.focusBuffer(dirty, creation.bufferId), creation.bufferId)

  "Closing a background tab" should "hand the active tab back when the close is abandoned" in {
    val (state, second) = twoUnsavedBuffers
    val begun           = close.begun(CloseScope.Tab(BufferId(0), returnTo = Some(second)), state)
    val prompt          = begun.state.topModal.getOrElse(fail("no prompt"))
    val workflow        = close.closePrompt(begun.state, prompt.id).getOrElse(fail("not a close prompt"))

    val abandoned = close.abandoned(prompt.id, workflow, begun.state)

    begun.completed shouldBe None
    begun.state.activeBuffer.map(_.id) shouldBe Some(BufferId(0))
    abandoned.runtime.modalStack shouldBe empty
    abandoned.runtime.actionStack shouldBe empty
    abandoned.activeBuffer.map(_.id) shouldBe Some(second)
  }

  "A save that conflicts during a quit" should "stop the quit and ask about the conflict" in {
    val (state, _) = twoUnsavedBuffers
    val begun      = close.begun(CloseScope.Quit, state)
    val prompt     = begun.state.topModal.getOrElse(fail("no prompt"))
    val workflow   = close.closePrompt(begun.state, prompt.id).getOrElse(fail("not a close prompt"))

    val conflicted = close.conflicted(prompt.id, workflow, begun.state)

    conflicted.runtime.actionStack shouldBe empty
    conflicted.runtime.modalStack.map(_.modal) should matchPattern {
      case List(Modal.ReloadConflict(ReloadConflictState(BufferId(0), _, _))) =>
    }
  }

  "Resolving the last unsaved buffer of a quit" should "complete the quit" in {
    val (state, second) = twoUnsavedBuffers
    val begun           = close.begun(CloseScope.Quit, state)
    val first           = close.closePrompt(begun.state, begun.state.topModal.map(_.id).getOrElse(fail("no prompt")))
    val next            = close.resolved(first.getOrElse(fail("no workflow")), begun.state)
    val last = close.closePrompt(next.state, next.state.topModal.map(_.id).getOrElse(fail("no second prompt")))

    last.map(_.currentBufferId) shouldBe Some(second)
    next.completed shouldBe None
    close.resolved(last.getOrElse(fail("no workflow")), next.state).completed shouldBe Some(CloseScope.Quit)
  }

  private def withPicker(state: AppState): (AppState, SurfaceId) =
    val shown = SessionWorkflowTransitions.withSessionPicker(state, SessionListPurpose.Open, Nil)
    (shown, shown.runtime.uiSurfaces.lastOption.map(_.id).getOrElse(fail("no picker")))

  "A named session loaded from the picker" should "replace the current session while the picker is open" in {
    val (state, pickerId) = withPicker(AppState.initial)
    val saved             = EditorTransitions.bufferCreated(AppState.initial, "saved", None).created

    val restored = SessionWorkflowTransitions.withNamedSessionLoaded(state, pickerId, Some(saved))

    restored.persisted.buffers.keySet shouldBe saved.persisted.buffers.keySet
    restored.runtime.uiSurfaces shouldBe empty
  }

  it should "be dropped once the picker was dismissed" in {
    val (state, pickerId) = withPicker(AppState.initial)
    val dismissed         = WorkflowSurfaces.dismissedToEditor(state, pickerId)
    val saved             = EditorTransitions.bufferCreated(AppState.initial, "saved", None).created

    SessionWorkflowTransitions.withNamedSessionLoaded(dismissed, pickerId, Some(saved)) shouldBe dismissed
  }

  it should "just close the picker when the session could not be read" in {
    val (state, pickerId) = withPicker(AppState.initial)

    val closed = SessionWorkflowTransitions.withNamedSessionLoaded(state, pickerId, None)

    closed.runtime.uiSurfaces.exists(_.id == pickerId) shouldBe false
    closed.persisted.buffers shouldBe state.persisted.buffers
  }
end WorkflowTransitionsSpec
