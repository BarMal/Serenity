package com.serenity.state.manager

import java.nio.file.Paths

import com.serenity.DockedPanelFixtures
import com.serenity.command.SessionCommands
import com.serenity.keystroke.events.ModalSubmit
import com.serenity.rope.Balance
import com.serenity.session.{SessionId, SessionMetadata}
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.{ModalEventReducer, ModalStateReducer}
import com.serenity.ui.layout.PanelPosition
import com.serenity.ui.widget.Loadable
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
    val workflow        = close.pending(begun.state).getOrElse(fail("no close pending"))
    val answered        = begun.state.dismissTopModal

    val abandoned = close.abandoned(workflow, answered)

    begun.completed shouldBe None
    begun.state.activeBuffer.map(_.id) shouldBe Some(BufferId(0))
    abandoned.runtime.modalStack shouldBe empty
    abandoned.runtime.actionStack shouldBe empty
    abandoned.activeBuffer.map(_.id) shouldBe Some(second)
  }

  "A save that conflicts during a quit" should "stop the quit and ask about the conflict" in {
    val (state, _) = twoUnsavedBuffers
    val begun      = close.begun(CloseScope.Quit, state)
    val workflow   = close.pending(begun.state).getOrElse(fail("no close pending"))

    val conflicted = close.conflicted(workflow, begun.state.dismissTopModal)

    conflicted.runtime.actionStack shouldBe empty
    conflicted.runtime.modalStack.map(_.modal) should matchPattern {
      case List(Modal.Confirm(prompt))
          if prompt.choices.items.headOption
            .map(_.action)
            .contains(
              ConfirmAction.Run(com.serenity.command.ExternalChangeCommands.reloadFromDisk(BufferId(0)))
            ) =>
    }
  }

  "Resolving the last unsaved buffer of a quit" should "complete the quit" in {
    val (state, second) = twoUnsavedBuffers
    val begun           = close.begun(CloseScope.Quit, state)
    val first           = close.pending(begun.state)
    val next            = close.resolved(first.getOrElse(fail("no workflow")), begun.state)
    val last            = close.pending(next.state)

    last.map(_.currentBufferId) shouldBe Some(second)
    next.completed shouldBe None
    close.resolved(last.getOrElse(fail("no workflow")), next.state).completed shouldBe Some(CloseScope.Quit)
  }

  private val draft = SessionMetadata(SessionId("draft"), "Draft", "draft.json", 0L, 0L)

  private def withPicker(state: AppState, purpose: SessionListPurpose): (AppState, SurfaceId) =
    SessionWorkflowTransitions.withSessionPickerOpened(state, purpose).getOrElse(fail("no picker"))

  private def picker(state: AppState, id: SurfaceId): Option[ListPicker] =
    state.runtime.uiSurfaces.find(_.id == id).collect {
      case UiSurface(_, SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)), _, _) => picker
    }

  /** The picker listed with `draft`, and `draft` picked: the picker now waits on its load. */
  private def pickedDraft: (AppState, SurfaceId) =
    val (opened, id) = withPicker(AppState.initial, SessionListPurpose.Open)
    val listed = SessionWorkflowTransitions.withSessionsListed(opened, id, SessionListPurpose.Open, Right(List(draft)))
    (ModalEventReducer.reduce(ModalType.ListPicker, ModalSubmit, listed).state, id)

  "A session listing" should "fill the picker it was taken for, one choice per session" in {
    val (opened, id) = withPicker(AppState.initial, SessionListPurpose.Open)
    picker(opened, id).map(_.items) shouldBe Some(Loadable.Loading())

    val listed = SessionWorkflowTransitions.withSessionsListed(opened, id, SessionListPurpose.Open, Right(List(draft)))

    picker(listed, id).flatMap(_.selectedChoice).map(_.action) shouldBe
      Some(SessionCommands.openNamedSession(draft.id))
  }

  it should "offer each session for renaming without waiting, for the Rename picker" in {
    val (opened, id) = withPicker(AppState.initial, SessionListPurpose.Rename)

    val listed =
      SessionWorkflowTransitions.withSessionsListed(opened, id, SessionListPurpose.Rename, Right(List(draft)))

    picker(listed, id).flatMap(_.selectedChoice).map(choice => (choice.action, choice.waitingLabel)) shouldBe
      Some((SessionCommands.renameNamedSession(draft.id, "Draft"), None))
  }

  it should "say there are none, or why they couldn't be read" in {
    val (opened, id) = withPicker(AppState.initial, SessionListPurpose.Open)

    picker(SessionWorkflowTransitions.withSessionsListed(opened, id, SessionListPurpose.Open, Right(Nil)), id)
      .map(_.items) shouldBe Some(Loadable.Empty("No saved sessions"))
    picker(SessionWorkflowTransitions.withSessionsListed(opened, id, SessionListPurpose.Open, Left("disk full")), id)
      .map(_.items) shouldBe Some(Loadable.Failed("disk full"))
  }

  it should "be dropped once the picker has closed" in {
    val (opened, id) = withPicker(AppState.initial, SessionListPurpose.Open)
    val dismissed    = WorkflowSurfaces.dismissedToPriorFocus(opened, id)

    SessionWorkflowTransitions.withSessionsListed(dismissed, id, SessionListPurpose.Open, Right(List(draft))) shouldBe
      dismissed
  }

  "A named session loaded from the picker" should "replace the current session while the picker waits on it" in {
    val (picked, id) = pickedDraft
    val saved        = EditorTransitions.bufferCreated(AppState.initial, "saved", None).created
    picker(picked, id).flatMap(_.pending).map(_.action) shouldBe Some(SessionCommands.openNamedSession(draft.id))

    val restored = SessionWorkflowTransitions.withNamedSessionLoaded(picked, Some(id), Some(saved))

    restored.persisted.buffers.keySet shouldBe saved.persisted.buffers.keySet
    restored.runtime.uiSurfaces shouldBe empty
  }

  it should "keep the session's docked panels, so the restored workspace tree still validates" in {
    val (picked, id) = pickedDraft
    val outlineId    = SurfaceId("outline")
    val saved = DockedPanelFixtures.dock(
      EditorTransitions.bufferCreated(AppState.initial, "saved", None).created,
      outlineId,
      SurfaceContent.Outline(Nil, None),
      PanelPosition.Left,
      30
    )

    val restored = SessionWorkflowTransitions.withNamedSessionLoaded(picked, Some(id), Some(saved))

    restored.runtime.uiSurfaces.map(_.id) shouldBe List(outlineId)
    AppStateValidation.validationErrors(restored) shouldBe Nil
  }

  it should "be dropped once the picker was dismissed" in {
    val (picked, id) = pickedDraft
    val dismissed    = WorkflowSurfaces.dismissedToPriorFocus(picked, id)
    val saved        = EditorTransitions.bufferCreated(AppState.initial, "saved", None).created

    SessionWorkflowTransitions.withNamedSessionLoaded(dismissed, Some(id), Some(saved)) shouldBe dismissed
  }

  it should "apply when no picker asked for it" in {
    val saved = EditorTransitions.bufferCreated(AppState.initial, "saved", None).created

    SessionWorkflowTransitions
      .withNamedSessionLoaded(AppState.initial, None, Some(saved))
      .persisted
      .buffers
      .keySet shouldBe saved.persisted.buffers.keySet
  }

  it should "just close the picker when the session could not be read" in {
    val (picked, id) = pickedDraft

    val closed = SessionWorkflowTransitions.withNamedSessionLoaded(picked, Some(id), None)

    closed.runtime.uiSurfaces.exists(_.id == id) shouldBe false
    closed.persisted.buffers shouldBe picked.persisted.buffers
  }
end WorkflowTransitionsSpec
