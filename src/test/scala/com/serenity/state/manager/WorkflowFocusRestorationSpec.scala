package com.serenity.state.manager

import java.nio.file.{Files, Path, Paths}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{Enter, Event, ModalDismiss, ModalSubmit, PeekInputEvent}
import com.serenity.rope.Balance
import com.serenity.session.{SessionId, SessionMetadata}
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{ModalEventReducer, ModalStateReducer, PeekStateReducer}
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.{PanelPosition, PeekContent}
import com.serenity.{DockedPanelFixtures, TestTemp}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Where focus goes when a workflow opened over a focused pinned panel ends: a workflow whose result is a buffer on
  * show hands focus to the editor and forgets the panel; every other workflow, submitted or cancelled, returns to the
  * panel.
  */
class WorkflowFocusRestorationSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val outlineId  = SurfaceId("outline")
  private val panelFocus = Focus.Surface(outlineId)

  private def withFocusedPanel(state: AppState): AppState =
    val docked = DockedPanelFixtures.dock(state, outlineId, SurfaceContent.Outline(Nil, None), PanelPosition.Left, 30)
    docked.copy(persisted = docked.persisted.copy(focus = panelFocus))

  private def createStateManager(): StateManager =
    StateManager
      .apply(
        LoggerFactory[IO].getLogger(using LoggerName("WorkflowFocusRestorationSpec")),
        sessionRootOverride = Some(TestTemp.directory("workflow-focus-restoration-sessions")),
        dictionaryCache = SharedDictionary.default
      )
      .unsafeRunSync()

  /** A state manager whose pinned panel is focused, with `open` applied over it. */
  private def openedOverPanel(open: AppState => AppState): StateManager =
    val stateManager = createStateManager()
    stateManager.updateState(withFocusedPanel).unsafeRunSync()
    stateManager.updateState(open).unsafeRunSync()
    stateManager

  private def settled(stateManager: StateManager, event: Event): AppState =
    (stateManager.applyEvent(event) >> stateManager.runtimeLifecycle.awaitEffects >> stateManager.getCurrentState)
      .unsafeRunSync()

  private def showFileWorkflow(mode: FileWorkflowMode, directory: Path)(state: AppState): AppState =
    ModalStateReducer
      .show(
        Modal.FileWorkflow(FileWorkflowState(mode = mode, filename = "notes.txt", path = directory.toString)),
        state
      )
      .state

  private def editorFocused(state: AppState): Focus =
    Focus.EditorPane(state.persisted.layout.activeEditorPaneId.getOrElse(fail("no active editor pane")))

  private def valid(state: AppState): Boolean = AppStateValidation.validationErrors(state).isEmpty

  "An Open dialog opened over a focused panel" should "hand focus to the opened file's editor and forget the panel" in {
    val directory = TestTemp.directory("workflow-focus-open")
    val target    = Files.writeString(directory.resolve("notes.txt"), "opened")
    try
      val opened = settled(openedOverPanel(showFileWorkflow(FileWorkflowMode.Open, directory)), Enter)

      opened.topModal shouldBe None
      opened.activeBuffer.flatMap(_.document.filePath) shouldBe Some(target)
      opened.persisted.focus shouldBe editorFocused(opened)
      opened.runtime.focusHistory should not contain panelFocus
    finally
      Files.deleteIfExists(target)
      Files.deleteIfExists(directory)
  }

  it should "return focus to the panel when cancelled" in {
    val cancelled =
      settled(openedOverPanel(showFileWorkflow(FileWorkflowMode.Open, Paths.get("/"))), ModalDismiss)

    cancelled.topModal shouldBe None
    cancelled.persisted.focus shouldBe panelFocus
  }

  it should "hand focus to the editor and forget the panel once a directory is opened as the project root" in {
    val requested = FileWorkflowState(mode = FileWorkflowMode.Open, path = "/work")
    val shown     = ModalStateReducer.show(Modal.FileWorkflow(requested), withFocusedPanel(AppState.initial)).state
    val id        = shown.topModal.map(_.id).getOrElse(fail("no dialog"))

    val opened = FileWorkflowTransitions.withProjectRootResolved(shown, id, requested, Paths.get("/work"), true)

    opened.topModal shouldBe None
    opened.persisted.focus shouldBe editorFocused(opened)
    opened.runtime.focusHistory should not contain panelFocus
    valid(opened) shouldBe true
  }

  "A Save-As dialog opened over a focused panel" should "return focus to the panel once it saves" in {
    val directory = TestTemp.directory("workflow-focus-save-as")
    val target    = directory.resolve("notes.txt")
    try
      val saved = settled(openedOverPanel(showFileWorkflow(FileWorkflowMode.SaveAs, directory)), Enter)

      Files.exists(target) shouldBe true
      saved.topModal shouldBe None
      saved.persisted.focus shouldBe panelFocus
    finally
      Files.deleteIfExists(target)
      Files.deleteIfExists(directory)
  }

  it should "return focus to the panel when cancelled" in {
    val cancelled =
      settled(openedOverPanel(showFileWorkflow(FileWorkflowMode.SaveAs, Paths.get("/"))), ModalDismiss)

    cancelled.topModal shouldBe None
    cancelled.persisted.focus shouldBe panelFocus
  }

  private def showSessionNamePrompt(state: AppState): AppState =
    ModalStateReducer.show(Modal.TextPrompt(TextPrompt.sessionName(SessionNamePromptMode.SaveAs, "Draft")), state).state

  "A session-name prompt opened over a focused panel" should "return focus to the panel once submitted" in {
    val submitted = settled(openedOverPanel(showSessionNamePrompt), Enter)

    submitted.modalSurface shouldBe None
    submitted.persisted.focus shouldBe panelFocus
  }

  it should "return focus to the panel when cancelled" in {
    val cancelled = settled(openedOverPanel(showSessionNamePrompt), ModalDismiss)

    cancelled.modalSurface shouldBe None
    cancelled.persisted.focus shouldBe panelFocus
  }

  private val draft = SessionMetadata(SessionId("draft"), "Draft", "draft.json", 0L, 0L)

  /** A session picker opened over the focused panel, listing `draft`. */
  private def listedSessionPicker: (AppState, SurfaceId) =
    val (opened, id) = SessionWorkflowTransitions
      .withSessionPickerOpened(withFocusedPanel(AppState.initial), SessionListPurpose.Open)
      .getOrElse(fail("no picker"))
    (SessionWorkflowTransitions.withSessionsListed(opened, id, SessionListPurpose.Open, Right(List(draft))), id)

  "A session picker opened over a focused panel" should "focus the editor and forget the panel once it closes" in {
    val (listed, id) = listedSessionPicker
    val picked       = ModalEventReducer.reduce(ModalType.ListPicker, ModalSubmit, listed).state

    val closed = SessionWorkflowTransitions.withNamedSessionLoaded(picked, Some(id), None)

    closed.modalSurface shouldBe None
    closed.persisted.focus shouldBe editorFocused(closed)
    closed.runtime.focusHistory should not contain panelFocus
    valid(closed) shouldBe true
  }

  it should "return focus to the panel when cancelled" in {
    val (listed, _) = listedSessionPicker

    val cancelled = ModalEventReducer.reduce(ModalType.ListPicker, ModalDismiss, listed).state

    cancelled.modalSurface shouldBe None
    cancelled.persisted.focus shouldBe panelFocus
  }

  private val close = new CloseWorkflowTransitions(identity)

  /** Buffer 0 unsaved and shown, a second buffer behind it, and the pinned panel focused. */
  private def unsavedBufferUnderFocusedPanel: AppState =
    val creation = EditorTransitions.bufferCreated(AppState.initial, "second", None)
    val first    = creation.created.persisted.buffers(BufferId(0))
    val dirty = creation.created.copy(persisted =
      creation.created.persisted.copy(buffers =
        creation.created.persisted.buffers
          .updated(BufferId(0), first.copy(document = first.document.copy(isDirty = true)))
      )
    )
    withFocusedPanel(dirty)

  /** The close of buffer 0 begun from the focused panel, and its prompt answered (closed) as the reducer does. */
  private def closePromptAnswered: (CloseWorkflowState, AppState) =
    val begun    = close.begun(CloseScope.Current, unsavedBufferUnderFocusedPanel)
    val workflow = close.pending(begun.state).getOrElse(fail("no close pending"))
    (workflow, begun.state.dismissTopModal)

  "A close prompt begun from a focused panel" should "return focus to the panel once its buffer is discarded" in {
    val (workflow, answered) = closePromptAnswered

    val discarded = close.resolved(workflow, answered).state

    discarded.persisted.buffers should not contain key(BufferId(0))
    discarded.persisted.focus shouldBe panelFocus
    valid(discarded) shouldBe true
  }

  it should "return focus to the panel when cancelled" in {
    val (workflow, answered) = closePromptAnswered

    val abandoned = close.abandoned(workflow, answered)

    abandoned.persisted.buffers should contain key BufferId(0)
    abandoned.persisted.focus shouldBe panelFocus
    valid(abandoned) shouldBe true
  }

  it should "return focus to the panel once the Save-As its Save opened has saved" in {
    val (workflow, answered) = closePromptAnswered
    val saveAs = ModalStateReducer.show(Modal.FileWorkflow(FileWorkflowState(mode = FileWorkflowMode.SaveAs)), answered)

    val saved = close.resolvedBySaveAs(workflow, saveAs.state).state

    saved.runtime.modalStack shouldBe empty
    saved.persisted.focus shouldBe panelFocus
    valid(saved) shouldBe true
  }

  "Clearing the close chain's modals" should "hand focus back from the modal layer it emptied" in {
    val saveAs = Modal.FileWorkflow(FileWorkflowState(mode = FileWorkflowMode.SaveAs))
    val shown  = ModalStateReducer.show(saveAs, withFocusedPanel(AppState.initial))

    val cleared = close.dismissModalSurface(shown.state)

    cleared.runtime.modalStack shouldBe empty
    cleared.persisted.focus shouldBe panelFocus
    valid(cleared) shouldBe true
  }

  "A peek shown over a focused panel" should "return focus to the panel when the peek dismisses itself" in {
    val stateManager =
      openedOverPanel(state => PeekStateReducer.show(PeekContent.QuickInfo("hint"), CursorPosition(0, 0), state).state)

    val dismissed = settled(stateManager, PeekInputEvent.Dismiss)

    dismissed.peekSurface shouldBe None
    dismissed.persisted.focus shouldBe panelFocus
  }
