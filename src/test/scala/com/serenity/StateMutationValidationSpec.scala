package com.serenity

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.CommandRegistry
import com.serenity.keystroke.events.{Enter, TabKey}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.ui.layout.{WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** #858 part 2: direct `Ref[IO, AppState]` mutation paths that bypass `validateAndUpdateState` can commit a
  * structurally invalid `AppState`. These tests drive a corrupted-but-realistic precondition (a `nextBufferId` counter
  * that has drifted out of sync with the live buffers -- the same class of drift #1181 found already happening by hand
  * in test fixtures) through the public file-loading API and assert the checked commit path rejects the resulting
  * invalid state instead of silently persisting it.
  */
class StateMutationValidationSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateMutationValidationSpec"))
    StateManager.apply(logger).unsafeRunSync()

  /** Forces `nextBufferId` to collide with the buffer that `AppState.initial` already ships (`BufferId(0)`), which is
    * also the focused buffer -- the precondition under which the bypassed commit produces a duplicate `bufferOrder`
    * entry and silently overwrites the live buffer.
    */
  private def withNextBufferIdCollidingWithLiveBuffer(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(nextBufferId = BufferId(0)))

  /** A state manager over `state` with `drift` applied. Every write is validated, so a drifted state can only be seeded
    * at construction.
    */
  private def driftedStateManager(state: AppState)(drift: AppState => AppState): StateManager =
    seededStateManager(_ => drift(state)).unsafeRunSync()

  private def stateManagerWithDriftedNextBufferId(): StateManager =
    driftedStateManager(createStateManager().getCurrentState.unsafeRunSync())(withNextBufferIdCollidingWithLiveBuffer)

  "StateManager.openFile" should "not commit a duplicate buffer-order entry when nextBufferId has drifted" in {
    val stateManager = stateManagerWithDriftedNextBufferId()

    val tempFile = Files.createTempFile("state-mutation-validation", ".txt")
    try
      Files.writeString(tempFile, "loaded from disk")

      val before = stateManager.getCurrentState.unsafeRunSync()
      stateManager.fileOpener.openFile(tempFile).unsafeRunSync()
      val after = stateManager.getCurrentState.unsafeRunSync()

      after.isValid shouldBe true
      after.persisted.bufferOrder should contain theSameElementsAs after.persisted.bufferOrder.distinct
      after.persisted.buffers(BufferId(0)).document.filePath shouldBe before.persisted
        .buffers(
          BufferId(0)
        )
        .document
        .filePath
    finally Files.deleteIfExists(tempFile)
  }

  "The open-file workflow modal" should
    "not commit a duplicate buffer-order entry when nextBufferId has drifted" in {
      val validStateManager = createStateManager()

      val tempRoot   = Files.createTempDirectory("state-mutation-validation-workflow")
      val targetFile = tempRoot.resolve("notes.scala")
      Files.writeString(targetFile, "val answer = 42")

      try
        val before = validStateManager.getCurrentState.unsafeRunSync()

        // Open the modal on an untouched, valid state -- the drift is introduced only after the modal is showing,
        // mirroring the many other unchecked `Ref.update` paths elsewhere in this codebase that could plausibly
        // desync `nextBufferId` between a validated commit and this workflow's own completion.
        validStateManager.modalService
          .showModal(
            Modal.FileWorkflow(
              FileWorkflowState(mode = FileWorkflowMode.Open, filename = "notes.scala", path = tempRoot.toString)
            )
          )
          .unsafeRunSync()
        val stateManager = driftedStateManager(validStateManager.getCurrentState.unsafeRunSync())(
          withNextBufferIdCollidingWithLiveBuffer
        )
        (stateManager.applyEvent(Enter) >> stateManager.runtimeLifecycle.awaitEffects).unsafeRunSync()

        val after = stateManager.getCurrentState.unsafeRunSync()

        after.isValid shouldBe true
        after.persisted.bufferOrder should contain theSameElementsAs after.persisted.bufferOrder.distinct
        after.persisted.buffers(BufferId(0)).document.filePath shouldBe before.persisted
          .buffers(
            BufferId(0)
          )
          .document
          .filePath
      finally
        Files.deleteIfExists(targetFile)
        Files.deleteIfExists(tempRoot)
    }

  /** #1183 item 1: the close-workflow family (`beginCloseAction`, `promptCloseWorkflow`, `submitCloseWorkflowEffect`,
    * `continueCloseWorkflow`) commits every step via direct `stateRef.set`, bypassing `validateAndUpdateState`
    * entirely. A close-all/close-others/quit sequence queues each dirty buffer's id in `CloseWorkflowState
    * .remainingBufferIds`, captured once when the sequence begins; if a queued buffer is no longer live by the time
    * `continueCloseWorkflow` resumes for it (some other unchecked mutation path removed it in the meantime -- exactly
    * the class of drift this suite already proves for `nextBufferId`), `promptCloseWorkflow`'s call to
    * `EditorState.rebalancePanes`/`assignBuffersToPanes` (see `EditorState.scala`'s `case None` branch, which assigns
    * `pane.copy(bufferId = Some(focusedBufferId))` without checking the buffer still exists) commits a pane pointing at
    * a nonexistent buffer -- with no `closeFocusedTab`/`removeBuffer` call afterward to clean it up, unlike the
    * discard/save branches that immediately close the buffer they just focused.
    */
  "A close-all workflow" should
    "not commit a pane referencing a buffer removed from under a still-queued close step" in {
      val stateManager   = createStateManager()
      val secondBufferId = stateManager.bufferManager.createBuffer("second", None).unsafeRunSync()
      val closeAllCommand = CommandRegistry.default
        .findCommand("close-all")
        .getOrElse(fail("\"close-all\" command not registered in CommandRegistry.default"))

      stateManager
        .updateState { state =>
          val dirtyBuffers =
            state.persisted.buffers.view.mapValues(b => b.copy(document = b.document.copy(isDirty = true))).toMap
          state.copy(persisted = state.persisted.copy(buffers = dirtyBuffers))
        }
        .unsafeRunSync()

      stateManager.commandExecutor.executeCommand(closeAllCommand).unsafeRunSync()

      val afterFirstPrompt = stateManager.getCurrentState.unsafeRunSync()
      afterFirstPrompt.topModal.flatMap {
        _.modal match
          case Modal.CloseWorkflow(workflow) => Some(workflow)
          case _                             => None
      } match
        case Some(workflow) =>
          workflow.currentBufferId shouldBe BufferId(0)
          workflow.remainingBufferIds shouldBe List(secondBufferId)
        case None => fail("Expected a close-workflow modal for the first dirty buffer")

      // Drift: the second buffer -- still queued in `remainingBufferIds` -- disappears from `persisted.buffers` out
      // from under the in-flight close-all sequence, simulating some other unchecked mutation path having removed it.
      stateManager
        .updateState { state =>
          state.copy(persisted =
            state.persisted.copy(
              buffers = state.persisted.buffers - secondBufferId,
              bufferOrder = state.persisted.bufferOrder.filterNot(_ == secondBufferId)
            )
          )
        }
        .unsafeRunSync()

      // Discard the (still-live) first buffer, which resumes the close-all sequence onto the now-stale second one.
      stateManager.applyEvent(TabKey).unsafeRunSync()
      stateManager.applyEvent(Enter).unsafeRunSync()

      val after = stateManager.getCurrentState.unsafeRunSync()
      after.isValid shouldBe true
      AppStateValidation.validationErrors(after) shouldBe empty
      after.persisted.layout.editorPanes.values.foreach { pane =>
        pane.bufferId.foreach(bufferId => after.persisted.buffers should contain key bufferId)
      }
    }

  /** #1183's remaining audit: `StateManagerViewportCapability.clickMinimap` committed via a bare `stateRef.update`,
    * placing the cursor at the clicked minimap row with no bounds check. A minimap click is resolved against the
    * document as rendered; if the buffer's content has since shrunk (a concurrent edit/undo racing the click), the
    * clicked row can land past the buffer's current line count, producing an out-of-bounds cursor -- exactly the
    * invariant `AppStateValidation.documentPositionErrors` polices. `clickMinimap` now clamps to bounds directly and
    * commits through `validateAndUpdateState` as a second line of defense.
    */
  "Clicking the minimap" should "not place the cursor beyond the buffer's current line count" in {
    val stateManager = createStateManager()
    val paneId = stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .layout
      .activeEditorPaneId
      .getOrElse(fail("Expected an active editor pane in the initial state"))

    // Shrink the focused buffer to a single line, simulating a concurrent edit that raced the minimap click.
    stateManager
      .updateState { state =>
        state.copy(persisted =
          state.persisted.copy(buffers =
            state.persisted.buffers.view
              .mapValues(b => b.copy(document = b.document.copy(content = com.serenity.rope.Rope("one line"))))
              .toMap
          )
        )
      }
      .unsafeRunSync()

    // Click far below the buffer's single remaining line, as a stale minimap rendering (from before the shrink)
    // would still permit.
    stateManager.scrollManager.clickMinimap(paneId, 500).unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.isValid shouldBe true
    AppStateValidation.validationErrors(after) shouldBe empty
    after.persisted.buffers.values.foreach(_.editing.cursorPositions.foreach(_.line shouldBe 0))
  }

  /** #1183's remaining audit: `ViewIntent.SplitPaneHorizontal`/`SplitPaneVertical`/`ClosePane`/`NextTab`/`PreviousTab`
    * committed via bare `stateRef.update` calls in `StateManagerPanelEffects`. `WorkspaceTree.split` already refuses a
    * `newPaneId` that collides with a live pane (returning `None`), and `EditorState.splitFocusedPane` already no-ops
    * on that `None` rather than partially applying the split -- so this drifted-`nextPaneId` precondition was never a
    * reachable corruption, with or without this routing. This is a defense-in-depth regression test: it pins down that
    * a bare `stateRef.update` call site can never regress that pre-existing guarantee, now that the commit is also
    * checked by `validateAndUpdateState`.
    */
  "Splitting the focused pane" should "leave the layout unchanged when nextPaneId has drifted to collide with it" in {
    val initial = createStateManager().getCurrentState.unsafeRunSync()
    val focusedPaneId = initial.persisted.layout.activeEditorPaneId
      .getOrElse(fail("Expected an active editor pane in the initial state"))

    // Drift: nextPaneId collides with the pane that is about to be split.
    val stateManager =
      driftedStateManager(initial)(state => state.copy(runtime = state.runtime.copy(nextPaneId = focusedPaneId)))
    val before      = stateManager.getCurrentState.unsafeRunSync()
    val panesBefore = before.persisted.layout.editorPanes

    val splitCommand = CommandRegistry.default
      .findCommand("split-pane-horizontal")
      .getOrElse(fail("\"split-pane-horizontal\" command not registered in CommandRegistry.default"))
    stateManager.commandExecutor.executeCommand(splitCommand).unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.persisted.layout.editorPanes shouldBe panesBefore
    after.persisted.layout.workspaceTree shouldBe before.persisted.layout.workspaceTree
  }

  /** Audit of #1183/#1499: `createBuffer`, `createNewEmptyBuffer`, `updateBuffer`, `createPane` and `switchToPane`
    * (`StateManagerEditorCapability.scala`) still commit via bare `stateRef.modify`/`stateRef.update`, bypassing
    * `validateAndUpdateState` entirely -- unlike the close-workflow family and the viewport/panel call sites above,
    * which #1183/#1499 already routed through it. `StateManagerEventPipeline`'s dismiss-last-pane branch
    * (`createBuffer("")` then `createPane(Some(bufferId))`) calls straight through this unchecked path, so it's
    * reachable from the same "some earlier unchecked mutation left the state mid-drift" scenarios the specs above
    * already exercise.
    */
  "StateManager.bufferManager.createBuffer" should
    "not commit a duplicate buffer-order entry when nextBufferId has drifted" in {
      val stateManager = stateManagerWithDriftedNextBufferId()

      val before = stateManager.getCurrentState.unsafeRunSync()
      stateManager.bufferManager.createBuffer("fresh content", None).unsafeRunSync()
      val after = stateManager.getCurrentState.unsafeRunSync()

      after.isValid shouldBe true
      after.persisted.bufferOrder should contain theSameElementsAs after.persisted.bufferOrder.distinct
      after.persisted.buffers(BufferId(0)).document.filePath shouldBe before.persisted
        .buffers(BufferId(0))
        .document
        .filePath
    }

  "StateManager.bufferManager.createNewEmptyBuffer" should
    "not commit a duplicate buffer-order entry when nextBufferId has drifted" in {
      val stateManager = stateManagerWithDriftedNextBufferId()

      val before = stateManager.getCurrentState.unsafeRunSync()
      stateManager.bufferManager.createNewEmptyBuffer.unsafeRunSync()
      val after = stateManager.getCurrentState.unsafeRunSync()

      after.isValid shouldBe true
      after.persisted.bufferOrder should contain theSameElementsAs after.persisted.bufferOrder.distinct
      after.persisted.buffers(BufferId(0)).document.filePath shouldBe before.persisted
        .buffers(BufferId(0))
        .document
        .filePath
    }

  /** `updateBuffer` replaces `document.content` without ever re-checking the buffer's existing cursors against the new
    * line count, so a cursor left on a now-removed line survives the edit as an out-of-bounds position -- the exact
    * invariant `AppStateValidation.documentPositionErrors` exists to catch.
    */
  "StateManager.bufferManager.updateBuffer" should
    "not commit a cursor left out-of-bounds by content that shrank under it" in {
      val stateManager = createStateManager()
      val bufferId     = stateManager.bufferManager.createBuffer("line one\nline two", None).unsafeRunSync()
      stateManager
        .updateState { state =>
          val buffer = state.persisted.buffers(bufferId)
          state.copy(persisted =
            state.persisted.copy(buffers =
              state.persisted.buffers.updated(
                bufferId,
                buffer.copy(editing = EditingState(List(CursorPosition(1, 0))))
              )
            )
          )
        }
        .unsafeRunSync()
      val before = stateManager.getCurrentState.unsafeRunSync()

      stateManager.bufferManager.updateBuffer(bufferId, "only one line").unsafeRunSync()
      val after = stateManager.getCurrentState.unsafeRunSync()

      after.isValid shouldBe true
      AppStateValidation.validationErrors(after) shouldBe empty
      after.persisted.buffers(bufferId).editing.cursors shouldBe before.persisted.buffers(bufferId).editing.cursors
    }

  /** `createPane` builds an `EditorPane.withBuffer(paneId, id)` from whatever `BufferId` its caller passes, with no
    * check that the buffer actually exists -- directly violating the "Pane references non-existent buffer" invariant
    * `AppStateValidation` enforces, and reachable with no drifted precondition at all.
    */
  "StateManager.paneManager.createPane" should
    "not commit a pane referencing a buffer that was never created" in {
      val stateManager  = createStateManager()
      val phantomBuffer = BufferId(9999)

      val before = stateManager.getCurrentState.unsafeRunSync()
      stateManager.paneManager.createPane(Some(phantomBuffer)).unsafeRunSync()
      val after = stateManager.getCurrentState.unsafeRunSync()

      after.isValid shouldBe true
      AppStateValidation.validationErrors(after) shouldBe empty
      after.persisted.layout.editorPanes.values.foreach { pane =>
        pane.bufferId.foreach(bufferId => after.persisted.buffers should contain key bufferId)
      }
      after.persisted.layout.editorPanes.keySet shouldBe before.persisted.layout.editorPanes.keySet
    }

  /** `switchToPane` only checks that `paneId` is a live key in `persisted.layout.editorPanes` before pointing focus at
    * it -- it never checks that the pane is still reachable from the workspace tree, so a pane that has drifted out of
    * the tree (the same class of drift the close-all spec above already proves happens) is still accepted, committing a
    * `Focus.EditorPane` that `AppStateValidation` flags as "outside workspace tree".
    */
  "StateManager.paneManager.switchToPane" should
    "not move focus onto a pane that has drifted out of the workspace tree" in {
      val validStateManager = createStateManager()
      val secondBuffer      = validStateManager.bufferManager.createBuffer("second", None).unsafeRunSync()
      val secondPane        = validStateManager.paneManager.createPane(Some(secondBuffer)).unsafeRunSync()
      validStateManager.paneManager.switchToPane(PaneId(0)).unsafeRunSync()

      // Drift: `secondPane` is dropped from the workspace tree but left dangling in `editorPanes`, simulating some
      // other unchecked mutation path having desynced the two (mirroring the close-all spec's buffer-order drift).
      val stateManager = driftedStateManager(validStateManager.getCurrentState.unsafeRunSync()) { state =>
        state.copy(persisted =
          state.persisted.copy(layout =
            state.persisted.layout
              .copy(workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), PaneId(0)))))
          )
        )
      }
      val before = stateManager.getCurrentState.unsafeRunSync()
      AppStateValidation.validationErrors(before) should not be empty

      stateManager.paneManager.switchToPane(secondPane).unsafeRunSync()
      val after = stateManager.getCurrentState.unsafeRunSync()

      after.persisted.focus shouldBe before.persisted.focus
      after.persisted.layout.activeEditorPaneId shouldBe before.persisted.layout.activeEditorPaneId
      AppStateValidation.validationErrors(after) shouldBe AppStateValidation.validationErrors(before)
    }

  /** #1183 item 1: `StateUpdater.updateState` is reachable from outside `state.manager` (`AppStartup`, `AppRuntime`,
    * `ClipboardEventSync`) with no call into `validateAndUpdateState` at all, so an external caller can commit a
    * structurally invalid `AppState` outright. `updateStateValidated` is the checked sibling those call sites now use
    * -- mirroring `EffectEditorPort`'s existing `updateState`/`validateAndUpdateState` pair inside `state.manager` --
    * and this pins down that it rejects an update that would leave the committed state invalid, falling back to the
    * state before the call instead of committing the corruption.
    */
  "StateManager.updateStateValidated" should "reject an update that collides nextBufferId with a live buffer" in {
    val stateManager = createStateManager()
    val before       = stateManager.getCurrentState.unsafeRunSync()

    stateManager
      .updateStateValidated(state => state.copy(runtime = state.runtime.copy(nextBufferId = BufferId(0))))
      .unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after shouldBe before
    AppStateValidation.validationErrors(after) shouldBe empty
  }

  it should "commit an update that leaves the state valid" in {
    val stateManager = createStateManager()

    stateManager
      .updateStateValidated(state => state.copy(runtime = state.runtime.copy(clipboard = Some("copied text"))))
      .unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.runtime.clipboard shouldBe Some("copied text")
    AppStateValidation.validationErrors(after) shouldBe empty
  }

  /** #1183 item 2: `switchFocus` (`StateManagerEditorCapability.scala`) committed the new focus via a bare
    * `stateRef.update`, bypassing the focus-target-exists check `AppStateValidation` otherwise enforces for every other
    * commit path -- so a focus switch onto a pane or surface that doesn't exist was silently applied.
    */
  "StateManager.focusManager.switchFocus" should "not move focus onto a pane that does not exist" in {
    val stateManager = createStateManager()
    val before       = stateManager.getCurrentState.unsafeRunSync()

    stateManager.focusManager.switchFocus(Focus.EditorPane(PaneId(9999))).unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.persisted.focus shouldBe before.persisted.focus
    AppStateValidation.validationErrors(after) shouldBe empty
  }

  it should "not move focus onto a surface that does not exist" in {
    val stateManager = createStateManager()
    val before       = stateManager.getCurrentState.unsafeRunSync()

    stateManager.focusManager.switchFocus(Focus.Surface(SurfaceId("nonexistent-surface"))).unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.persisted.focus shouldBe before.persisted.focus
    AppStateValidation.validationErrors(after) shouldBe empty
  }

  it should "move focus onto a pane that does exist" in {
    val stateManager = createStateManager()
    val secondBuffer = stateManager.bufferManager.createBuffer("second", None).unsafeRunSync()
    val secondPane   = stateManager.paneManager.createPane(Some(secondBuffer)).unsafeRunSync()

    stateManager.focusManager.switchFocus(Focus.EditorPane(secondPane)).unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.persisted.focus shouldBe Focus.EditorPane(secondPane)
  }
