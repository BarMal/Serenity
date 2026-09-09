package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** #1016's other acceptance criterion alongside `UndoRedoSpec`: closing a pane is undoable, exercised end to end
  * through `StateManager` -- not just `HistoryEntry.PaneClose.restore` in isolation (`HistoryEntrySpec`).
  */
class PaneCloseUndoSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  trait PaneFixture:
    val sm: StateManager =
      StateManager.apply(LoggerFactory[IO].getLogger(using LoggerName("PaneCloseUndoSpec"))).unsafeRunSync()
    val pane0: PaneId = sm.getCurrentState.unsafeRunSync().persisted.layout.activeEditorPaneId.get

  behavior of "Undoing a pane close"

  it should "restore the closed pane, its buffer assignment, and the tree topology" in new PaneFixture:
    val pane1 = sm.paneManager.createPane(None).unsafeRunSync()
    sm.paneManager.switchToPane(pane1).unsafeRunSync()
    val beforeClose = sm.getCurrentState.unsafeRunSync()

    sm.applyEvent(ClosePane).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().persisted.layout.editorPanes.keySet shouldBe Set(pane0)

    sm.applyEvent(Undo).unsafeRunSync()

    val restored = sm.getCurrentState.unsafeRunSync()
    restored.persisted.layout.editorPanes.keySet shouldBe Set(pane0, pane1)
    restored.persisted.layout shouldBe beforeClose.persisted.layout
    restored.persisted.focus shouldBe beforeClose.persisted.focus

  it should "redo back to the closed state" in new PaneFixture:
    val pane1 = sm.paneManager.createPane(None).unsafeRunSync()
    sm.paneManager.switchToPane(pane1).unsafeRunSync()

    sm.applyEvent(ClosePane).unsafeRunSync()
    val afterClose = sm.getCurrentState.unsafeRunSync()

    sm.applyEvent(Undo).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().persisted.layout.editorPanes.keySet shouldBe Set(pane0, pane1)

    sm.applyEvent(Redo).unsafeRunSync()
    val redone = sm.getCurrentState.unsafeRunSync()
    redone.persisted.layout.editorPanes.keySet shouldBe Set(pane0)
    redone.persisted.layout shouldBe afterClose.persisted.layout

  it should "interleave correctly with a buffer-edit undo performed before the close" in new PaneFixture:
    val bufferId = sm.bufferManager.createBuffer("hello", None).unsafeRunSync()
    sm.setBufferForPane(pane0, bufferId).unsafeRunSync()
    sm.setCursorPosition(pane0, 0, 5).unsafeRunSync()
    sm.applyEvent(InsertChar('!')).unsafeRunSync()

    val pane1 = sm.paneManager.createPane(None).unsafeRunSync()
    sm.paneManager.switchToPane(pane1).unsafeRunSync()
    sm.applyEvent(ClosePane).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().persisted.layout.editorPanes.keySet shouldBe Set(pane0)

    // Most recent change first: the pane-close undoes before the earlier buffer edit does.
    sm.applyEvent(Undo).unsafeRunSync()
    val afterFirstUndo = sm.getCurrentState.unsafeRunSync()
    afterFirstUndo.persisted.layout.editorPanes.keySet shouldBe Set(pane0, pane1)
    afterFirstUndo.persisted.buffers(bufferId).document.content.collect() shouldBe "hello!"

    sm.applyEvent(Undo).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).document.content.collect() shouldBe "hello"

  it should "undo detaching the last pane's buffer, when there is only one pane to close" in new PaneFixture:
    val bufferId = sm.bufferManager.createBuffer("solo", None).unsafeRunSync()
    sm.setBufferForPane(pane0, bufferId).unsafeRunSync()

    sm.applyEvent(ClosePane).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().persisted.layout.editorPanes(pane0).bufferId shouldBe None

    sm.applyEvent(Undo).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().persisted.layout.editorPanes(pane0).bufferId shouldBe Some(bufferId)
