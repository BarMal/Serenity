package com.serenity.state.manager

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent, ViewIntent}
import com.serenity.keystroke.events.{CloseTabById, Event, InsertChar, Redo, Undo}
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.manager.StateManagerTestFacade.{createBuffer, createPane, executeCommand}
import com.serenity.state.models.*
import com.serenity.{setBufferForPane, setCursorPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.{NoOpFactory, NoOpLogger}

/** Undo and redo driven through `StateManager` the way a user drives them (#1930): two buffers side by side, each with
  * its own history.
  */
class UndoHistoryScenarioSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = NoOpFactory[IO]

  private trait TwoBuffers:
    def maxUndoDepth: Int = SessionManager.SessionPolicy().maxUndoDepth

    val stateManager: StateManager =
      StateManager(NoOpLogger[IO], policy = SessionManager.SessionPolicy(maxUndoDepth = maxUndoDepth)).unsafeRunSync()

    val paneA: PaneId     = stateManager.getCurrentState.unsafeRunSync().persisted.layout.editorPanes.keys.head
    val bufferA: BufferId = stateManager.createBuffer("alpha", None).unsafeRunSync()
    stateManager.setBufferForPane(paneA, bufferA).unsafeRunSync()
    stateManager.setCursorPosition(paneA, 0, 5).unsafeRunSync()

    val bufferB: BufferId = stateManager.createBuffer("beta", None).unsafeRunSync()
    val paneB: PaneId     = stateManager.createPane(Some(bufferB)).unsafeRunSync()
    stateManager.setCursorPosition(paneB, 0, 4).unsafeRunSync()

    def in(paneId: PaneId)(events: Event*): Unit =
      stateManager.switchToPane(paneId).unsafeRunSync()
      events.foreach(event => stateManager.applyEvent(event).unsafeRunSync())

    def typed(text: String): Seq[Event] = text.map(InsertChar(_))

    def text(bufferId: BufferId): Option[String] =
      stateManager.getCurrentState.unsafeRunSync().persisted.buffers.get(bufferId).map(_.document.content.collect())

    def focus: Focus = stateManager.getCurrentState.unsafeRunSync().persisted.focus

  private def togglePanel(panel: PanelId): Command =
    Command.typed("toggle-panel", "Toggles a panel.", CommandIntent.View(ViewIntent.TogglePanelShown(panel)), CommandCategory.View)

  behavior of "Undo across two buffers"

  it should "leave buffer A's edit alone when undoing in buffer B, which has none" in new TwoBuffers:
    in(paneA)(typed("!")*)
    in(paneB)(Undo)

    text(bufferA) shouldBe Some("alpha!")
    text(bufferB) shouldBe Some("beta")
    focus shouldBe Focus.EditorPane(paneB)

  it should "undo each buffer's own edit in whichever pane has focus" in new TwoBuffers:
    in(paneA)(typed("!")*)
    in(paneB)(typed("?")*)

    in(paneA)(Undo)
    text(bufferA) shouldBe Some("alpha")
    text(bufferB) shouldBe Some("beta?")

    in(paneB)(Undo)
    text(bufferB) shouldBe Some("beta")

  it should "redo only the focused buffer's undone edit" in new TwoBuffers:
    in(paneA)((typed("!") :+ Undo)*)
    in(paneB)(Redo)
    text(bufferA) shouldBe Some("alpha")

    in(paneA)(Redo)
    text(bufferA) shouldBe Some("alpha!")

  it should "drop a buffer's history when the buffer is closed" in new TwoBuffers:
    in(paneA)(typed("!")*)
    in(paneB)(CloseTabById(bufferA))

    stateManager.getModel.unsafeRunSync().undo.undoStack shouldBe empty

  it should "keep a buffer's edit undoable however many panel toggles follow it" in new TwoBuffers:
    override def maxUndoDepth: Int = 3

    in(paneA)(typed("!")*)
    (1 to 8).foreach(_ => stateManager.executeCommand(togglePanel(PanelId.Diagnostics)).unsafeRunSync())
    in(paneA)(Seq.fill(4)(Undo)*)

    text(bufferA) shouldBe Some("alpha")
