package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent, ViewIntent}
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.{PanelContent, PanelPosition, PanelTarget}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** #1016 PR4's acceptance criterion: pinning a panel is undoable, regardless of which of the two production pin/unpin
  * entry points made the change -- `StateManagerPanelEffects`'s kind-based pin/unpin (reached via `ViewIntent`/the
  * command runner) and `PanelStateReducer` via `StateManagerSurfaceCapability` (`PanelManager`, reached by
  * drag-to-pin/peek-to-pin). See `HistoryEntrySpec` for `HistoryEntry.PanelChange.restore` in isolation.
  */
class PanelPinUndoSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  trait PanelFixture:
    val sm: StateManager =
      StateManager.apply(LoggerFactory[IO].getLogger(using LoggerName("PanelPinUndoSpec"))).unsafeRunSync()

  private def viewCommand(intent: ViewIntent): Command =
    Command.typed("test-view-command", "A test view command.", CommandIntent.View(intent), CommandCategory.View)

  behavior of "Undoing a panel pin/unpin via the kind-based entry point (ViewIntent)"

  it should "restore the previous (unpinned) state after undoing a pin" in new PanelFixture:
    sm.executeCommand(viewCommand(ViewIntent.PinDiagnosticsPanel)).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().pinnedSurfaces.map(_.content) shouldBe List(SurfaceContent.Diagnostics(Nil))

    sm.applyEvent(Undo).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().pinnedSurfaces shouldBe Nil

  it should "redo back to the pinned state" in new PanelFixture:
    sm.executeCommand(viewCommand(ViewIntent.PinDiagnosticsPanel)).unsafeRunSync()
    val afterPin = sm.getCurrentState.unsafeRunSync()

    sm.applyEvent(Undo).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().pinnedSurfaces shouldBe Nil

    sm.applyEvent(Redo).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().pinnedSurfaces.map(_.content) shouldBe
      afterPin.pinnedSurfaces.map(_.content)

  it should "restore the previously pinned surface after undoing an unpin" in new PanelFixture:
    sm.executeCommand(viewCommand(ViewIntent.PinOutlinePanel)).unsafeRunSync()
    val afterPin = sm.getCurrentState.unsafeRunSync()

    sm.executeCommand(viewCommand(ViewIntent.SetPanelPin(com.serenity.command.PanelKind.Outline, None)))
      .unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().pinnedSurfaces shouldBe Nil

    sm.applyEvent(Undo).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().pinnedSurfaces.map(_.content) shouldBe afterPin.pinnedSurfaces.map(_.content)

  behavior of "Undoing a panel pin/unpin via the PanelManager entry point (drag-to-pin)"

  it should "restore the previous (unpinned) state after undoing a pin" in new PanelFixture:
    sm.panelManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().pinnedSurfaces should have size 1

    sm.applyEvent(Undo).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().pinnedSurfaces shouldBe Nil

  it should "redo back to the pinned state" in new PanelFixture:
    sm.panelManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    val afterPin = sm.getCurrentState.unsafeRunSync()

    sm.applyEvent(Undo).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().pinnedSurfaces shouldBe Nil

    sm.applyEvent(Redo).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().pinnedSurfaces.map(_.content) shouldBe
      afterPin.pinnedSurfaces.map(_.content)

  it should "restore the previously pinned surface after undoing an unpin" in new PanelFixture:
    sm.panelManager.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Bottom, 10).unsafeRunSync()
    val afterPin  = sm.getCurrentState.unsafeRunSync()
    val surfaceId = afterPin.pinnedSurfaces.head.id

    sm.panelManager.unpinPanel(PanelTarget.ById(surfaceId)).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().pinnedSurfaces shouldBe Nil

    sm.applyEvent(Undo).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().pinnedSurfaces.map(_.content) shouldBe afterPin.pinnedSurfaces.map(_.content)

  it should "interleave correctly with a buffer-edit undo performed before the pin" in new PanelFixture:
    val bufferId = sm.bufferManager.createBuffer("hello", None).unsafeRunSync()
    val pane0    = sm.getCurrentState.unsafeRunSync().persisted.layout.activeEditorPaneId.get
    sm.setBufferForPane(pane0, bufferId).unsafeRunSync()
    sm.setCursorPosition(pane0, 0, 5).unsafeRunSync()
    sm.applyEvent(InsertChar('!')).unsafeRunSync()

    sm.panelManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().pinnedSurfaces should have size 1

    // Most recent change first: the panel pin undoes before the earlier buffer edit does.
    sm.applyEvent(Undo).unsafeRunSync()
    val afterFirstUndo = sm.getCurrentState.unsafeRunSync()
    afterFirstUndo.pinnedSurfaces shouldBe Nil
    afterFirstUndo.persisted.buffers(bufferId).document.content.collect() shouldBe "hello!"

    sm.applyEvent(Undo).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).document.content.collect() shouldBe "hello"
