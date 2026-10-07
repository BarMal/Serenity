package com.serenity.command.menu

import com.serenity.command.{CommandAvailability, CommandRegistry}
import com.serenity.config.{AppConfig, AppMode, StatusLinePlacement}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId, PaneId}
import com.serenity.state.undo.{BufferSnapshot, HistoryEntry, UndoState}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MenuItemStateSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry = CommandRegistry.withToggleUI
  private val app      = AppState.initial(AppConfig.default.withAppMode(AppMode.Code))

  private def command(id: String) = registry.findCommand(id).getOrElse(fail(s"$id is not registered"))

  private def stateOf(id: String, in: AppState = app, undo: UndoState = UndoState()): MenuItemState =
    MenuItemState.of(command(id), in, undo.canUndo, undo.canRedo)

  private val edit =
    val buffer = app.activeBuffer.getOrElse(fail("the initial state has no buffer"))
    HistoryEntry.BufferEdit(BufferId(0), PaneId(0), BufferSnapshot.fromBuffer(buffer))

  "Undo and redo" should "be disabled with nothing to undo or redo" in {
    stateOf("undo").enabled shouldBe false
    stateOf("undo").disabledReason should not be empty
    stateOf("redo").enabled shouldBe false
  }

  it should "be enabled once the history holds an entry to undo, or a pending edit" in {
    stateOf("undo", undo = UndoState().pushUndo(edit)).enabled shouldBe true
    stateOf("undo", undo = UndoState(pendingGroup = Some(edit))).enabled shouldBe true
    stateOf("redo", undo = UndoState().pushUndo(edit)).enabled shouldBe false
    stateOf("redo", undo = UndoState(redoStack = Vector(edit))).enabled shouldBe true
  }

  "Close, close pane and save" should "need an active buffer" in {
    val nothingOpen = AppState.empty(AppConfig.default.withAppMode(AppMode.Code))

    for id <- List("close", "close-pane", "save") do
      stateOf(id, nothingOpen).enabled shouldBe false
      stateOf(id).enabled shouldBe true
  }

  "A disabled reason" should "be the one the palette gives" in {
    val prose   = AppState.initial(AppConfig.default.withAppMode(AppMode.Prose))
    val darling = command("cut-to-darlings")
    val reason  = CommandAvailability.of(darling, prose.commandRunnerContext).disabledReason
    val state   = MenuItemState.of(darling, prose, canUndo = true, canRedo = true)

    reason should not be empty
    state.disabledReason shouldBe reason
    state.enabled shouldBe false
  }

  "A toggle's checked state" should "follow the configuration" in {
    def checked(id: String, config: AppConfig) = stateOf(id, AppState.initial(config)).checked

    checked("toggle-line-numbers", AppConfig.default.withLineNumbers(true)) shouldBe Some(true)
    checked("toggle-line-numbers", AppConfig.default.withLineNumbers(false)) shouldBe Some(false)
    checked("toggle-line-wrap", AppConfig.default.withWordWrap(false)) shouldBe Some(false)
    checked("toggle-status-line", AppConfig.default.withStatusLinePlacement(StatusLinePlacement.Off)) shouldBe
      Some(false)
    checked("toggle-status-line", AppConfig.default.withStatusLinePlacement(StatusLinePlacement.Pinned)) shouldBe
      Some(true)
  }

  it should "be absent for a command that is not a toggle" in {
    stateOf("save").checked shouldBe None
  }
