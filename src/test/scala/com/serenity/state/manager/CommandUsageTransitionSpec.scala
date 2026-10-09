package com.serenity.state.manager

import com.serenity.command.{CommandId, CommandRegistry, CommandUsageHistory}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, AppStateValidation}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The MRU bump every executed command makes (#1048), which used to be written without validation. */
class CommandUsageTransitionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "withCommandUsageRecorded" should "make the command the most recently used one" in {
    val once  = StateManagerEffectHandlers.withCommandUsageRecorded(AppState.initial, "save")
    val twice = StateManagerEffectHandlers.withCommandUsageRecorded(once, "open")
    val again = StateManagerEffectHandlers.withCommandUsageRecorded(twice, "save")

    again.persisted.commandUsage shouldBe Map(CommandId("save") -> 3, CommandId("open") -> 2)
    AppStateValidation.validationErrors(again) shouldBe Nil
  }

  it should "change nothing but the usage record" in {
    val recorded = StateManagerEffectHandlers.withCommandUsageRecorded(AppState.initial, "save")

    recorded.copy(persisted = recorded.persisted.copy(commandUsage = Map.empty)) shouldBe AppState.initial
  }

  // #1877: the settings surface runs option cycling as a command named by its intent's `toString`, and an input row's
  // commit as one named by the row id.
  it should "leave the table alone for a command the registry does not have" in {
    val cycledOption = "Settings(Font(SetTextFontFamily(Menlo)))"
    val cycled       = StateManagerEffectHandlers.withCommandUsageRecorded(AppState.initial, cycledOption)
    val committed    = StateManagerEffectHandlers.withCommandUsageRecorded(cycled, "code-font-size")

    committed.persisted.commandUsage shouldBe empty
  }

  it should "keep only the most recently used commands once the table is full" in {
    val names = CommandRegistry.withToggleUI.getAllCommands.map(_.name).distinct.take(CommandUsageHistory.Capacity + 1)
    names should have size (CommandUsageHistory.Capacity + 1).toLong

    val recorded = names.foldLeft(AppState.initial)(StateManagerEffectHandlers.withCommandUsageRecorded)

    recorded.persisted.commandUsage should have size CommandUsageHistory.Capacity.toLong
    recorded.persisted.commandUsage.keySet should not contain CommandId(names.head)
    recorded.persisted.commandUsage.keySet should contain(CommandId(names.last))
  }
