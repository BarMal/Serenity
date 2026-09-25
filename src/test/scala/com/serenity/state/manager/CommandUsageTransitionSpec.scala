package com.serenity.state.manager

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

    again.persisted.commandUsage shouldBe Map("save" -> 3, "open" -> 2)
    AppStateValidation.validationErrors(again) shouldBe Nil
  }

  it should "change nothing but the usage record" in {
    val recorded = StateManagerEffectHandlers.withCommandUsageRecorded(AppState.initial, "save")

    recorded.copy(persisted = recorded.persisted.copy(commandUsage = Map.empty)) shouldBe AppState.initial
  }
