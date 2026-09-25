package com.serenity.session

import _root_.io.circe.parser.decode
import _root_.io.circe.syntax.*
import com.serenity.rope.Balance
import com.serenity.state.models.AppState
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #1719: the command runner's recents are part of the session file. */
class SessionCommandUsageCodecSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val recents = Map("open" -> 3, "save" -> 1, "lsp-rename" -> 2)

  private val sessionWithRecents: SessionState =
    val initial = AppState.initial
    SessionState.fromAppState(initial.copy(persisted = initial.persisted.copy(commandUsage = recents)))

  "SessionState" should "take the recents from the app state it snapshots" in {
    sessionWithRecents.commandUsage shouldBe recents
  }

  it should "round-trip the recents through its JSON codec" in {
    decode[SessionState](sessionWithRecents.asJson.noSpaces).map(_.commandUsage) shouldBe Right(recents)
  }

  it should "decode a session file written before recents were saved as having none" in {
    val legacyJson = sessionWithRecents.asJson.mapObject(_.remove("commandUsage"))

    decode[SessionState](legacyJson.noSpaces).map(_.commandUsage) shouldBe Right(Map.empty)
  }

  it should "restore the recents into the app state" in {
    SessionState.toAppState(sessionWithRecents, Theme.default).persisted.commandUsage shouldBe recents
  }
end SessionCommandUsageCodecSpec
