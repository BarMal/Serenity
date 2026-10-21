package com.serenity.session

import _root_.io.circe.Json
import _root_.io.circe.parser.decode
import _root_.io.circe.syntax.*
import com.serenity.config.ChromeElement
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, ChromeSummons}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Chrome the user locked is part of the session, and a chord that is only being held is not. */
class SessionLockedChromeCodecSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val summons = ChromeSummons(
    held = Set(ChromeElement.Scrollbar),
    locked = Set(ChromeElement.StatusBar, ChromeElement.LineNumbers)
  )

  private val sessionWithLocks: SessionState =
    val initial = AppState.initial
    SessionState.fromAppState(initial.copy(runtime = initial.runtime.copy(summons = summons)))

  "SessionState" should "take the locked chrome, in a stable order, and not the chrome merely held" in {
    sessionWithLocks.lockedChrome shouldBe List("line_numbers", "status_bar")
  }

  it should "round-trip the locked chrome through its JSON codec" in {
    decode[SessionState](sessionWithLocks.asJson.noSpaces).map(_.lockedChrome) shouldBe
      Right(List("line_numbers", "status_bar"))
  }

  it should "write it under lockedChrome" in {
    sessionWithLocks.asJson.hcursor.get[List[String]]("lockedChrome") shouldBe Right(List("line_numbers", "status_bar"))
  }

  it should "restore the locked chrome into the app state, with nothing held" in {
    val restored = SessionState.toAppState(sessionWithLocks, Theme.default)

    restored.runtime.summons shouldBe ChromeSummons(locked = Set(ChromeElement.LineNumbers, ChromeElement.StatusBar))
    restored.chrome.lineNumbers shouldBe true
    restored.chrome.scrollbar shouldBe false
  }

  it should "restore a session written before locked chrome as having none" in {
    val legacy = sessionWithLocks.asJson.mapObject(_.remove("lockedChrome"))

    decode[SessionState](legacy.noSpaces).map(_.lockedChrome) shouldBe Right(Nil)
    SessionState.toAppState(sessionWithLocks.copy(lockedChrome = Nil), Theme.default).runtime.summons shouldBe
      ChromeSummons.empty
  }

  it should "skip an element name this build does not know rather than refuse the session" in {
    val unknown = sessionWithLocks.copy(lockedChrome = List("status_bar", "minimap"))

    SessionState.toAppState(unknown, Theme.default).runtime.summons.locked shouldBe Set(ChromeElement.StatusBar)
  }

  it should "never write the summoned chrome into the config it saves" in {
    val config = sessionWithLocks.asJson.hcursor.downField("config").focus.getOrElse(Json.Null)

    config shouldBe AppState.initial.persisted.config.asJson
  }
