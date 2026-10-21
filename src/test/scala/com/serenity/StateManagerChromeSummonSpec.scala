package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.ChromeElement
import com.serenity.keystroke.events.{
  ChromeSummonLocked,
  ChromeSummonPressed,
  ChromeSummonReleased,
  ChromeSummonToggled
}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.AppState
import com.serenity.testkit.SharedDictionary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Summoned chrome through the state manager's own event pipeline, rather than the reducer alone. */
class StateManagerChromeSummonSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerChromeSummonSpec"))

  private def after(events: com.serenity.keystroke.events.Event*): AppState =
    (for
      stateManager <- StateManager(logger, dictionaryCache = SharedDictionary.default)
      _            <- events.toList.foldLeft(IO.unit)((done, event) => done >> stateManager.applyEvent(event))
      state        <- stateManager.getCurrentState
    yield state).unsafeRunSync()

  "A held chord" should "show the element, and its release hide it" in {
    after(ChromeSummonPressed(ChromeElement.LineNumbers)).chrome.lineNumbers shouldBe true
    after(
      ChromeSummonPressed(ChromeElement.LineNumbers),
      ChromeSummonReleased(ChromeElement.LineNumbers)
    ).chrome.lineNumbers shouldBe false
  }

  "A locked chord" should "stay after the release" in {
    after(
      ChromeSummonPressed(ChromeElement.StatusBar),
      ChromeSummonLocked,
      ChromeSummonReleased(ChromeElement.StatusBar)
    ).chrome.statusLine.isPinned shouldBe true
  }

  "A terminal's chord" should "toggle" in {
    after(ChromeSummonToggled(ChromeElement.Scrollbar)).chrome.scrollbar shouldBe true
    after(
      ChromeSummonToggled(ChromeElement.Scrollbar),
      ChromeSummonToggled(ChromeElement.Scrollbar)
    ).chrome.scrollbar shouldBe false
  }
