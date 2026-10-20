package com.serenity.state.reducers

import com.serenity.command.CommandRegistry
import com.serenity.config.{AppConfig, ChromeElement}
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChromeSummonEventSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry = CommandRegistry.withToggleUI

  private def apply(state: AppState, events: GlobalAppEvent*): AppState =
    events.foldLeft(state)((current, event) => AppEventReducer.reduce(event, current, registry).state)

  private val start = AppState.initial(AppConfig.default)

  "A summon press" should "show the element until the release" in {
    val held = apply(start, ChromeSummonPressed(ChromeElement.LineNumbers))
    held.chrome.lineNumbers shouldBe true

    apply(held, ChromeSummonReleased(ChromeElement.LineNumbers)).chrome.lineNumbers shouldBe false
  }

  "A lock during the hold" should "keep the element after the release" in {
    val state = apply(
      start,
      ChromeSummonPressed(ChromeElement.StatusBar),
      ChromeSummonLocked,
      ChromeSummonReleased(ChromeElement.StatusBar)
    )

    state.chrome.statusLine.isPinned shouldBe true
    state.runtime.summons.locked shouldBe Set(ChromeElement.StatusBar)
  }

  "A second press on a locked element" should "dismiss it, and the release leaves it hidden" in {
    val locked = apply(
      start,
      ChromeSummonPressed(ChromeElement.Scrollbar),
      ChromeSummonLocked,
      ChromeSummonReleased(ChromeElement.Scrollbar)
    )
    val dismissed = apply(locked, ChromeSummonPressed(ChromeElement.Scrollbar))

    dismissed.chrome.scrollbar shouldBe false
    apply(dismissed, ChromeSummonReleased(ChromeElement.Scrollbar)).chrome.scrollbar shouldBe false
  }

  "A terminal's summon toggle" should "show and lock, then dismiss" in {
    val shown = apply(start, ChromeSummonToggled(ChromeElement.PaneHeaders))
    shown.chrome.paneHeaders shouldBe true
    shown.runtime.summons.locked shouldBe Set(ChromeElement.PaneHeaders)

    apply(shown, ChromeSummonToggled(ChromeElement.PaneHeaders)).chrome.paneHeaders shouldBe false
  }

  "A summon event" should "leave the config as it was" in {
    apply(start, ChromeSummonToggled(ChromeElement.LineNumbers), ChromeSummonLocked).persisted.config shouldBe
      start.persisted.config
  }

  it should "request no effect" in {
    AppEventReducer.reduce(ChromeSummonPressed(ChromeElement.TabStrip), start, registry).effects shouldBe Nil
  }
