package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ContextualToolbarUiScenarioSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  "Contextual toolbar UI scenario" should "summon, follow the caret, and return focus on close" in {
    val driver = UiScenarioDriver
      .create("contextual-toolbar", UiScenarioEnvironment(viewport = com.serenity.ui.layout.ViewportSize(60, 18)))
      .unsafeRunSync()
    driver.dispatch(ToggleContextualToolbar).unsafeRunSync()
    val opened    = driver.renderFrame("opened").unsafeRunSync()
    val surfaceId = opened.evidence.surfaceRects.keys.headOption.getOrElse(fail("Expected toolbar rectangle"))
    val initial   = opened.evidence.surfaceRects(surfaceId)
    initial.width should be < driver.environment.viewport.width
    opened.evidence.focus shouldBe Focus.EditorPane(PaneId(0))

    driver.stateManager.setCursorPosition(PaneId(0), 0, 1).unsafeRunSync()
    driver.dispatch(InsertChar('x')).unsafeRunSync()
    driver.dispatch(ToggleContextualToolbar).unsafeRunSync()
    val closed = driver.renderFrame("closed").unsafeRunSync()

    closed.evidence.surfaceRects shouldBe empty
    closed.evidence.focus shouldBe Focus.EditorPane(PaneId(0))
  }
