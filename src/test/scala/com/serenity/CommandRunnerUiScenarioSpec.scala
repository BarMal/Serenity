package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, SurfaceContent}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerUiScenarioSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  "Command runner UI scenario" should "search, expose matching click targets, and close and reopen" in {
    val driver = UiScenarioDriver.create("command-runner").unsafeRunSync()
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    "toggle-line".foreach(char => driver.dispatch(InsertChar(char)).unsafeRunSync())
    val searched  = driver.renderFrame("searched").unsafeRunSync()
    val surfaceId = searched.evidence.surfaceRects.keys.headOption.getOrElse(fail("Expected command runner"))
    val target =
      searched.evidence.itemRects.getOrElse(surfaceId, Nil).headOption.getOrElse(fail("Expected item target"))
    val drawnItem = searched.evidence.drawnItems
      .getOrElse(surfaceId, Nil)
      .find(_.hitTarget == target)
      .getOrElse(fail("Expected drawn bounds for command item"))

    drawnItem.textBounds.map(_.text).mkString(" ") should include("Toggle Line")
    drawnItem.textBounds.foreach(text => target.containsRect(text.bounds) shouldBe true)

    driver.dispatch(MouseMove(target.x + 1, target.y)).unsafeRunSync()
    val hovered = driver.state.unsafeRunSync().commandRunnerSurface.getOrElse(fail("Expected command runner"))
    val selectedIndex = hovered.content match
      case com.serenity.state.models.SurfaceContent.CommandPalette(runner) => runner.selectedIndex
      case _                                                               => fail("Expected command palette")
    selectedIndex shouldBe 0
    val beforeClick = driver.state.unsafeRunSync().config.showLineNumbers
    driver.dispatch(MouseClick(target.x + 1, target.y)).unsafeRunSync()
    driver.state.unsafeRunSync().config.showLineNumbers shouldBe !beforeClick
    searched.evidence.layoutViolations shouldBe empty
    driver.dispatch(Escape).unsafeRunSync()
    driver.advanceToSettled().unsafeRunSync() shouldBe true
    driver.renderFrame("closed").unsafeRunSync().evidence.surfaceRects shouldBe empty
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    driver.renderFrame("reopened").unsafeRunSync().evidence.surfaceRects should not be empty
  }

  it should "navigate nested settings, edit a decimal, and preserve configured row spacing" in {
    val config = com.serenity.config.AppConfig.default.withCommandRunnerItemGapRows(1)
    val driver = UiScenarioDriver.create("command-runner-settings", initialConfig = config).unsafeRunSync()
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    "blur radius".foreach(char => driver.dispatch(InsertChar(char)).unsafeRunSync())
    driver.dispatch(Enter).unsafeRunSync()
    driver.dispatch(InsertChar('0')).unsafeRunSync()
    driver.dispatch(InsertChar('.')).unsafeRunSync()
    driver.dispatch(InsertChar('5')).unsafeRunSync()
    val editing = runnerFrom(driver.state.unsafeRunSync())
    editing.activeSubmenu.map(_.editingText) shouldBe Some("0.5")

    val frame     = driver.renderFrame("nested-decimal").unsafeRunSync()
    val surfaceId = frame.evidence.surfaceRects.keys.head
    val rows      = frame.evidence.itemRects(surfaceId)
    rows.sliding(2).foreach {
      case List(first, second) => second.y - first.y should be >= 2
      case _                   => ()
    }
    frame.evidence.drawnItems(surfaceId).foreach { item =>
      item.textBounds.foreach(text => item.hitTarget.containsRect(text.bounds) shouldBe true)
    }
    frame.evidence.visibleText.mkString(" ") should not include "/home/"
    driver.dispatch(Enter).unsafeRunSync()
    driver.state.unsafeRunSync().config.blurRadius shouldBe 0.5f
  }

  private def runnerFrom(state: AppState): com.serenity.command.CommandRunner =
    state.commandRunnerSurface
      .orElse(state.commandRunnerSubmenuSurface)
      .getOrElse(fail("Expected command runner"))
      .content match
      case SurfaceContent.CommandPalette(runner)              => runner
      case SurfaceContent.CommandPaletteSubmenu(runner, _, _) => runner
      case _                                                  => fail("Expected command runner content")
