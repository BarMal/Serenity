package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.command.{CommandRunner, RecordingState, SettingsPage, SettingsSurfaceState}
import com.serenity.config.{AppConfig, InterfaceDensity}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, KeyboardFidelityTier}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, SurfaceContent, SurfaceId, UiSurface}
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
    val beforeClick = driver.state.unsafeRunSync().persisted.config.surfaceConfig.showLineNumbers
    driver.dispatch(MouseClick(target.x + 1, target.y)).unsafeRunSync()
    driver.state.unsafeRunSync().persisted.config.surfaceConfig.showLineNumbers shouldBe !beforeClick
    searched.evidence.layoutViolations shouldBe empty
    driver.dispatch(Escape).unsafeRunSync()
    driver.advanceToSettled().unsafeRunSync() shouldBe true
    driver.renderFrame("closed").unsafeRunSync().evidence.surfaceRects shouldBe empty
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    driver.renderFrame("reopened").unsafeRunSync().evidence.surfaceRects should not be empty
  }

  it should "navigate nested settings, edit a decimal, and preserve configured row spacing" in {
    val config = AppConfig.default.withCommandRunnerItemGapRows(1)
    val driver = UiScenarioDriver.create("command-runner-settings", initialConfig = config).unsafeRunSync()
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    "blur radius".foreach(char => driver.dispatch(InsertChar(char)).unsafeRunSync())
    driver.dispatch(Enter).unsafeRunSync()
    driver.dispatch(InsertChar('0')).unsafeRunSync()
    driver.dispatch(InsertChar('.')).unsafeRunSync()
    driver.dispatch(InsertChar('5')).unsafeRunSync()
    val editing = runnerFrom(driver.state.unsafeRunSync())
    editing.activeSettingsSurface.map(_.current.draftText) shouldBe Some("0.5")

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
    driver.state.unsafeRunSync().persisted.config.surfaceConfig.blurRadius shouldBe 0.5f
  }

  it should "render two-row command targets in comfortable and spacious density" in
    List(InterfaceDensity.Comfortable, InterfaceDensity.Spacious).foreach { density =>
      val driver = UiScenarioDriver
        .create(s"command-runner-target-$density", initialConfig = AppConfig.default.withInterfaceDensity(density))
        .unsafeRunSync()

      driver.dispatch(ToggleCommandRunner).unsafeRunSync()
      val frame     = driver.renderFrame(s"command-runner-target-$density").unsafeRunSync()
      val surfaceId = frame.evidence.surfaceRects.keys.headOption.getOrElse(fail("Expected command runner"))
      val target = frame.evidence.itemRects
        .getOrElse(surfaceId, Nil)
        .headOption
        .getOrElse(fail("Expected command target"))

      frame.evidence.itemRects.getOrElse(surfaceId, Nil).foreach(_.height should be >= 2)
      driver.dispatch(MouseMove(target.x + 1, target.y + 1)).unsafeRunSync()
      runnerFrom(driver.state.unsafeRunSync()).selectedIndex shouldBe 0
    }

  it should "open the dedicated settings surface, search and persist an edit, then dismiss one level at a time" in {
    val driver = UiScenarioDriver.create("dedicated-settings-surface").unsafeRunSync()
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    "open settings".foreach(char => driver.dispatch(InsertChar(char)).unsafeRunSync())
    driver.dispatch(Enter).unsafeRunSync()

    val opened = runnerFrom(driver.state.unsafeRunSync())
    opened.isSettingsSurface shouldBe true
    driver.state.unsafeRunSync().runtime.uiSurfaces should have size 1

    "blur radius".foreach(char => driver.dispatch(InsertChar(char)).unsafeRunSync())
    driver.dispatch(Enter).unsafeRunSync()
    "0.5".foreach(char => driver.dispatch(InsertChar(char)).unsafeRunSync())
    driver.dispatch(Enter).unsafeRunSync()

    driver.state.unsafeRunSync().persisted.config.surfaceConfig.blurRadius shouldBe 0.5f

    // issue #1059: Escape now pops one settings level at a time here too, matching the settings-tab-in-palette path
    // -- the dedicated Settings surface previously fully closed on a single Escape regardless of depth, which was
    // the bug. Dismissing entirely now takes one Escape per remaining page, so dispatch until it actually closes.
    dismissUntilClosed(driver)

    driver.advanceToSettled().unsafeRunSync() shouldBe true
    driver.state.unsafeRunSync().commandRunnerSurface shouldBe None
  }

  it should
    "paint the tier-fidelity warning footer after recording a bare-modifier chord on a ModifyOtherKeys-tier TUI session" in {
      val driver = UiScenarioDriver.create("keymap-tier-warning").unsafeRunSync()
      driver
        .updateState { state =>
          val activated = CommandRunner.empty
            .activate(
              com.serenity.command.CommandRegistry.default,
              state.persisted.config,
              isTuiMode = true,
              keyboardFidelityTier = KeyboardFidelityTier.ModifyOtherKeys
            )
            .openSettings
          val runner = activated.copy(
            activeSettingsSurface = Some(
              SettingsSurfaceState(
                SettingsPage.Editing(
                  groupId = "settings-keymap",
                  itemId = "keymap-global-find",
                  draftText = "",
                  recording = Some(RecordingState("keymap-global-find"))
                )
              )
            )
          )
          // A dedicated settings surface (`isSettingsSurface`) renders its submenu view directly on the one
          // `CommandPalette` surface -- there is no second floating surface at all anymore (issue #1059).
          val surface = UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            com.serenity.state.models.SurfacePresentation.Floating(
              state.activeCursorPosition,
              com.serenity.state.models.SurfacePlacement.BelowCursor
            )
          )
          state.copy(
            persisted = state.persisted.copy(focus = com.serenity.state.models.Focus.Surface(surface.id)),
            runtime = state.runtime.copy(
              uiSurfaces = List(surface),
              isTuiMode = true,
              keyboardFidelityTier = KeyboardFidelityTier.ModifyOtherKeys
            )
          )
        }
        .unsafeRunSync()

      // Alt, not Ctrl: `ctrl+ctrl` is already the default global binding for ToggleCommandRunner, so recording it here
      // would route through the (unrelated) global-hotkey-conflict flow instead of exercising the tier warning.
      driver.dispatch(RunnerRecordBinding(KeyStrokeInfo(InputKey.Alt, None, Set.empty), 1_000L)).unsafeRunSync()
      driver.dispatch(RunnerRecordBinding(KeyStrokeInfo(InputKey.Alt, None, Set.empty), 1_100L)).unsafeRunSync()

      runnerFrom(driver.state.unsafeRunSync()).statusMessage shouldBe Some(
        "\"alt+alt\" recorded, but won't fire -- this terminal can't send a bare-modifier key event " +
          "at its negotiated keyboard protocol tier"
      )
      val frame = driver.renderFrame("keymap-tier-warning").unsafeRunSync()
      frame.evidence.drawnText.map(_.text).mkString(" ") should include("won't fire")
    }

  @scala.annotation.tailrec
  private def dismissUntilClosed(driver: UiScenarioDriver): Unit =
    if driver.state.unsafeRunSync().commandRunnerSurface.isDefined then
      driver.dispatch(Escape).unsafeRunSync()
      dismissUntilClosed(driver)

  private def runnerFrom(state: AppState): com.serenity.command.CommandRunner =
    state.commandRunnerSurface
      .getOrElse(fail("Expected command runner"))
      .content match
      case SurfaceContent.CommandPalette(runner) => runner
      case _                                     => fail("Expected command runner content")
