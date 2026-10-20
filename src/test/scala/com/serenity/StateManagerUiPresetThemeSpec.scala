package com.serenity

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.keystroke.events.ToggleCommandRunner
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.presets.{UiPreset, UiPresetStore}
import com.serenity.ui.theme.Theme
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Giving a custom preset a theme, or taking it away, through the preset commands and the store behind them. */
class StateManagerUiPresetThemeSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val root: Path = TestTemp.directory("state-manager-ui-preset-theme")

  override protected def afterAll(): Unit =
    try
      val tree = Files.walk(root)
      try tree.iterator().asScala.toList.reverse.foreach(Files.deleteIfExists(_))
      finally tree.close()
    finally super.afterAll()

  private val chosenTheme = Theme.light.name

  private class Rig(name: String):
    val path: Path = Files.createDirectories(root.resolve(name)).resolve("ui-presets.json")
    val store      = UiPresetStore(path)

    val sm: StateManager = StateManager
      .apply(
        LoggerFactory[IO].getLogger(using LoggerName("StateManagerUiPresetThemeSpec")),
        uiPresetStore = store,
        sessionRootOverride = Some(Files.createDirectories(root.resolve(s"$name-session"))),
        dictionaryCache = SharedDictionary.default
      )
      .unsafeRunSync()

    (sm.applyEvent(ToggleCommandRunner) >> sm.runtimeLifecycle.awaitEffects).unsafeRunSync()

    def run(intent: UiPresetsIntent): Unit =
      sm.executeCommand(
        Command.typed("preset-theme", "Preset theme", CommandIntent.UiPresets(intent), CommandCategory.Settings)
      ).unsafeRunSync()

    def saved(presetName: String): Option[UiPreset] = store.find(presetName).unsafeRunSync()

    def status: Option[String] =
      sm.getCurrentState
        .map(_.commandRunnerSurface.flatMap(_.content match
          case SurfaceContent.CommandPalette(runner) => runner.statusMessage
          case _                                     => None))
        .unsafeRunSync()

    def currentThemeName: String = sm.getCurrentState.map(_.persisted.theme.name).unsafeRunSync()

    def withThemelessCopy(copyName: String): Unit =
      run(UiPresetsIntent.DuplicateUiPreset("Writing", copyName))
      saved(copyName).map(_.themeName) shouldBe Some(None)

  "Setting a preset theme" should "name the chosen theme on a custom preset and keep it across a reload" in {
    val rig = Rig("set")
    rig.withThemelessCopy("Mine")
    val before = rig.saved("Mine").getOrElse(fail("Mine should exist"))

    rig.run(UiPresetsIntent.SetUiPresetTheme("Mine", chosenTheme))

    rig.saved("Mine") shouldBe Some(before.copy(themeName = Some(chosenTheme)))
    UiPresetStore(rig.path).find("Mine").unsafeRunSync().flatMap(_.themeName) shouldBe Some(chosenTheme)
    rig.status shouldBe Some(s"Preset theme set to $chosenTheme. Configure Mine.")
  }

  it should "replace a theme the preset already names" in {
    val rig = Rig("replace")
    rig.withThemelessCopy("Mine")
    rig.run(UiPresetsIntent.SetUiPresetTheme("Mine", Theme.dark.name))
    rig.run(UiPresetsIntent.SetUiPresetTheme("Mine", chosenTheme))

    rig.saved("Mine").flatMap(_.themeName) shouldBe Some(chosenTheme)
  }

  it should "refuse a theme that cannot be loaded and leave the preset alone" in {
    val rig = Rig("unloadable")
    rig.withThemelessCopy("Mine")
    val before = rig.saved("Mine")

    rig.run(UiPresetsIntent.SetUiPresetTheme("Mine", "no-such-theme-anywhere"))

    rig.saved("Mine") shouldBe before
    rig.status.getOrElse(fail("the refusal should be visible")) should include(
      "Theme 'no-such-theme-anywhere' could not be loaded"
    )
  }

  it should "say so when the custom preset does not exist" in {
    val rig = Rig("missing")

    rig.run(UiPresetsIntent.SetUiPresetTheme("Nowhere", chosenTheme))

    rig.saved("Nowhere") shouldBe None
    rig.status.getOrElse(fail("the refusal should be visible")) should include("'Nowhere' was not found")
  }

  "Using the current theme" should "name the theme in use on the preset" in {
    val rig = Rig("current")
    rig.withThemelessCopy("Mine")

    rig.run(UiPresetsIntent.UseCurrentThemeForUiPreset("Mine"))

    rig.saved("Mine").flatMap(_.themeName) shouldBe Some(rig.currentThemeName)
    rig.status shouldBe Some(s"Preset theme set to ${rig.currentThemeName}. Configure Mine.")
  }

  "Clearing a preset theme" should "leave the preset naming no theme, and write it without one" in {
    val rig = Rig("clear")
    rig.withThemelessCopy("Mine")
    rig.run(UiPresetsIntent.SetUiPresetTheme("Mine", chosenTheme))

    rig.run(UiPresetsIntent.ClearUiPresetTheme("Mine"))

    rig.saved("Mine").map(_.themeName) shouldBe Some(None)
    Files.readString(rig.path) should not include "themeName"
    rig.status shouldBe Some("Preset theme cleared. Configure Mine.")
  }

  "A built-in preset" should "stay themeless and read-only under every theme command" in {
    val rig = Rig("built-in")

    rig.run(UiPresetsIntent.SetUiPresetTheme("Writing", chosenTheme))
    rig.status shouldBe Some("Built-in preset themes cannot be changed. Duplicate Writing first.")
    rig.run(UiPresetsIntent.UseCurrentThemeForUiPreset("Writing"))
    rig.status shouldBe Some("Built-in preset themes cannot be changed. Duplicate Writing first.")
    rig.run(UiPresetsIntent.ClearUiPresetTheme("Writing"))
    rig.status shouldBe Some("Built-in preset themes cannot be changed. Duplicate Writing first.")

    rig.saved("Writing") shouldBe None
    UiPreset.builtIn("Writing").map(_.themeName) shouldBe Some(None)
  }
