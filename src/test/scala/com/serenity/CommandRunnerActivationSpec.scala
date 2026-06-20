package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.*
import com.serenity.rope.Balance
import com.serenity.state.models.SurfaceContent
import com.serenity.ui.fonts.FontLoader.FontConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class CommandRunnerActivationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry = CommandRegistry.default

  "CommandRunner.activate" should "reflect non-default ligature settings in option selections per font role" in {
    val config = AppConfig.default.withFontConfig(
      FontConfig(enableLigatures = false, textLigatures = false, uiLigatures = true)
    )
    val runner = CommandRunner.empty.activate(registry, config)

    runner.optionSelections.get("code-ligatures") shouldBe Some(1)
    runner.optionSelections.get("text-ligatures") shouldBe Some(1)
    runner.optionSelections.get("ui-ligatures") shouldBe Some(0)
  }

  it should "split font settings into code, prose, and UI groups" in {
    val runner   = CommandRunner.empty.activate(registry, AppConfig.default)
    val groupIds = runner.settingsGroups.map(_.id)

    groupIds should contain allOf ("settings-code-font", "settings-prose-font", "settings-ui-font")
    groupIds should not contain "settings-typography"

    runner.settingsGroups.find(_.id == "settings-code-font").map(_.children.map(_.id)) should contain(
      List("code-font", "code-ligatures", "code-font-size")
    )
    runner.settingsGroups.find(_.id == "settings-prose-font").map(_.children.map(_.id)) should contain(
      List("text-font", "text-ligatures", "text-font-size")
    )
    runner.settingsGroups.find(_.id == "settings-ui-font").map(_.children.map(_.id)) should contain(
      List("ui-font", "ui-ligatures", "ui-font-size")
    )
  }

  it should "reflect non-default code font family in option selections" in {
    val config = AppConfig.default.withFontConfig(FontConfig(codeFontFamily = "Courier New"))
    val runner = CommandRunner.empty.activate(registry, config)

    val expectedIndex = com.serenity.ui.fonts.FontLoader.availableMonospaceFamilies.indexOf("Courier New")
    if expectedIndex >= 0 then runner.optionSelections.get("code-font") shouldBe Some(expectedIndex)
    else succeed
  }

  it should "include keymap editing rows seeded from current bindings" in {
    val config = AppConfig.default
      .withHotkeyOverride(HotkeyAction.ToggleCommandRunner, "ctrl+k")
      .withCommandRunnerKeyOverride(CommandRunnerKeyAction.Submit, "ctrl+enter")
    val runner = CommandRunner.empty.activate(registry, config)

    val keymapGroup = runner.settingsGroups.find(_.id == "settings-keymap").getOrElse(fail("Expected keymap group"))

    keymapGroup.label shouldBe "Keymap"
    keymapGroup.children.collectFirst {
      case item: CommandSurfaceItem.InputItem if item.id == "keymap-global-command_palette" => item.currentValue
    } shouldBe Some("ctrl+k")
    keymapGroup.children.collectFirst {
      case item: CommandSurfaceItem.InputItem if item.id == "keymap-command-runner-submit" => item.currentValue
    } shouldBe Some("ctrl+enter")
  }

  it should "expose interface density in the appearance settings group" in {
    val config = AppConfig.default
      .withInterfaceDensity(InterfaceDensity.Compact)
      .withUiElementGap(2)
      .withUiCornerRadiusPx(6)
    val runner = CommandRunner.empty.activate(registry, config)

    runner.optionSelections.get("interface-density") shouldBe Some(0)
    runner.settingsGroups.find(_.id == "settings-appearance").map(_.children.map(_.id)) should contain(
      List(
        "cursor-mode",
        "cursor-info-bar",
        "background-style",
        "interface-density",
        "blur-radius",
        "ui-element-gap",
        "ui-corner-radius",
        "cursor-info-bar-placement"
      )
    )
    runner.settingsGroups
      .find(_.id == "settings-appearance")
      .flatMap(
        _.children.collectFirst {
          case item: CommandSurfaceItem.InputItem if item.id == "ui-element-gap" =>
            (item.currentValue, item.hint, item.parse("3"), item.parse("9"))
        }
      ) shouldBe Some(("2", "Cells (0-8)", Some(CommandIntent.SetUiElementGap(3)), None))
    runner.settingsGroups
      .find(_.id == "settings-appearance")
      .flatMap(
        _.children.collectFirst {
          case item: CommandSurfaceItem.InputItem if item.id == "ui-corner-radius" =>
            (item.currentValue, item.hint, item.parse("14"), item.parse("33"))
        }
      ) shouldBe Some(("6", "Pixels (0-32)", Some(CommandIntent.SetUiCornerRadiusPx(14)), None))
  }

  it should "expose cursor info bar mode in the appearance settings group" in {
    val config = AppConfig.default.withCursorInfoBarMode(CursorInfoBarMode.Detailed)
    val runner = CommandRunner.empty.activate(registry, config)

    runner.optionSelections.get("cursor-info-bar") shouldBe Some(2)
    runner.settingsGroups.find(_.id == "settings-appearance").map(_.children.map(_.id)) should contain(
      List(
        "cursor-mode",
        "cursor-info-bar",
        "background-style",
        "interface-density",
        "blur-radius",
        "ui-element-gap",
        "ui-corner-radius",
        "cursor-info-bar-placement"
      )
    )
  }

  it should "expose cursor info bar placement in the appearance settings group" in {
    val config = AppConfig.default.withCursorInfoBarPlacement(CursorInfoBarPlacement.PinnedBottom)
    val runner = CommandRunner.empty.activate(registry, config)

    runner.optionSelections.get("cursor-info-bar-placement") shouldBe Some(1)
    runner.settingsGroups
      .find(_.id == "settings-appearance")
      .map(_.children.collect {
        case item: CommandSurfaceItem.OptionItem if item.id == "cursor-info-bar-placement" =>
          item.selectedOption -> item.options.map(_.label)
      }) should contain(List("Pinned Bottom" -> List("Floating", "Pinned Bottom")))
  }

  it should "expose material and motion presets with current selections" in {
    val config = AppConfig.default
      .withMaterialPreset(MaterialPreset.Crystal)
      .withMotionPreset(MotionPreset.Reduced)
      .withElementTransitionSpeedScale(1.5)
    val runner = CommandRunner.empty.activate(registry, config)

    val materialGroup = runner.settingsGroups.find(_.id == "settings-material-motion").getOrElse {
      fail("Expected material and motion settings group")
    }
    materialGroup.children.collectFirst {
      case item: CommandSurfaceItem.OptionItem if item.id == "material-preset" =>
        (item.selectedOption, item.options.map(_.label))
    } shouldBe Some("Crystal" -> List("Solid", "Clear", "Frosted", "Crystal", "Custom"))
    materialGroup.children.collectFirst {
      case item: CommandSurfaceItem.OptionItem if item.id == "motion-preset" =>
        (item.selectedOption, item.options.map(_.label))
    } shouldBe Some("Reduced" -> List("Reduced", "Subtle", "Smooth", "Expressive", "Custom"))
    materialGroup.children.collectFirst {
      case item: CommandSurfaceItem.InputItem if item.id == "element-transition-speed-scale" =>
        (item.currentValue, item.hint, item.parse("2.25"))
    } shouldBe Some(("1.50", "Scale (0.0-4.0)", Some(CommandIntent.SetElementTransitionSpeedScale(2.25))))
  }

  "ensureCommandRunnerSurface (via closePane)" should "use the current config, not defaults" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val sm                  = com.serenity.state.manager.StateManager.apply(logger).unsafeRunSync()

    sm.updateState(s =>
      s.copy(config = s.config.withFontConfig(s.config.fontConfig.copy(enableLigatures = false, textLigatures = false)))
    ).unsafeRunSync()

    val stateBefore = sm.getCurrentState.unsafeRunSync()
    val paneId      = stateBefore.layout.editorPanes.keys.head

    sm.closePane(paneId).unsafeRunSync()

    val stateAfter = sm.getCurrentState.unsafeRunSync()
    val runner = stateAfter.commandRunnerSurface
      .map(_.content)
      .collect { case SurfaceContent.CommandPalette(r) => r }

    runner shouldBe defined
    runner.get.optionSelections.get("code-ligatures") shouldBe Some(1)
    runner.get.optionSelections.get("text-ligatures") shouldBe Some(1)
  }
