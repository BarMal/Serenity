package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.TransitionKind
import com.serenity.command.*
import com.serenity.config.*
import com.serenity.keystroke.KeyboardFidelityTier
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

  private def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
    group.children.flatMap {
      case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
      case child                               => List(child)
    }

  private def settingsGroup(runner: CommandRunner, id: String): Option[CommandSurfaceItem.GroupItem] =
    (runner.settingsGroups ++ runner.settingsGroups.flatMap(group =>
      descendants(group).collect { case child: CommandSurfaceItem.GroupItem => child }
    )).find(_.id == id)

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
    val runner = CommandRunner.empty.activate(registry, AppConfig.default)
    val groupIds = runner.settingsGroups.flatMap(group =>
      group.id :: descendants(group).collect { case child: CommandSurfaceItem.GroupItem => child.id }
    )

    groupIds should contain allOf ("settings-code-font", "settings-prose-font", "settings-ui-font")
    runner.settingsGroups.map(_.id) should contain("settings-typography")

    settingsGroup(runner, "settings-code-font").map(_.children.map(_.id)) should contain(
      List("code-font", "code-ligatures", "code-font-size")
    )
    settingsGroup(runner, "settings-prose-font").map(_.children.map(_.id)) should contain(
      List("text-font", "text-ligatures", "text-font-size")
    )
    settingsGroup(runner, "settings-ui-font").map(_.children.map(_.id)) should contain(
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
      .withKeymapBinding(KeymapGroup.CommandRunner)(CommandRunnerKeyAction.Submit, "ctrl+enter")
    val runner = CommandRunner.empty.activate(registry, config)

    val keymapGroup = runner.settingsGroups.find(_.id == "settings-keymap").getOrElse(fail("Expected keymap group"))

    keymapGroup.label shouldBe "Keymap"
    keymapGroup.children.collectFirst {
      case item: CommandSurfaceItem.InputItem if item.id == "keymap-global-command_palette" => item.currentValue
    } shouldBe Some("ctrl+k")
    keymapGroup.children.collectFirst {
      case item: CommandSurfaceItem.InputItem if item.id == "keymap-command-runner-submit" => item.currentValue
    } shouldBe Some("ctrl+enter")
    val ids = keymapGroup.children.map(_.id).toSet
    HotkeyAction.values.foreach(action => ids should contain(s"keymap-global-${action.configKey}"))
    EditorKeyAction.values.foreach(action => ids should contain(s"keymap-editor-${action.configKey}"))
    CommandRunnerKeyAction.values.foreach(action => ids should contain(s"keymap-command-runner-${action.configKey}"))
    ModalKeyAction.values.foreach(action => ids should contain(s"keymap-modal-${action.configKey}"))
    PanelKeyAction.values.foreach(action => ids should contain(s"keymap-panel-${action.configKey}"))
    PeekKeyAction.values.foreach(action => ids should contain(s"keymap-peek-${action.configKey}"))
  }

  it should "expose interface density and restart-only window chrome in the interface layout settings group" in {
    val config = AppConfig.default
      .withInterfaceDensity(InterfaceDensity.Compact)
      .withWindowChromeMode(WindowChromeMode.NativeThemed)
      .withUiElementGap(2)
      .withUiCornerRadiusPx(6)
      .withUiOutlineThicknessPx(3)
    val runner = CommandRunner.empty.activate(registry, config)

    runner.optionSelections.get("interface-density") shouldBe Some(0)
    runner.optionSelections.get("window-chrome") shouldBe Some(2)
    settingsGroup(runner, "settings-interface-layout").map(_.children.map(_.id)) should contain(
      List(
        "interface-density",
        "window-chrome",
        "command-runner-key-hints",
        "ui-element-gap",
        "ui-corner-radius",
        "ui-outline-thickness",
        "command-runner-visible-rows",
        "command-runner-item-gap-rows",
        "command-runner-cursor-gap-rows"
      )
    )
    settingsGroup(runner, "settings-interface-layout")
      .flatMap(
        _.children.collectFirst { case item: CommandSurfaceItem.OptionItem if item.id == "window-chrome" => item }
      )
      .map(item => (item.selectedIndex, item.hint)) shouldBe
      Some((2, Some("Applies after restart; auto uses Serenity chrome on Linux")))
    settingsGroup(runner, "settings-interface-layout")
      .flatMap(
        _.children.collectFirst {
          case item: CommandSurfaceItem.InputItem if item.id == "ui-element-gap" =>
            (item.currentValue, item.hint, item.parse("3"), item.parse("9"))
        }
      ) shouldBe Some(
      (
        "2",
        "Cells, decimals supported (0.0-8.0)",
        Some(CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetUiElementGap(3)))),
        None
      )
    )
    settingsGroup(runner, "settings-interface-layout")
      .flatMap(
        _.children.collectFirst {
          case item: CommandSurfaceItem.InputItem if item.id == "ui-corner-radius" =>
            (item.currentValue, item.hint, item.parse("14"), item.parse("33"))
        }
      ) shouldBe Some(
      (
        "6",
        "Pixels (0-32)",
        Some(CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetUiCornerRadiusPx(14)))),
        None
      )
    )
    settingsGroup(runner, "settings-interface-layout")
      .flatMap(
        _.children.collectFirst {
          case item: CommandSurfaceItem.InputItem if item.id == "ui-outline-thickness" =>
            (item.currentValue, item.hint, item.parse("4"), item.parse("9"))
        }
      ) shouldBe Some(
      (
        "3",
        "Pixels (1-8)",
        Some(CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetUiOutlineThicknessPx(4)))),
        None
      )
    )
  }

  it should "expose focused text body and contextual toolbar in the text display settings group" in {
    val config = AppConfig.default
      .withFocusedTextBody(true)
      .withContextualToolbarEnabled(false)
    val runner = CommandRunner.empty.activate(registry, config)

    runner.optionSelections.get("focused-text-body") shouldBe Some(0)
    runner.optionSelections.get("contextual-toolbar") shouldBe Some(1)
    settingsGroup(runner, "settings-text-display").map(_.children.map(_.id)) should contain(
      List(
        "line-numbers",
        "gutter",
        "line-wrap",
        "visual-line-navigation",
        "show-word-count",
        "focused-text-body",
        "contextual-toolbar",
        "contextual-toolbar-display"
      )
    )
  }

  it should "expose contextual toolbar display mode in the text display settings group" in {
    val config = AppConfig.default.withContextualToolbarDisplayMode(ToolbarDisplayMode.TextOnly)
    val runner = CommandRunner.empty.activate(registry, config)

    runner.optionSelections.get("contextual-toolbar-display") shouldBe Some(1)
    settingsGroup(runner, "settings-text-display")
      .flatMap(
        _.children.collectFirst {
          case item: CommandSurfaceItem.OptionItem if item.id == "contextual-toolbar-display" =>
            item.selectedOption -> item.options.map(_.label)
        }
      ) shouldBe Some("Text Only" -> List("Icon Only", "Text Only", "Icon + Text"))
  }

  it should "expose cursor info bar segment toggles in the appearance settings group" in {
    val config = AppConfig.default.withCursorInfoBarSegments(List(CursorInfoBarSegment.Position))
    val runner = CommandRunner.empty.activate(registry, config)

    runner.optionSelections.get("cursor-info-bar-position") shouldBe Some(0)
    runner.optionSelections.get("cursor-info-bar-title") shouldBe Some(1)
    settingsGroup(runner, "settings-cursor").map(_.children.map(_.id)) should contain(
      List(
        "cursor-mode",
        "cursor-info-bar-title",
        "cursor-info-bar-position",
        "cursor-info-bar-word-count",
        "cursor-info-bar-char-count",
        "cursor-info-bar-reading-time",
        "cursor-info-bar-placement"
      )
    )
  }

  it should "expose reorder commands once 2+ cursor info bar segments are included" in {
    val config = AppConfig.default.withCursorInfoBarSegments(
      List(CursorInfoBarSegment.Position, CursorInfoBarSegment.Title)
    )
    val runner = CommandRunner.empty.activate(registry, config)

    settingsGroup(runner, "settings-cursor").map(_.children.map(_.id)) should contain(
      List(
        "cursor-mode",
        "cursor-info-bar-title",
        "cursor-info-bar-position",
        "cursor-info-bar-word-count",
        "cursor-info-bar-char-count",
        "cursor-info-bar-reading-time",
        "move-cursor-info-bar-title-earlier",
        "move-cursor-info-bar-title-later",
        "move-cursor-info-bar-position-earlier",
        "move-cursor-info-bar-position-later",
        "cursor-info-bar-placement"
      )
    )
  }

  it should "expose cursor info bar placement in the appearance settings group" in {
    val config = AppConfig.default.withCursorInfoBarPlacement(CursorInfoBarPlacement.PinnedBottom)
    val runner = CommandRunner.empty.activate(registry, config)

    runner.optionSelections.get("cursor-info-bar-placement") shouldBe Some(1)
    settingsGroup(runner, "settings-cursor")
      .map(_.children.collect {
        case item: CommandSurfaceItem.OptionItem if item.id == "cursor-info-bar-placement" =>
          item.selectedOption -> item.options.map(_.label)
      }) should contain(List("Pinned Bottom" -> List("Floating", "Pinned Bottom")))
  }

  it should "expose material and motion presets with current selections" in {
    val config = AppConfig.default
      .withMaterialPreset(MaterialPreset.Crystal)
      .withMotionPreset(MotionPreset.Reduced)
      .withEditorInsertionTransitionKind(TransitionKind.TypedText)
      .withElementTransitionSpeedScale(1.5)
      .withEditorTextTransitionSpeedScale(Some(0.5))
      .withCommandRunnerTransitionSpeedScale(Some(2.25))
      .withUiTransitionSpeedScale(Some(1.25))
      .withCursorTransitionSpeedScale(Some(0.75))
      .withPanelOpenTransitionKind(Some(TransitionKind.DirectionalSweep))
      .withPanelCloseTransitionKind(Some(TransitionKind.Disabled))
      .withCommandRunnerTransitionKind(Some(TransitionKind.OutlineThenContent))
    val runner = CommandRunner.empty.activate(registry, config)

    val surfaceGroup = settingsGroup(runner, "settings-surface-appearance").getOrElse {
      fail("Expected surface appearance settings group")
    }
    val motionGroup = settingsGroup(runner, "settings-animation").getOrElse {
      fail("Expected motion and animation settings group")
    }
    surfaceGroup.children.collectFirst {
      case item: CommandSurfaceItem.OptionItem if item.id == "material-preset" =>
        (item.selectedOption, item.options.map(_.label))
    } shouldBe Some("Crystal" -> List("Solid", "Clear", "Frosted", "Crystal", "Custom"))
    motionGroup.children.collectFirst {
      case item: CommandSurfaceItem.OptionItem if item.id == "motion-preset" =>
        (item.selectedOption, item.options.map(_.label))
    } shouldBe Some("Reduced" -> List("Reduced", "Subtle", "Smooth", "Expressive", "Custom"))
    motionGroup.children.collectFirst {
      case item: CommandSurfaceItem.InputItem if item.id == "element-transition-speed-scale" =>
        (item.currentValue, item.hint, item.parse("2.25"))
    } shouldBe Some(
      (
        "1.50",
        "Scale (0.0-4.0)",
        Some(CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetElementTransitionSpeedScale(2.25))))
      )
    )
    motionGroup.children.collectFirst {
      case item: CommandSurfaceItem.InputItem if item.id == "editor-text-speed-scale" =>
        (item.currentValue, item.hint, item.parse("0.75"))
    } shouldBe Some(
      (
        "0.50",
        "Editor text scale (0.0-4.0)",
        Some(CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetEditorTextTransitionSpeedScale(0.75))))
      )
    )
    motionGroup.children.collectFirst {
      case item: CommandSurfaceItem.InputItem if item.id == "command-runner-speed-scale" =>
        (item.currentValue, item.hint, item.parse("1.75"))
    } shouldBe Some(
      (
        "2.25",
        "Command runner scale (0.0-4.0)",
        Some(CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerTransitionSpeedScale(1.75))))
      )
    )
    motionGroup.children.collectFirst {
      case item: CommandSurfaceItem.InputItem if item.id == "ui-speed-scale" =>
        (item.currentValue, item.hint, item.parse("1.00"))
    } shouldBe Some(
      (
        "1.25",
        "Panel/UI scale (0.0-4.0)",
        Some(CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetUiTransitionSpeedScale(1.0))))
      )
    )
    motionGroup.children.collectFirst {
      case item: CommandSurfaceItem.OptionItem if item.id == "editor-text-transition" =>
        (item.selectedOption, item.options.map(_.label))
    } shouldBe Some("Typed" -> List("Fade", "Typed", "Directional", "Tandem", "Off"))
    motionGroup.children.collectFirst {
      case item: CommandSurfaceItem.OptionItem if item.id == "panel-open-transition" =>
        (item.selectedOption, item.options.map(_.label))
    } shouldBe Some("Directional" -> List("Fade", "Directional", "Tandem", "Outline", "Off"))
    motionGroup.children.collectFirst {
      case item: CommandSurfaceItem.OptionItem if item.id == "panel-close-transition" =>
        (item.selectedOption, item.options.map(_.label))
    } shouldBe Some("Off" -> List("Fade", "Directional", "Tandem", "Outline", "Off"))
    motionGroup.children.collectFirst {
      case item: CommandSurfaceItem.OptionItem if item.id == "command-runner-transition" =>
        (item.selectedOption, item.options.map(_.label))
    } shouldBe Some("Outline" -> List("Fade", "Directional", "Tandem", "Outline", "Off"))
    motionGroup.children.collectFirst {
      case item: CommandSurfaceItem.OptionItem if item.id == "command-runner-fade" =>
        (item.selectedOption, item.options.map(_.label))
    } shouldBe Some("Off" -> List("Off", "Subtle", "Smooth", "Expressive"))
    motionGroup.children.collectFirst {
      case item: CommandSurfaceItem.InputItem if item.id == "cursor-speed-scale" =>
        (item.currentValue, item.hint, item.parse("0.25"))
    } shouldBe Some(
      (
        "0.75",
        "Cursor scale (0.0-4.0)",
        Some(CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCursorTransitionSpeedScale(0.25))))
      )
    )
  }

  "ensureCommandRunnerSurface (via closePane)" should "use the current config, not defaults" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val sm                  = com.serenity.state.manager.StateManager.apply(logger).unsafeRunSync()

    sm.updateState(s =>
      s.copy(persisted =
        s.persisted.copy(config =
          s.persisted.config
            .withFontConfig(
              s.persisted.config.editorConfig.fontConfig.copy(enableLigatures = false, textLigatures = false)
            )
        )
      )
    ).unsafeRunSync()

    val stateBefore = sm.getCurrentState.unsafeRunSync()
    val paneId      = stateBefore.persisted.layout.editorPanes.keys.head

    sm.closePane(paneId).unsafeRunSync()

    val stateAfter = sm.getCurrentState.unsafeRunSync()
    val runner = stateAfter.commandRunnerSurface
      .map(_.content)
      .collect { case SurfaceContent.CommandPalette(r) => r }

    runner shouldBe defined
    runner.get.optionSelections.get("code-ligatures") shouldBe Some(1)
    runner.get.optionSelections.get("text-ligatures") shouldBe Some(1)
  }

  "CommandRunner.activate with isTuiMode" should "default to false and carry the flag into settingsGroups (issue #1112)" in {
    val defaultRunner = CommandRunner.empty.activate(registry, AppConfig.default)
    defaultRunner.isTuiMode shouldBe false
    settingsGroup(defaultRunner, "settings-typography").flatMap(_.hint) shouldBe
      Some("Typefaces for prose, code, and interface")

    val tuiRunner = CommandRunner.empty.activate(registry, AppConfig.default, isTuiMode = true)
    tuiRunner.isTuiMode shouldBe true
    settingsGroup(tuiRunner, "settings-typography").flatMap(_.hint) shouldBe
      Some("Typefaces for prose, code, and interface -- inert in TUI mode")
  }

  "CommandRunner.activate with keyboardFidelityTier" should "default to Full and carry the negotiated tier through (issue #1194)" in {
    val defaultRunner = CommandRunner.empty.activate(registry, AppConfig.default)
    defaultRunner.keyboardFidelityTier shouldBe KeyboardFidelityTier.Full

    val cappedRunner = CommandRunner.empty.activate(
      registry,
      AppConfig.default,
      isTuiMode = true,
      keyboardFidelityTier = KeyboardFidelityTier.ModifyOtherKeys
    )
    cappedRunner.keyboardFidelityTier shouldBe KeyboardFidelityTier.ModifyOtherKeys
  }
