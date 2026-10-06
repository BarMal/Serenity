package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.*
import com.serenity.config.AppConfigOps.*
import com.serenity.frontend.FrontendCapabilities
import com.serenity.keystroke.KeyboardFidelityTier
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.SurfaceContent
import com.serenity.testkit.SharedDictionary
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
    val runner = CommandRunner.empty.activate(registry, AppConfig.default.withShowAllSettingsRegardlessOfMode(true))
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

    def bindingRows(item: CommandSurfaceItem): List[CommandSurfaceItem.InputItem] =
      item match
        case group: CommandSurfaceItem.GroupItem => group.children.flatMap(bindingRows)
        case row: CommandSurfaceItem.InputItem   => List(row)
        case _                                   => Nil
    val rows = bindingRows(keymapGroup)

    keymapGroup.label shouldBe "Keys"
    rows.collectFirst {
      case item if item.id == "keymap-global-command_palette" => item.currentValue
    } shouldBe Some("ctrl+k")
    rows.collectFirst {
      case item if item.id == "keymap-command-runner-submit" => item.currentValue
    } shouldBe Some("ctrl+enter")
    val ids = rows.map(_.id).toSet
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
      .withUiElementGap(Some(2))
      .withUiOutlineThicknessPx(3)
    val runner = CommandRunner.empty.activate(registry, config)

    runner.optionSelections.get("interface-density") shouldBe Some(0)
    runner.optionSelections.get("window-chrome") shouldBe Some(2)
    // issue #1046: command-runner visible-rows/item-gap-rows/cursor-gap-rows are no longer separate rows here --
    // Interface Density above is the one control governing all three now.
    settingsGroup(runner, "settings-interface-layout").map(_.children.map(_.id)) should contain(
      List(
        "interface-density",
        "window-chrome",
        "command-runner-key-hints"
      )
    )
    settingsGroup(runner, "settings-interface-layout")
      .flatMap(
        _.children.collectFirst { case item: CommandSurfaceItem.OptionItem if item.id == "window-chrome" => item }
      )
      .map(item => (item.selectedIndex, item.hint)) shouldBe
      Some((2, Some("Applies after restart; auto uses Serenity chrome on Linux")))
    settingsGroup(runner, "settings-look-advanced")
      .flatMap(
        _.children.collectFirst {
          case item: CommandSurfaceItem.InputItem if item.id == "ui-element-gap" =>
            (item.currentValue, item.hint, item.parse("3"), item.parse("9"))
        }
      ) shouldBe Some(
      (
        "2",
        "Cells, decimals supported (0.0-8.0)",
        Some(CommandIntent.Settings(SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetUiElementGap(3)))),
        None
      )
    )
    settingsGroup(runner, "settings-look-advanced")
      .flatMap(
        _.children.collectFirst {
          case item: CommandSurfaceItem.InputItem if item.id == "ui-outline-thickness" =>
            (item.currentValue, item.hint, item.parse("4"), item.parse("9"))
        }
      ) shouldBe Some(
      (
        "3",
        "Pixels (1-8)",
        Some(CommandIntent.Settings(SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetUiOutlineThicknessPx(4)))),
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
        "line-number-side",
        "line-number-margin-left",
        "line-number-margin-right",
        "line-number-padding",
        "line-wrap",
        "visual-line-navigation",
        "typewriter-scrolling",
        "columns",
        "column-gap",
        "wheel-scroll-lines",
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

  private val statusToggleIds = List(
    "status-position",
    "status-title",
    "status-language",
    "status-mode",
    "status-word-count",
    "status-char-count",
    "status-reading-time",
    "status-word-goal"
  )

  it should "expose the status line's placement and one toggle per segment in its own settings group" in {
    val config = AppConfig.default.withStatusLineSegments(List(StatusSegment.Position))
    val runner = CommandRunner.empty.activate(registry, config)

    runner.optionSelections.get("status-position") shouldBe Some(0)
    runner.optionSelections.get("status-title") shouldBe Some(1)
    settingsGroup(runner, "settings-status-line").map(_.children.map(_.id)) shouldBe
      Some(("status-placement" :: statusToggleIds) :+ "word-goal")
  }

  // #1298: reorder commands are listed in the segments' real current order (Position, then Title) and only offer
  // the direction that would actually move the segment -- Position (first) has no "earlier", Title (last) has no
  // "later" -- rather than the fixed definition order with both directions always offered.
  it should "expose reorder commands, in current order and gated by position, once 2+ status segments are shown" in {
    val config = AppConfig.default.withStatusLineSegments(List(StatusSegment.Position, StatusSegment.Title))
    val runner = CommandRunner.empty.activate(registry, config)

    settingsGroup(runner, "settings-status-line").map(_.children.map(_.id)) shouldBe
      Some(
        "status-placement" :: statusToggleIds ++
          List("move-status-position-later", "move-status-title-earlier", "word-goal")
      )
  }

  it should "expose the status line placement as pinned, floating or off" in {
    val config = AppConfig.default.withStatusLinePlacement(StatusLinePlacement.Floating)
    val runner = CommandRunner.empty.activate(registry, config)

    runner.optionSelections.get("status-placement") shouldBe Some(1)
    settingsGroup(runner, "settings-status-line")
      .map(_.children.collect {
        case item: CommandSurfaceItem.OptionItem if item.id == "status-placement" =>
          item.selectedOption -> item.options.map(_.label)
      }) should contain(List("Floating" -> List("Pinned", "Floating", "Off")))
  }

  it should "expose the render cadence options with current selections" in {
    val config = AppConfig.default
      .withRenderFpsTarget(RenderFpsTarget.Fps120)
      .withRenderDamageGranularity(RenderDamageGranularity.Cells)
    val runner = CommandRunner.empty.activate(registry, config)

    val children = settingsGroup(runner, "settings-look-advanced").toList.flatMap(_.children)
    children.collectFirst {
      case item: CommandSurfaceItem.OptionItem if item.id == "render-fps" =>
        (item.selectedOption, item.options.map(_.label))
    } shouldBe Some("120 FPS" -> List("30 FPS", "60 FPS", "90 FPS", "120 FPS", "Uncapped"))
    children.collectFirst {
      case item: CommandSurfaceItem.OptionItem if item.id == "render-damage-granularity" =>
        (item.selectedOption, item.options.map(_.label))
    } shouldBe Some("Cells" -> List("Rows", "Cells"))
  }

  "ensureCommandRunnerSurface (via closePane)" should "use the current config, not defaults" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val sm =
      com.serenity.state.manager.StateManager.apply(logger, dictionaryCache = SharedDictionary.default).unsafeRunSync()

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

  "CommandRunner.activate with capabilities" should "default to GUI and carry the flag into settingsGroups (issue #1112)" in {
    val defaultRunner = CommandRunner.empty.activate(registry, AppConfig.default)
    defaultRunner.capabilities shouldBe FrontendCapabilities.gui
    settingsGroup(defaultRunner, "settings-typography").flatMap(_.hint) shouldBe
      Some("Typefaces for prose, code, and interface")

    // Every typography row a code workspace shows is GUI-only, so on a terminal the group has nothing left to offer.
    val tuiRunner = CommandRunner.empty.activate(registry, AppConfig.default, capabilities = FrontendCapabilities.tui())
    tuiRunner.capabilities.isCellGrid shouldBe true
    settingsGroup(tuiRunner, "settings-typography") shouldBe None
  }

  "CommandRunner.activate with keyboardFidelityTier" should "default to Full and carry the negotiated tier through (issue #1194)" in {
    val defaultRunner = CommandRunner.empty.activate(registry, AppConfig.default)
    defaultRunner.capabilities.keyboardFidelityTier shouldBe KeyboardFidelityTier.Full

    val cappedRunner = CommandRunner.empty.activate(
      registry,
      AppConfig.default,
      capabilities = FrontendCapabilities.tui(KeyboardFidelityTier.ModifyOtherKeys)
    )
    cappedRunner.capabilities.keyboardFidelityTier shouldBe KeyboardFidelityTier.ModifyOtherKeys
  }
