package com.serenity.command

import com.serenity.config.{BackgroundStyle, PostProcessingEffect, WindowChromeMode}
import com.serenity.ui.layout.PanelPosition
import com.serenity.ui.presets.UiPreset
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerSettingsItemsSpec extends AnyFlatSpec with Matchers:

  "CommandRunnerSettingsItems" should "build typed option rows independently of runner state" in {
    val background = CommandRunnerSettingsItems.backgroundStyleOptionItem(Map("background-style" -> 3))
    val cursor     = CommandRunnerSettingsItems.cursorModeOptionItem(Map("cursor-mode" -> 1))
    val chrome     = CommandRunnerSettingsItems.windowChromeOptionItem(Map("window-chrome" -> 0))

    background.label shouldBe "Background Style"
    background.selectedOption shouldBe "Glass"
    background.selectedIntent shouldBe Some(CommandIntent.SetBackgroundStyle(BackgroundStyle.GlassLike))
    background.options.map(_.label) shouldBe List("Solid", "Transparent", "Frosted", "Glass")

    val postProcessing = CommandRunnerSettingsItems.postProcessingOptionItem(Map("post-processing" -> 2))
    postProcessing.label shouldBe "Post-processing"
    postProcessing.selectedOption shouldBe "Glow"
    postProcessing.selectedIntent shouldBe Some(CommandIntent.SetPostProcessingEffect(PostProcessingEffect.Glow))
    postProcessing.options.map(_.label) shouldBe List("Off", "Scanlines", "Glow")

    cursor.label shouldBe "Cursor Style"
    cursor.selectedOption shouldBe "Breathe"
    chrome.selectedOption shouldBe "Auto (Linux Rounded)"
    chrome.selectedIntent shouldBe Some(CommandIntent.SetWindowChromeMode(WindowChromeMode.Auto))
    chrome.options.map(_.label) shouldBe List("Auto (Linux Rounded)", "Native", "Native Themed (Windows)", "Custom")
  }

  it should "build workspace panel controls with bounded selections" in {
    val workspaceItems = CommandRunnerSettingsItems.workspaceLayoutItems(
      Map("panel-outline-pin" -> 2, "panel-diagnostics-pin" -> 99)
    )

    val panelPins = workspaceItems
      .collectFirst { case group: CommandSurfaceItem.GroupItem if group.id == "settings-panel-pins" => group }
      .getOrElse(fail("missing panel pins group"))
    val outline = panelPins.children
      .collectFirst { case option: CommandSurfaceItem.OptionItem if option.id == "panel-outline-pin" => option }
      .getOrElse(fail("missing outline pin option"))
    val diagnostics = panelPins.children
      .collectFirst { case option: CommandSurfaceItem.OptionItem if option.id == "panel-diagnostics-pin" => option }
      .getOrElse(fail("missing diagnostics pin option"))

    panelPins.children.map(_.id) shouldBe List(
      "panel-explorer-pin",
      "panel-outline-pin",
      "panel-diagnostics-pin",
      "panel-markdown-preview-pin"
    )
    outline.selectedOption shouldBe "Right"
    outline.selectedIntent shouldBe Some(CommandIntent.SetPanelPin(PanelKind.Outline, Some(PanelPosition.Right)))
    diagnostics.selectedOption shouldBe "Left"
    diagnostics.selectedIntent shouldBe Some(
      CommandIntent.SetPanelPin(PanelKind.Diagnostics, Some(PanelPosition.Left))
    )
    workspaceItems.map(_.id) should not contain "settings-panel-order"
  }

  it should "hide panel order controls unless multiple panels share an edge" in {
    val noPanels = CommandRunnerSettingsItems.workspaceLayoutItems(Map.empty)
    val separateEdges = CommandRunnerSettingsItems.workspaceLayoutItems(
      Map("panel-outline-pin" -> 2, "panel-diagnostics-pin" -> 3)
    )
    val sameEdge = CommandRunnerSettingsItems.workspaceLayoutItems(
      Map("panel-outline-pin" -> 2, "panel-diagnostics-pin" -> 2)
    )
    val sameEdgeWithSeparatePanel = CommandRunnerSettingsItems.workspaceLayoutItems(
      Map("panel-outline-pin" -> 2, "panel-diagnostics-pin" -> 2, "panel-explorer-pin" -> 3)
    )

    noPanels.map(_.id) should not contain "settings-panel-order"
    separateEdges.map(_.id) should not contain "settings-panel-order"

    val panelOrder = sameEdge
      .collectFirst { case group: CommandSurfaceItem.GroupItem if group.id == "settings-panel-order" => group }
      .getOrElse(fail("missing panel order group"))
    panelOrder.children.collect { case CommandSurfaceItem.CommandItem(command) => command.name } shouldBe List(
      "move-outline-panel-earlier",
      "move-outline-panel-later",
      "move-diagnostics-panel-earlier",
      "move-diagnostics-panel-later"
    )

    val separatePanelOrder = sameEdgeWithSeparatePanel
      .collectFirst { case group: CommandSurfaceItem.GroupItem if group.id == "settings-panel-order" => group }
      .getOrElse(fail("missing panel order group"))
    separatePanelOrder.children.collect { case CommandSurfaceItem.CommandItem(command) => command.name } shouldBe List(
      "move-outline-panel-earlier",
      "move-outline-panel-later",
      "move-diagnostics-panel-earlier",
      "move-diagnostics-panel-later"
    )
  }

  it should "hide panel actions for edges without pinned panels" in {
    val noPanels = CommandRunnerSettingsItems.workspaceLayoutItems(Map.empty)
    noPanels.map(_.id) should not contain "settings-panel-actions"
    val topOnlyPanel = CommandRunnerSettingsItems.workspaceLayoutItems(Map("panel-outline-pin" -> 1))
    topOnlyPanel.map(_.id) should not contain "settings-panel-actions"

    val leftAndRightPanels = CommandRunnerSettingsItems.workspaceLayoutItems(
      Map("panel-outline-pin" -> 4, "panel-diagnostics-pin" -> 2)
    )
    val panelActions = leftAndRightPanels
      .collectFirst { case group: CommandSurfaceItem.GroupItem if group.id == "settings-panel-actions" => group }
      .getOrElse(fail("missing panel actions group"))

    panelActions.children.collect { case CommandSurfaceItem.CommandItem(command) => command.name } shouldBe List(
      "focus-left-panel",
      "expand-left-panel",
      "unpin-left-panel",
      "focus-right-panel",
      "expand-right-panel",
      "unpin-right-panel",
      "collapse-expanded-panel"
    )
  }

  it should "normalize preset previews for the combined preset picker" in {
    val picker = CommandRunnerSettingsItems.uiPresetSelectOptionItem(
      previews = List(
        UiPreset.Preview(" Review ", " Saved workspace setup "),
        UiPreset.Preview("review", "duplicate"),
        UiPreset.Preview("Drafting", "Saved workspace setup")
      ),
      optionSelections = Map("ui-preset-custom" -> 1)
    )

    picker.options.map(_.label) shouldBe List("Writing", "Documentation", "Code", "Review", "Drafting", "Review")
    picker.selectedOption shouldBe "Review"
    picker.options.takeRight(2).map(_.hint) shouldBe List(Some("Saved workspace setup"), Some("Saved workspace setup"))
  }

  it should "build language and theme command items as settings surface rows" in {
    val themeIntents = CommandRunnerSettingsItems.themeItems.collect {
      case CommandSurfaceItem.CommandItem(command) =>
        command.intent
    }
    val languageIds = CommandRunnerSettingsItems.languageItems.map(_.id)

    themeIntents should contain allOf (
      CommandIntent.OpenThemeChooser,
      CommandIntent.ToggleTheme,
      CommandIntent.ReloadTheme
    )
    languageIds.headOption shouldBe Some("lang-plain-text")
    languageIds should contain("lang-scala")
  }
