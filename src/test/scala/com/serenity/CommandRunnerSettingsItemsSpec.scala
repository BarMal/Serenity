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
    background.selectedIntent shouldBe Some(
      CommandIntent.Settings(
        SettingsIntent.General(GeneralSettingsIntent.SetBackgroundStyle(BackgroundStyle.GlassLike))
      )
    )
    background.options.map(_.label) shouldBe List("Solid", "Transparent", "Frosted", "Glass")

    val postProcessing = CommandRunnerSettingsItems.postProcessingOptionItem(Map("post-processing" -> 2))
    postProcessing.label shouldBe "Post-processing"
    postProcessing.selectedOption shouldBe "Glow"
    postProcessing.selectedIntent shouldBe Some(
      CommandIntent.Settings(
        SettingsIntent.General(GeneralSettingsIntent.SetPostProcessingEffect(PostProcessingEffect.Glow))
      )
    )
    postProcessing.options.map(_.label) shouldBe List("Off", "Scanlines", "Glow", "Scanlines + Glow")

    val shadows = CommandRunnerSettingsItems.uiShadowsOptionItem(Map("ui-shadows" -> 1))
    shadows.selectedOption shouldBe "On"
    shadows.selectedIntent shouldBe Some(
      CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetUiShadowsEnabled(true)))
    )

    cursor.label shouldBe "Cursor Style"
    cursor.selectedOption shouldBe "Breathe"
    chrome.selectedOption shouldBe "Auto (Linux Rounded)"
    chrome.selectedIntent shouldBe Some(
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowChromeMode(WindowChromeMode.Auto)))
    )
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
      "panel-comments-pin",
      "panel-diagnostics-pin",
      "panel-markdown-preview-pin"
    )
    outline.selectedOption shouldBe "Right"
    outline.selectedIntent shouldBe Some(
      CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Outline, Some(PanelPosition.Right)))
    )
    diagnostics.selectedOption shouldBe "Left"
    diagnostics.selectedIntent shouldBe Some(
      CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Diagnostics, Some(PanelPosition.Left)))
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

  it should "normalize preset previews for the combined preset picker" in {
    val picker = CommandRunnerSettingsItems.uiPresetSelectOptionItem(
      previews = List(
        UiPreset.Preview(" Review ", " Saved workspace setup "),
        UiPreset.Preview("review", "duplicate"),
        UiPreset.Preview("Drafting", "Saved workspace setup")
      ),
      optionSelections = Map("ui-preset-custom" -> 1)
    )

    picker.options
      .map(_.label) shouldBe List("Writing", "Documentation", "Code", "Compact", "Review", "Drafting", "Review")
    picker.selectedOption shouldBe "Review"
    picker.options.takeRight(2).map(_.hint) shouldBe List(Some("Saved workspace setup"), Some("Saved workspace setup"))
  }

  // issue #1057: `themeItems`/`languageItems` (this test's original subject) are removed -- theme and
  // buffer-language one-shot actions are ordinary `CommandRegistry` commands now, covered by
  // `CommandRunnerOneShotActionsSpec` instead.

  it should "never build panel-actions settings-tree duplicates, even with panels pinned on both edges" in {
    // issue #1057: Focus/Expand/Unpin/Collapse used to appear here as a "Panel Actions" settings group once two
    // edges had pinned panels -- that was a duplicate of ordinary CommandRegistry commands with no persisted value
    // of its own. It never appears now, regardless of what's pinned.
    val noPanels = CommandRunnerSettingsItems.workspaceLayoutItems(Map.empty)
    noPanels.map(_.id) should not contain "settings-panel-actions"

    val leftAndRightPanels = CommandRunnerSettingsItems.workspaceLayoutItems(
      Map("panel-outline-pin" -> 4, "panel-diagnostics-pin" -> 2)
    )
    leftAndRightPanels.map(_.id) should not contain "settings-panel-actions"
  }

  it should "build a command-runner key-hints option item toggling the persistent footer (issue #931, Stage 3)" in {
    val onByDefault = CommandRunnerSettingsItems.commandRunnerKeyHintsOptionItem(Map.empty)
    onByDefault.label shouldBe "Command Runner Key Hints"
    onByDefault.selectedOption shouldBe "On"
    onByDefault.selectedIntent shouldBe Some(
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetCommandRunnerShowKeyHints(true)))
    )

    val explicitlyOff =
      CommandRunnerSettingsItems.commandRunnerKeyHintsOptionItem(Map("command-runner-key-hints" -> 1))
    explicitlyOff.selectedOption shouldBe "Off"
    explicitlyOff.selectedIntent shouldBe Some(
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetCommandRunnerShowKeyHints(false)))
    )
  }

  it should "expose 5 cursor info bar include/exclude toggles (#1261)" in {
    val items = CommandRunnerSettingsItems.cursorInfoBarSegmentItems(Map.empty)

    items.collect { case o: CommandSurfaceItem.OptionItem => o.id } shouldBe List(
      "cursor-info-bar-title",
      "cursor-info-bar-position",
      "cursor-info-bar-word-count",
      "cursor-info-bar-char-count",
      "cursor-info-bar-reading-time"
    )
  }

  it should "expose no cursor info bar reorder commands when fewer than 2 segments are included" in {
    val items = CommandRunnerSettingsItems.cursorInfoBarSegmentItems(Map("cursor-info-bar-position" -> 0))

    items.collect { case c: CommandSurfaceItem.CommandItem => c.command.name } shouldBe Nil
  }

  it should "expose earlier/later reorder commands for each included cursor info bar segment once 2+ are included" in {
    val items = CommandRunnerSettingsItems.cursorInfoBarSegmentItems(
      Map("cursor-info-bar-position" -> 0, "cursor-info-bar-title" -> 0)
    )

    items.collect { case c: CommandSurfaceItem.CommandItem => c.command.name } shouldBe List(
      "move-cursor-info-bar-title-earlier",
      "move-cursor-info-bar-title-later",
      "move-cursor-info-bar-position-earlier",
      "move-cursor-info-bar-position-later"
    )
  }
