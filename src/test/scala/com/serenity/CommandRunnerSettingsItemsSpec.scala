package com.serenity.command

import com.serenity.config.{StatusSegment, WindowChromeMode}
import com.serenity.ui.presets.UiPreset
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerSettingsItemsSpec extends AnyFlatSpec with Matchers:

  // issue #1299/#1044: boolean settings toggles such as "Show All Settings" and "Spell Check" were built inline with
  // their options ordered Off/On, the one encoding in the settings tree that disagreed with
  // `CommandRunnerSettingsOptionItemHelpers.enabledOptionItem`'s On/Off convention every other boolean toggle follows
  // (`code-ligatures`, the `enabledOptionItem`-built toggles, etc). They are normalized onto that one pattern now.
  "boolean toggle settings" should "all order their options On, Off, matching the shared enabledOptionItem convention" in {
    val onOffToggles = List(
      CommandRunnerSettingsItems.showAllSettingsOptionItem(Map.empty),
      CommandRunnerSettingsItems.spellCheckOptionItem(Map.empty)
    )

    onOffToggles.foreach { toggle =>
      withClue(s"${toggle.id}: ") {
        toggle.options.map(_.label) shouldBe List("On", "Off")
      }
    }
  }

  "CommandRunnerSettingsItems" should "build typed option rows independently of runner state" in {
    val cursor = CommandRunnerSettingsCursorItems.cursorModeOptionItem(Map("cursor-mode" -> 0))
    val chrome = CommandRunnerSettingsAppearanceItems.windowChromeOptionItem(Map("window-chrome" -> 0))

    cursor.label shouldBe "Cursor Style"
    cursor.selectedOption shouldBe "Blink"
    chrome.selectedOption shouldBe "Auto (Linux Custom)"
    chrome.selectedIntent shouldBe Some(
      CommandIntent.Settings(
        SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetWindowChromeMode(WindowChromeMode.Auto))
      )
    )
    chrome.options.map(_.label) shouldBe List("Auto (Linux Custom)", "Native", "Native Themed (Windows)", "Custom")
  }

  it should "offer the Arrange Panels list in place of a pin row per panel and separate order commands" in {
    CommandRunnerSettingsPanelItems.workspaceLayoutItems.collect {
      case CommandSurfaceItem.CommandItem(command, _) => command.name -> command.intent
    } shouldBe List("arrange-panels" -> CommandIntent.View(ViewIntent.ArrangePanels))
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
    CommandRunnerSettingsPanelItems.workspaceLayoutItems.map(_.id) should not contain "settings-panel-actions"
  }

  it should "build a command-runner key-hints option item toggling the persistent footer (issue #931, Stage 3)" in {
    val onByDefault = CommandRunnerSettingsTextDisplayItems.commandRunnerKeyHintsOptionItem(Map.empty)
    onByDefault.label shouldBe "Command Runner Key Hints"
    onByDefault.selectedOption shouldBe "On"
    onByDefault.selectedIntent shouldBe Some(
      CommandIntent.Settings(SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetCommandRunnerShowKeyHints(true)))
    )

    val explicitlyOff =
      CommandRunnerSettingsTextDisplayItems.commandRunnerKeyHintsOptionItem(Map("command-runner-key-hints" -> 1))
    explicitlyOff.selectedOption shouldBe "Off"
    explicitlyOff.selectedIntent shouldBe Some(
      CommandIntent.Settings(SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetCommandRunnerShowKeyHints(false)))
    )
  }

  it should "expose an include/exclude toggle per status line segment" in {
    val items = CommandRunnerSettingsStatusLineItems.segmentItems(Map.empty, currentOrder = Nil)

    items.collect { case o: CommandSurfaceItem.OptionItem => o.id } shouldBe List(
      "status-position",
      "status-title",
      "status-language",
      "status-mode",
      "status-word-count",
      "status-char-count",
      "status-reading-time",
      "status-word-goal"
    )
  }

  it should "expose no status line reorder commands when fewer than 2 segments are shown" in {
    val items = CommandRunnerSettingsStatusLineItems.segmentItems(
      Map("status-position" -> 0),
      currentOrder = List(StatusSegment.Position)
    )

    items.collect { case c: CommandSurfaceItem.CommandItem => c.command.name } shouldBe Nil
  }

  // #1298: the commands follow the line's real order and only the direction that would actually move the segment is
  // offered -- the first segment has no-op "earlier" suppressed, the last has no-op "later" suppressed.
  it should "order status line reorder commands by the current segment order and suppress no-op directions" in {
    val items = CommandRunnerSettingsStatusLineItems.segmentItems(
      Map("status-position" -> 0, "status-title" -> 0),
      currentOrder = List(StatusSegment.Position, StatusSegment.Title)
    )

    val moveCommands = items.collect { case c: CommandSurfaceItem.CommandItem => c.command }
    moveCommands.map(_.name) shouldBe List("move-status-position-later", "move-status-title-earlier")
    // #1298: pressing "move earlier/later" repeatedly shouldn't force a full menu re-open between presses.
    moveCommands.foreach(_.keepMenuOpenOnSubmit shouldBe true)
  }
