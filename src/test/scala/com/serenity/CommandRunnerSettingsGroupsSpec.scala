package com.serenity

import com.serenity.command.*
import com.serenity.config.{AppConfig, AppMode}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.presets.UiPreset
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerSettingsGroupsSpec extends AnyFlatSpec with Matchers:

  private def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
    group.children.flatMap {
      case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
      case child                               => List(child)
    }

  private def groupById(
    groups: List[CommandSurfaceItem.GroupItem],
    id: String
  ): CommandSurfaceItem.GroupItem =
    (groups ++ groups.flatMap(group =>
      descendants(group).collect { case child: CommandSurfaceItem.GroupItem => child }
    ))
      .find(_.id == id)
      .getOrElse(fail(s"missing group $id"))

  "CommandRunnerSettingsGroups" should "build the settings taxonomy independently of runner navigation state" in {
    // "Show all settings" is on so this test can see the full tree, mode filtering (issue #1297) aside.
    val config = AppConfig.default.withShowAllSettingsRegardlessOfMode(true)
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = CommandRunnerOptionSelections.default(config),
      inputItems = CommandRunnerSettingsInputItems.build(config),
      uiPresetPreviews = Nil,
      editingPresetName = None
    )

    groups.map(_.id) shouldBe List(
      "settings-app-mode",
      "settings-workspace-layout",
      "settings-document-writing",
      "settings-editor-view",
      "settings-typography",
      "settings-appearance-motion",
      "settings-ui-presets",
      "settings-accessibility",
      "settings-keymap"
    )
    groupById(groups, "settings-workspace-layout").children.map(_.id) shouldBe List(
      "settings-panel-pins"
    )
    // issue #1057: "settings-language" (Current Buffer Language) is gone -- buffer-language switching is a
    // one-shot action, now an ordinary CommandRegistry command (`CommandRunnerOneShotActionsSpec`), not a settings
    // group.
    groupById(groups, "settings-document-writing").children.map(_.id) shouldBe List(
      "settings-navigation",
      "settings-document-defaults",
      "settings-rich-text",
      "settings-spellcheck"
    )
    groupById(groups, "settings-document-defaults").children.map(_.id) shouldBe List(
      "default-document-mode",
      "markdown-view"
    )
    groupById(groups, "settings-spellcheck").children.map(_.id) should contain allOf (
      "spellcheck-enabled",
      "spellcheck-languages",
      "spellcheck-dictionaries"
    )
    groupById(groups, "settings-text-display").children.map(_.id) shouldBe List(
      "line-numbers",
      "gutter",
      "line-wrap",
      "visual-line-navigation",
      "show-word-count",
      "focused-text-body",
      "contextual-toolbar",
      "contextual-toolbar-display"
    )
    groupById(groups, "settings-interface-layout").children.map(_.id) shouldBe List(
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
    groupById(groups, "settings-surface-appearance").children.map(_.id) shouldBe List(
      "background-style",
      "material-preset",
      "post-processing",
      "ui-shadows",
      "blur-radius"
    )
    groupById(groups, "settings-rendering").children.map(_.id) shouldBe
      List("render-fps", "render-damage-granularity")
    groupById(groups, "settings-animation").children.map(_.id) shouldBe List(
      "motion-accessibility",
      "motion-preset",
      "editor-text-transition",
      "panel-open-transition",
      "panel-close-transition",
      "command-runner-transition",
      "command-runner-fade",
      "ui-animation",
      "element-transition-speed-scale",
      "editor-text-speed-scale",
      "command-runner-speed-scale",
      "ui-speed-scale",
      "cursor-speed-scale",
      "window-sitter-enabled",
      "window-sitter-action",
      "window-sitter-frames",
      "window-sitter-active-ticks",
      "window-sitter-fast-active-ticks",
      "window-sitter-fast-threshold-ms"
    )
    groupById(groups, "settings-animation").children.map(_.id) should not contain "render-fps"
    groupById(groups, "settings-animation").children.map(_.id) should not contain "animation-duration"
    groupById(groups, "settings-animation").children.map(_.id) should not contain "animation-steps"
    groupById(groups, "settings-cursor").children.map(_.id) shouldBe List(
      "cursor-mode",
      "cursor-info-bar-title",
      "cursor-info-bar-position",
      "cursor-info-bar-word-count",
      "cursor-info-bar-char-count",
      "cursor-info-bar-reading-time",
      "cursor-info-bar-placement"
    )
    groupById(groups, "settings-text-area").children.map(_.id) shouldBe List(
      "text-area-left",
      "text-area-right",
      "text-area-top",
      "text-area-bottom"
    )
    groupById(groups, "settings-keymap").children.map(_.id) should contain allOf (
      "keymap-global-command_palette",
      "keymap-command-runner-submit"
    )
    groupById(groups, "settings-accessibility").children.map(_.id) shouldBe List("motion-accessibility")
  }

  it should "show advanced motion timing only for a custom baseline" in {
    val config = AppConfig.default.withMotionPreset(com.serenity.config.MotionPreset.Custom)
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = CommandRunnerOptionSelections.default(config),
      inputItems = CommandRunnerSettingsInputItems.build(config),
      uiPresetPreviews = Nil,
      editingPresetName = None
    )

    groupById(groups, "settings-animation").children.map(_.id) should contain allOf (
      "animation-duration",
      "animation-steps"
    )
  }

  it should "apply preset edit context while deriving preset groups" in {
    val config = AppConfig.default
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = CommandRunnerOptionSelections.default(config) + ("ui-preset-custom" -> 1),
      inputItems = CommandRunnerSettingsInputItems.build(config),
      uiPresetPreviews = List(UiPreset.Preview("Drafting", "Saved workspace setup"), UiPreset.Preview("Review", "")),
      editingPresetName = None
    )

    val editGroup = groupById(groups, "settings-preset-edit")
    editGroup.children.map(_.id).take(2) shouldBe List("settings-preset-name", "settings-preset-actions")
    groupById(groups, "settings-preset-name").children.map(_.id) shouldBe List("ui-preset-rename")
    groupById(groups, "settings-preset-actions").children.map(_.id) shouldBe List(
      "ui-preset-apply",
      "ui-preset-overwrite",
      "ui-preset-duplicate",
      "ui-preset-delete",
      "ui-preset-reset"
    )
    val presetInputs = descendants(editGroup).collect {
      case item: CommandSurfaceItem.InputItem if item.id.startsWith("ui-preset-") => item.id -> item.currentValue
    }.toMap

    editGroup.label shouldBe "Edit Preset: Review"
    presetInputs("ui-preset-overwrite") shouldBe "Review"
    presetInputs("ui-preset-apply") shouldBe "Review"
    presetInputs("ui-preset-duplicate") shouldBe "Review -> "
    presetInputs("ui-preset-rename") shouldBe "Review -> "
    presetInputs("ui-preset-delete") shouldBe "Review"
    presetInputs("ui-preset-reset") shouldBe "Review"
  }

  it should "group preset animation controls by element surface" in {
    val config = AppConfig.default
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = CommandRunnerOptionSelections.default(config),
      inputItems = CommandRunnerSettingsInputItems.build(config),
      uiPresetPreviews = Nil,
      editingPresetName = None
    )

    groupById(groups, "settings-preset-animations").children.map(_.id) shouldBe List(
      "settings-preset-cursor-motion",
      "settings-preset-text-entry-motion",
      "settings-preset-ui-surface-motion"
    )
    groupById(groups, "settings-preset-cursor-motion").children.map(_.id) shouldBe List(
      "cursor-mode",
      "cursor-speed-scale",
      "cursor-info-bar-title",
      "cursor-info-bar-position",
      "cursor-info-bar-word-count",
      "cursor-info-bar-char-count",
      "cursor-info-bar-reading-time",
      "cursor-info-bar-placement"
    )
    groupById(groups, "settings-preset-text-entry-motion").children.map(_.id) should contain allOf (
      "editor-text-transition",
      "editor-text-speed-scale",
      "element-transition-speed-scale"
    )
    groupById(groups, "settings-preset-ui-surface-motion").children.map(_.id) should contain allOf (
      "motion-preset",
      "panel-open-transition",
      "panel-close-transition",
      "command-runner-transition",
      "command-runner-fade",
      "command-runner-speed-scale",
      "ui-animation",
      "ui-speed-scale"
    )
    groupById(groups, "settings-preset-ui-surface-motion").children.map(_.id) should not contain "render-fps"
  }

  it should "group preset font controls by editor and interface surface" in {
    val config = AppConfig.default
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = CommandRunnerOptionSelections.default(config),
      inputItems = CommandRunnerSettingsInputItems.build(config),
      uiPresetPreviews = Nil,
      editingPresetName = None
    )

    groupById(groups, "settings-preset-fonts").children.map(_.id) shouldBe List(
      "settings-preset-editor-typography",
      "settings-preset-code-typography",
      "settings-preset-ui-typography"
    )
    groupById(groups, "settings-preset-editor-typography").children.map(_.id) shouldBe List(
      "text-font",
      "text-ligatures",
      "text-font-size"
    )
    groupById(groups, "settings-preset-code-typography").children.map(_.id) shouldBe List(
      "code-font",
      "code-ligatures",
      "code-font-size"
    )
    groupById(groups, "settings-preset-ui-typography").children.map(_.id) shouldBe List(
      "ui-font",
      "ui-ligatures",
      "ui-font-size"
    )
  }

  // Windows Desktop Publish release-blocker: settings-tree search walks these font-family groups regardless of
  // which CommandRegistry a caller scoped the palette to, so a test that stubs a deterministic FontFamilyCatalog
  // (rather than depending on `FontLoader`'s live, machine-specific installed-font enumeration) is the only way to
  // keep search-term assertions immune to whatever fonts happen to be installed on the machine running the test.
  it should "build code/text/ui font groups from the supplied FontFamilyCatalog, not the live system fonts" in {
    val config = AppConfig.default
    val stubCatalog = FontLoader.FontFamilyCatalog(
      monospace = List("Stub Mono One", "Stub Mono Two"),
      text = List("Stub Text One", "Stub Text Two"),
      ui = List("Stub Ui One", "Stub Ui Two")
    )
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = CommandRunnerOptionSelections.default(config),
      inputItems = CommandRunnerSettingsInputItems.build(config),
      uiPresetPreviews = Nil,
      editingPresetName = None,
      fontFamilies = stubCatalog
    )

    def familyLabels(groupId: String): List[String] =
      groupById(groups, groupId).children.collect { case CommandSurfaceItem.CommandItem(command) => command.label }

    familyLabels("code-font") shouldBe stubCatalog.monospace
    familyLabels("text-font") shouldBe stubCatalog.text
    familyLabels("ui-font") shouldBe stubCatalog.ui
  }

  it should "group preset theme controls by theme and surface material" in {
    val config = AppConfig.default
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = CommandRunnerOptionSelections.default(config),
      inputItems = CommandRunnerSettingsInputItems.build(config),
      uiPresetPreviews = Nil,
      editingPresetName = None
    )

    // issue #1057: "settings-preset-theme-selection" (Theme Chooser/Creator/Toggle/Reload) is gone -- those are
    // one-shot actions with no preset-scoped value of their own, now ordinary CommandRegistry commands.
    groupById(groups, "settings-preset-theme").children.map(_.id) shouldBe List(
      "settings-preset-surface-material"
    )
    groupById(groups, "settings-preset-surface-material").children.map(_.id) shouldBe List(
      "background-style",
      "material-preset",
      "post-processing",
      "ui-shadows",
      "blur-radius"
    )
  }

  it should "group preset document defaults by document, preview, and spelling" in {
    val config = AppConfig.default
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = CommandRunnerOptionSelections.default(config),
      inputItems = CommandRunnerSettingsInputItems.build(config),
      uiPresetPreviews = Nil,
      editingPresetName = None
    )

    groupById(groups, "settings-preset-document-defaults").children.map(_.id) shouldBe List(
      "settings-preset-new-documents",
      "settings-preset-markdown-preview",
      "settings-preset-spelling"
    )
    groupById(groups, "settings-preset-new-documents").children.map(_.id) shouldBe List("default-document-mode")
    groupById(groups, "settings-preset-markdown-preview").children.map(_.id) shouldBe List("markdown-view")
    groupById(groups, "settings-preset-spelling").children.map(_.id) shouldBe List(
      "spellcheck-enabled",
      "spellcheck-languages",
      "spellcheck-dictionaries",
      "spellcheck-words"
    )
  }

  "CommandRunnerSettingsGroups.build with isTuiMode = true" should
    "annotate the typography and post-processing groups as inert, leaving every other hint untouched" in {
      val config = AppConfig.default.withShowAllSettingsRegardlessOfMode(true)
      val guiGroups = CommandRunnerSettingsGroups.build(
        optionSelections = CommandRunnerOptionSelections.default(config),
        inputItems = CommandRunnerSettingsInputItems.build(config),
        uiPresetPreviews = Nil,
        editingPresetName = None,
        isTuiMode = false
      )
      val tuiGroups = CommandRunnerSettingsGroups.build(
        optionSelections = CommandRunnerOptionSelections.default(config),
        inputItems = CommandRunnerSettingsInputItems.build(config),
        uiPresetPreviews = Nil,
        editingPresetName = None,
        isTuiMode = true
      )

      def hintOf(groups: List[CommandSurfaceItem.GroupItem], id: String): Option[String] =
        groupById(groups, id).hint

      def optionHintOf(groups: List[CommandSurfaceItem.GroupItem], groupId: String, itemId: String): Option[String] =
        groupById(groups, groupId).children.collectFirst {
          case item: CommandSurfaceItem.OptionItem if item.id == itemId => item.hint
        }.flatten

      // The annotation leads the hint rather than trailing it: the settings surface's hint column is a fixed share of
      // the panel width and elides from the right, so a trailing annotation was cut off before it could be read.
      hintOf(tuiGroups, "settings-typography") shouldBe Some(
        "Inert in TUI mode -- Typefaces for prose, code, and interface"
      )
      hintOf(tuiGroups, "settings-code-font") shouldBe Some("Inert in TUI mode -- Family, size, ligatures")
      hintOf(tuiGroups, "settings-prose-font") shouldBe Some("Inert in TUI mode -- Family, size, ligatures")
      hintOf(tuiGroups, "settings-ui-font") shouldBe Some("Inert in TUI mode -- Family, size, ligatures")
      optionHintOf(tuiGroups, "settings-surface-appearance", "post-processing").exists(
        _.startsWith("Inert in TUI mode --")
      ) shouldBe true

      hintOf(guiGroups, "settings-typography") shouldBe Some("Typefaces for prose, code, and interface")
      hintOf(guiGroups, "settings-code-font") shouldBe Some("Family, size, ligatures")
      optionHintOf(guiGroups, "settings-surface-appearance", "post-processing").exists(
        _.startsWith("Inert in TUI mode --")
      ) shouldBe false

      // Unrelated groups are untouched by the flag.
      hintOf(tuiGroups, "settings-cursor") shouldBe hintOf(guiGroups, "settings-cursor")
      hintOf(tuiGroups, "settings-rendering") shouldBe hintOf(guiGroups, "settings-rendering")
      tuiGroups.map(_.id) shouldBe guiGroups.map(_.id)
    }

  "CommandRunnerSettingsGroups.build with an app mode" should
    "hide document-writing and prose typography settings by default in code mode" in {
      // AppConfig.default is already code mode with the override off.
      val config = AppConfig.default
      val groups = CommandRunnerSettingsGroups.build(
        optionSelections = CommandRunnerOptionSelections.default(config),
        inputItems = CommandRunnerSettingsInputItems.build(config),
        uiPresetPreviews = Nil,
        editingPresetName = None
      )

      groups.map(_.id) should not contain "settings-document-writing"
      groupById(groups, "settings-typography").children.map(_.id) shouldBe List(
        "settings-code-font",
        "settings-ui-font"
      )
    }

  it should "hide code typography settings by default in prose mode, while keeping document writing" in {
    val config = AppConfig.default.withAppMode(AppMode.Prose)
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = CommandRunnerOptionSelections.default(config),
      inputItems = CommandRunnerSettingsInputItems.build(config),
      uiPresetPreviews = Nil,
      editingPresetName = None
    )

    groups.map(_.id) should contain("settings-document-writing")
    groupById(groups, "settings-typography").children.map(_.id) shouldBe List(
      "settings-prose-font",
      "settings-ui-font"
    )
  }

  it should "show every group regardless of mode once the override is enabled" in {
    val config = AppConfig.default.withShowAllSettingsRegardlessOfMode(true)
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = CommandRunnerOptionSelections.default(config),
      inputItems = CommandRunnerSettingsInputItems.build(config),
      uiPresetPreviews = Nil,
      editingPresetName = None
    )

    groups.map(_.id) should contain("settings-document-writing")
    groupById(groups, "settings-typography").children.map(_.id) shouldBe List(
      "settings-prose-font",
      "settings-code-font",
      "settings-ui-font"
    )
  }

  it should "always surface the app mode group itself, even while filtering everything else" in {
    val config = AppConfig.default
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = CommandRunnerOptionSelections.default(config),
      inputItems = CommandRunnerSettingsInputItems.build(config),
      uiPresetPreviews = Nil,
      editingPresetName = None
    )

    groupById(groups, "settings-app-mode").children.map(_.id) shouldBe List("app-mode", "settings-show-all")
  }
