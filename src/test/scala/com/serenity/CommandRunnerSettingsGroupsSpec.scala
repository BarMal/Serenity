package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.rope.Balance
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.PanelPosition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerSettingsGroupsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def groupById(items: List[CommandSurfaceItem], id: String): CommandSurfaceItem.GroupItem =
    items
      .collectFirst { case group: CommandSurfaceItem.GroupItem if group.id == id => group }
      .getOrElse(fail(s"missing group $id"))

  private def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
    group.children.flatMap {
      case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
      case child                               => List(child)
    }

  private def groupByIdRecursive(
    groups: List[CommandSurfaceItem.GroupItem],
    id: String
  ): CommandSurfaceItem.GroupItem =
    (groups ++ groups.flatMap(group =>
      descendants(group).collect { case child: CommandSurfaceItem.GroupItem => child }
    ))
      .find(_.id == id)
      .getOrElse(fail(s"missing group $id"))

  "CommandRunner state" should "surface visual appearance settings as an expandable group in settings browsing" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)

    val appearanceGroup = groupByIdRecursive(runner.settingsGroups, "settings-surface-appearance")

    appearanceGroup.label shouldBe "Surface Appearance"
    appearanceGroup.children.map(_.id) shouldBe List(
      "background-style",
      "material-preset",
      "post-processing",
      "ui-shadows",
      "blur-radius"
    )
  }

  // issue #931: category tabs are retired -- browsing settings groups with no search now only happens via the
  // dedicated Settings surface (`.openSettings`), not by switching the palette's category. `visibleItems` still
  // routes to `settingsSurfaceItems` once `isSettingsSurface` is true, so this fixture change is the only one
  // needed.
  it should "group related settings into expandable submenu rows" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    // "Show all settings" is on: this test inspects the full taxonomy, mode filtering (issue #1297) aside.
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withShowAllSettingsRegardlessOfMode(true))
      .openSettings

    val groupItems = runner.visibleItems.collect { case group: CommandSurfaceItem.GroupItem => group }

    groupItems.map(_.id) shouldBe List(
      "settings-app-mode",
      "settings-workspace-layout",
      "settings-document-writing",
      "settings-editor-view",
      "settings-typography",
      "settings-appearance-motion",
      "settings-ui-presets",
      "settings-accessibility",
      "settings-performance",
      "settings-keymap"
    )
    def group(id: String): CommandSurfaceItem.GroupItem =
      groupItems.find(_.id == id).getOrElse(fail(s"missing group $id"))
    def nestedGroup(id: String): CommandSurfaceItem.GroupItem =
      groupItems
        .flatMap(item => descendants(item).collect { case group: CommandSurfaceItem.GroupItem => group })
        .find(_.id == id)
        .getOrElse(fail(s"missing nested group $id"))

    val workspaceLayoutGroup = group("settings-workspace-layout")
    workspaceLayoutGroup.label shouldBe "Panels & Workspace"
    workspaceLayoutGroup.children.map(_.id) shouldBe List(
      "settings-panel-pins"
    )
    val panelPins = groupById(workspaceLayoutGroup.children, "settings-panel-pins")
    panelPins.label shouldBe "Panel Pins"
    panelPins.children.map(_.id) shouldBe List(
      "panel-explorer-pin",
      "panel-outline-pin",
      "panel-comments-pin",
      "panel-diagnostics-pin",
      "panel-markdown-preview-pin"
    )
    // issue #1057: "settings-language" (Current Buffer Language) is gone -- see CommandRunnerOneShotActionsSpec.
    group("settings-document-writing").children.map(_.id) shouldBe List(
      "settings-navigation",
      "settings-document-defaults",
      "settings-rich-text",
      "settings-spellcheck"
    )
    group("settings-editor-view").children.map(_.id) shouldBe List(
      "settings-text-display",
      "settings-text-area",
      "settings-text-scale"
    )
    group("settings-typography").children.map(_.id) shouldBe List(
      "settings-prose-font",
      "settings-code-font",
      "settings-ui-font"
    )
    group("settings-appearance-motion").children.map(_.id) shouldBe List(
      "settings-cursor",
      "settings-surface-appearance",
      "settings-interface-layout",
      "settings-rendering",
      "settings-animation"
    )
    nestedGroup("settings-cursor").label shouldBe "Cursor"
    nestedGroup("settings-cursor").children.map(_.id) should contain allOf (
      "cursor-mode",
      "cursor-info-bar-title",
      "cursor-info-bar-placement"
    )
    nestedGroup("settings-surface-appearance").label shouldBe "Surface Appearance"
    nestedGroup("settings-surface-appearance").children.map(_.id) shouldBe List(
      "background-style",
      "material-preset",
      "post-processing",
      "ui-shadows",
      "blur-radius"
    )
    nestedGroup("settings-interface-layout").label shouldBe "Interface Layout"
    nestedGroup("settings-interface-layout").children.map(_.id) shouldBe List(
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
    nestedGroup("settings-rendering").children.map(_.id) shouldBe
      List("render-fps", "render-damage-granularity")
    nestedGroup("settings-animation").children.map(_.id) should contain allOf (
      "motion-preset",
      "editor-text-transition",
      "panel-open-transition",
      "panel-close-transition",
      "command-runner-transition",
      "command-runner-fade",
      "ui-animation"
    )
    group("settings-ui-presets").label shouldBe "UI Presets"
    group("settings-ui-presets").children.map(_.id) shouldBe List(
      "settings-preset-select",
      "settings-preset-create",
      "settings-preset-edit"
    )
    nestedGroup("settings-text-display").label shouldBe "Text Display"
    nestedGroup("settings-text-display").children.map(_.id) shouldBe List(
      "line-numbers",
      "gutter",
      "line-wrap",
      "visual-line-navigation",
      "typewriter-scrolling",
      "show-word-count",
      "focused-text-body",
      "contextual-toolbar",
      "contextual-toolbar-display"
    )
    nestedGroup("settings-text-area").label shouldBe "Text Area"
    nestedGroup("settings-text-area").children.map(_.id) shouldBe List(
      "text-area-left",
      "text-area-right",
      "text-area-top",
      "text-area-bottom"
    )
    nestedGroup("settings-code-font").label shouldBe "Code Font"
    nestedGroup("settings-code-font").children.map(_.id) should contain allOf (
      "code-font",
      "code-ligatures",
      "code-font-size"
    )
    nestedGroup("settings-prose-font").label shouldBe "Prose Font"
    nestedGroup("settings-prose-font").children.map(_.id) should contain allOf (
      "text-font",
      "text-ligatures",
      "text-font-size"
    )
    nestedGroup("settings-rich-text").label shouldBe "Rich Text"
    nestedGroup("settings-rich-text").children.map(_.id) should contain allOf (
      "rich-text-font-family",
      "rich-text-font-size",
      "rich-text-color"
    )
    nestedGroup("settings-ui-font").label shouldBe "UI Font"
    nestedGroup("settings-ui-font").children.map(_.id) should contain allOf ("ui-font", "ui-ligatures", "ui-font-size")
    nestedGroup("settings-text-scale").label shouldBe "Text Scale"
    nestedGroup("settings-text-scale").children.map(_.id) should contain allOf ("text-scale-mode", "text-scale")
    nestedGroup("settings-code-font").children
      .collectFirst { case group: CommandSurfaceItem.GroupItem if group.id == "code-font" => group }
      .map(_.children.map(_.id)) should not be empty
    nestedGroup("settings-spellcheck").label shouldBe "Spell Check"
    nestedGroup("settings-spellcheck").children.map(_.id) should contain allOf (
      "spellcheck-enabled",
      "spellcheck-languages",
      "spellcheck-dictionaries",
      "spellcheck-words"
    )
    group("settings-keymap").label shouldBe "Keymap"
    group("settings-keymap").children.map(_.id) should contain allOf (
      "keymap-global-command_palette",
      "keymap-command-runner-submit",
      "keymap-modal-dismiss"
    )
    nestedGroup("settings-preset-markdown-preview").label shouldBe "Markdown Preview"
    nestedGroup("settings-preset-markdown-preview").children.map(_.id) should contain("markdown-view")
    nestedGroup("settings-document-defaults").label shouldBe "Document Defaults"
    nestedGroup("settings-document-defaults").children.map(_.id) should contain allOf (
      "default-document-mode",
      "markdown-view"
    )
  }

  // issue #931: category tabs are retired -- see the "group related settings" test above for why this fixture uses
  // `.openSettings` now.
  it should "surface workspace panel pins as dynamic option rows" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .copy(optionSelections = Map("panel-outline-pin" -> 2, "panel-diagnostics-pin" -> 4))
      .openSettings

    val workspace = runner.visibleItems
      .collectFirst { case group: CommandSurfaceItem.GroupItem if group.id == "settings-workspace-layout" => group }
      .getOrElse(fail("Expected workspace layout settings group"))

    val panelPins  = groupById(workspace.children, "settings-panel-pins")
    val pinOptions = panelPins.children.collect { case option: CommandSurfaceItem.OptionItem => option }

    pinOptions.map(_.id) shouldBe List(
      "panel-explorer-pin",
      "panel-outline-pin",
      "panel-comments-pin",
      "panel-diagnostics-pin",
      "panel-markdown-preview-pin"
    )
    pinOptions.foreach(_.options.map(_.label) shouldBe List("Off", "Top", "Right", "Bottom", "Left"))
    pinOptions.find(_.id == "panel-outline-pin").map(_.selectedOption) shouldBe Some("Right")
    pinOptions.find(_.id == "panel-outline-pin").flatMap(_.selectedIntent) shouldBe
      Some(CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Outline, Some(PanelPosition.Right))))
    pinOptions.find(_.id == "panel-diagnostics-pin").flatMap(_.selectedIntent) shouldBe
      Some(CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Diagnostics, Some(PanelPosition.Left))))
  }

  it should "show current text display states as settings options" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val config = AppConfig.default
      .copy(surfaceConfig =
        AppConfig.default.surfaceConfig.copy(
          showLineNumbers = false,
          showGutter = false,
          wordWrapEnabled = false,
          contextualToolbarEnabled = false,
          contextualToolbarDisplayMode = ToolbarDisplayMode.TextOnly
        )
      )
    val runner = CommandRunner.empty
      .activate(registry, config)

    val textDisplay = groupByIdRecursive(runner.settingsGroups, "settings-text-display")
    val options     = textDisplay.children.collect { case option: CommandSurfaceItem.OptionItem => option }

    options.map(option => option.id -> option.selectedOption) shouldBe List(
      "line-numbers"               -> "Off",
      "gutter"                     -> "Off",
      "line-wrap"                  -> "Off",
      "visual-line-navigation"     -> "On",
      "typewriter-scrolling"       -> "Off",
      "show-word-count"            -> "Off",
      "focused-text-body"          -> "Off",
      "contextual-toolbar"         -> "Off",
      "contextual-toolbar-display" -> "Text Only"
    )
    options.flatMap(_.selectedIntent) shouldBe List(
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetLineNumbers(false))),
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetGutter(false))),
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWordWrap(false))),
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetVisualLineCursorNavigation(true))),
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetTypewriterScrolling(false))),
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetShowWordCount(false))),
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetFocusedTextBody(false))),
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetContextualToolbarEnabled(false))),
      CommandIntent.Settings(
        SettingsIntent.PanelChrome(PanelChromeIntent.SetContextualToolbarDisplayMode(ToolbarDisplayMode.TextOnly))
      )
    )
  }

  it should "surface command runner visible rows as a typed interface setting" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withCommandRunnerVisibleRows(Some(9)))

    val interfaceGroup = groupByIdRecursive(runner.settingsGroups, "settings-interface-layout")
    val input = interfaceGroup.children
      .collectFirst { case item: CommandSurfaceItem.InputItem if item.id == "command-runner-visible-rows" => item }
      .getOrElse(fail("missing command runner visible rows input"))

    input.currentValue shouldBe "9"
    input.parse("12") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerVisibleRows(Some(12))))
    )
    input.parse("auto") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerVisibleRows(None)))
    )
    input.parse("0") shouldBe None
  }

  it should "surface render FPS target as a rendering setting" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withRenderFpsTarget(RenderFpsTarget.Fps120))

    val renderingGroup = groupByIdRecursive(runner.settingsGroups, "settings-rendering")
    val option = renderingGroup.children
      .collectFirst { case item: CommandSurfaceItem.OptionItem if item.id == "render-fps" => item }
      .getOrElse(fail("missing render FPS option"))

    option.selectedOption shouldBe "120 FPS"
    option.options.map(_.label) shouldBe List("30 FPS", "60 FPS", "90 FPS", "120 FPS", "Uncapped")
    option.selectedIntent shouldBe Some(
      CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetRenderFpsTarget(RenderFpsTarget.Fps120)))
    )
  }

  it should "reuse derived settings groups within a command runner state" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)

    runner.settingsGroups shouldBe theSameInstanceAs(runner.settingsGroups)
  }

  it should "reuse derived visible items within a command runner state" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)

    runner.visibleItems shouldBe theSameInstanceAs(runner.visibleItems)
  }

  it should "surface default document mode as a typed document defaults setting" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(
        registry,
        AppConfig.default
          .withDefaultDocumentMode(DefaultDocumentMode.RichText)
          .withShowAllSettingsRegardlessOfMode(true)
      )

    val documentDefaultsGroup = groupByIdRecursive(runner.settingsGroups, "settings-document-defaults")

    val documentMode =
      documentDefaultsGroup.children
        .collectFirst {
          case item: CommandSurfaceItem.OptionItem if item.id == "default-document-mode" =>
            item
        }
        .getOrElse(fail("missing default document mode option"))

    documentMode.selectedOption shouldBe "Rich Text"
    documentMode.options.map(_.label) shouldBe List("Plain Text", "Markdown", "Rich Text")
    documentMode.options.map(_.intent) shouldBe List(
      CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.PlainText)),
      CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown)),
      CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.RichText))
    )
  }

  it should "surface rich text inline style inputs in settings" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    // A deterministic catalog, not the host's installed fonts (see `CommandRunnerSettingsGroups.build`'s own doc).
    val fontFamilies = FontLoader.FontFamilyCatalog(monospace = List("Mono"), text = List("Serif", "Sans"), ui = Nil)
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withShowAllSettingsRegardlessOfMode(true))
      .copy(fontFamilies = fontFamilies)

    val richTextGroup = groupByIdRecursive(runner.settingsGroups, "settings-rich-text")

    // issue #1060: font family is a picker now, matching code/prose/UI font family, not typed free text.
    richTextGroup.children.map(_.id) shouldBe List("rich-text-font-family", "rich-text-font-size", "rich-text-color")
    val fontFamilyGroup = richTextGroup.children
      .collectFirst { case item: CommandSurfaceItem.GroupItem if item.id == "rich-text-font-family" => item }
      .getOrElse(fail("missing rich text font family picker"))
    fontFamilyGroup.label shouldBe "Selection Font Family"
    fontFamilyGroup.children.collect { case CommandSurfaceItem.CommandItem(command) => command.intent } should
      contain(CommandIntent.RichText(RichTextIntent.SetRichTextFontFamily("Serif")))

    val inputs = richTextGroup.children.collect { case item: CommandSurfaceItem.InputItem => item }
    inputs.map(_.id) shouldBe List("rich-text-font-size", "rich-text-color")
    // issue #1060: 8.0-48.0, matching code/prose/UI font size -- no longer a 1.0-144.0 outlier.
    inputs.head.hint shouldBe "Points (8.0-48.0)"
    inputs.head.parse("18") shouldBe Some(CommandIntent.RichText(RichTextIntent.SetRichTextFontSize(18.0f)))
    inputs.head.parse("144") shouldBe None
    inputs(1).parse("#336699") shouldBe Some(CommandIntent.RichText(RichTextIntent.SetRichTextColor("#336699")))
    inputs(1).parse("not-a-colour") shouldBe None
  }

  it should "surface spell-check settings as typed controls" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(
        registry,
        AppConfig.default
          .withSpellCheck(
            SpellCheckConfig(
              enabled = true,
              languages = List("en", "fr"),
              dictionaryPaths = List("C:\\Dictionaries\\en_US.dic"),
              additionalWords = List("serenity")
            )
          )
          .withShowAllSettingsRegardlessOfMode(true)
      )

    val spellGroup = groupByIdRecursive(runner.settingsGroups, "settings-spellcheck")

    val enabledOption =
      spellGroup.children
        .collectFirst {
          case item: CommandSurfaceItem.OptionItem if item.id == "spellcheck-enabled" =>
            item
        }
        .getOrElse(fail("missing spell-check enabled option"))
    val inputs = spellGroup.children.collect { case item: CommandSurfaceItem.InputItem => item }

    enabledOption.selectedOption shouldBe "On"
    enabledOption.selectedIntent shouldBe Some(
      CommandIntent.Settings(SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckEnabled(true)))
    )
    inputs.map(_.id) shouldBe List("spellcheck-languages", "spellcheck-dictionaries", "spellcheck-words")
    inputs.head.currentValue shouldBe "en,fr"
    inputs.head.parse("fr,en") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckLanguages(List("fr", "en"))))
    )
    inputs(1).currentValue shouldBe "C:\\Dictionaries\\en_US.dic"
    inputs(1).parse("C:\\Dictionaries\\en_US.dic,/usr/share/hunspell/fr.dic") shouldBe Some(
      CommandIntent.Settings(
        SettingsIntent.SpellCheck(
          SpellCheckIntent.SetSpellCheckDictionaryPaths(
            List("C:\\Dictionaries\\en_US.dic", "/usr/share/hunspell/fr.dic")
          )
        )
      )
    )
    inputs(2).currentValue shouldBe "serenity"
    inputs(2).parse("Serenity,caf\u00e9") shouldBe Some(
      CommandIntent.Settings(
        SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckWords(List("serenity", "caf\u00e9")))
      )
    )
  }
