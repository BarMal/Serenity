package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.config.AppConfigOps.*
import com.serenity.rope.Balance
import com.serenity.ui.fonts.FontLoader
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

  // issue #931: category tabs are retired -- browsing settings groups with no search now only happens via the
  // dedicated Settings surface (`.openSettings`), not by switching the palette's category. `visibleItems` still
  // routes to `settingsSurfaceItems` once `isSettingsSurface` is true, so this fixture change is the only one
  // needed.
  "CommandRunner state" should "group related settings into expandable submenu rows" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    // "Show all settings" is on: this test inspects the full taxonomy, mode filtering (issue #1297) aside.
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withShowAllSettingsRegardlessOfMode(true))
      .openSettings

    val groupItems = runner.visibleItems.collect { case group: CommandSurfaceItem.GroupItem => group }

    groupItems.map(_.id) shouldBe List(
      "settings-workspace",
      "settings-editor",
      "settings-typography",
      "settings-look",
      "settings-language-tools",
      "settings-keymap"
    )
    def group(id: String): CommandSurfaceItem.GroupItem =
      groupItems.find(_.id == id).getOrElse(fail(s"missing group $id"))
    def nestedGroup(id: String): CommandSurfaceItem.GroupItem =
      groupItems
        .flatMap(item => descendants(item).collect { case group: CommandSurfaceItem.GroupItem => group })
        .find(_.id == id)
        .getOrElse(fail(s"missing nested group $id"))

    group("settings-workspace").label shouldBe "Workspace"
    group("settings-workspace").children.map(_.id) shouldBe List(
      "app-mode",
      "settings-show-all",
      "settings-workspace-layout",
      "settings-ui-presets"
    )
    val workspaceLayoutGroup = nestedGroup("settings-workspace-layout")
    workspaceLayoutGroup.label shouldBe "Panels"
    workspaceLayoutGroup.children.map(_.id) shouldBe
      List("arrange-panels", "panel-escape-code", "panel-escape-prose")
    nestedGroup("settings-ui-presets").children.map(_.id) shouldBe List(
      "settings-preset-select",
      "settings-preset-create",
      "settings-preset-edit"
    )
    group("settings-editor").children.map(_.id) shouldBe List(
      "settings-text-display",
      "settings-status-line",
      "settings-text-area",
      "settings-document-defaults",
      "settings-saving",
      "settings-navigation"
    )
    nestedGroup("settings-navigation").label shouldBe "Comments"
    group("settings-typography").children.map(_.id) shouldBe List(
      "settings-prose-font",
      "settings-code-font",
      "settings-ui-font",
      "settings-text-scale",
      "settings-rich-text"
    )
    group("settings-look").label shouldBe "Look"
    group("settings-look").children.map(_.id) shouldBe List(
      "theme",
      "settings-follow-system",
      "settings-interface-layout",
      "settings-cursor",
      "settings-look-advanced"
    )
    nestedGroup("settings-cursor").label shouldBe "Cursor"
    nestedGroup("settings-cursor").children.map(_.id) shouldBe List("cursor-mode")
    nestedGroup("settings-status-line").label shouldBe "Status Line"
    nestedGroup("settings-status-line").children.map(_.id) should contain allOf ("status-placement", "status-title")
    // issue #1046: command-runner visible-rows/item-gap-rows/cursor-gap-rows are no longer separate rows here --
    // Interface Density above is the one control governing all three now.
    nestedGroup("settings-interface-layout").label shouldBe "Interface Layout"
    nestedGroup("settings-interface-layout").children.map(_.id) shouldBe List(
      "interface-density",
      "window-chrome",
      "command-runner-key-hints"
    )
    // Blur, spacing, corners, outlines, render cadence and decorative extras are one Advanced leaf, not five groups.
    nestedGroup("settings-look-advanced").label shouldBe "Advanced"
    nestedGroup("settings-look-advanced").children.map(_.id) shouldBe List(
      "ui-element-gap",
      "ui-outline-thickness",
      "render-fps",
      "render-damage-granularity"
    )
    group("settings-language-tools").children.map(_.id) shouldBe List("buffer-language", "settings-spellcheck")
    nestedGroup("settings-text-display").label shouldBe "Text Display"
    nestedGroup("settings-text-display").children.map(_.id) shouldBe List(
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
    group("settings-keymap").label shouldBe "Keys"
    group("settings-keymap").children.map(_.id) shouldBe List(
      "settings-keymap-global",
      "settings-keymap-editor",
      "settings-keymap-command-runner",
      "settings-keymap-dialogs",
      "settings-keymap-panels",
      "settings-keymap-peek"
    )
    nestedGroup("settings-keymap-global").children.map(_.id) shouldBe List(
      "settings-keymap-global-navigation",
      "settings-keymap-global-files",
      "settings-keymap-global-editing",
      "settings-keymap-global-view"
    )
    nestedGroup("settings-keymap-global-navigation").children.map(_.id).headOption shouldBe
      Some("keymap-global-command_palette")
    nestedGroup("settings-keymap-command-runner").children.map(_.id) should contain("keymap-command-runner-submit")
    nestedGroup("settings-keymap-dialogs").children.map(_.id) should contain("keymap-modal-dismiss")
    nestedGroup("settings-keymap-panels").children.map(_.id) should contain("keymap-panel-navigate_up")
    nestedGroup("settings-saving").label shouldBe "Saving"
    nestedGroup("settings-saving").children.map(_.id) shouldBe List("auto-save-mode", "auto-save-delay")
    nestedGroup("settings-document-defaults").label shouldBe "Document Defaults"
    nestedGroup("settings-document-defaults").children.map(_.id) should contain allOf (
      "default-document-mode",
      "markdown-view",
      "drop-caps-enabled"
    )
  }

  // issue #931: category tabs are retired -- see the "group related settings" test above for why this fixture uses
  // `.openSettings` now.
  it should "open the Arrange Panels list from the Panels group, in either mode" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    def panelsGroupIntents(selections: Map[String, Int]) =
      val runner =
        CommandRunner.empty.activate(registry, AppConfig.default).copy(optionSelections = selections).openSettings
      groupByIdRecursive(runner.settingsGroups, "settings-workspace-layout").children.collect {
        case CommandSurfaceItem.CommandItem(command, _) => command.intent
      }

    panelsGroupIntents(Map.empty) shouldBe List(CommandIntent.View(ViewIntent.ArrangePanels))
    panelsGroupIntents(Map("app-mode" -> 1)) shouldBe List(CommandIntent.View(ViewIntent.ArrangePanels))
  }

  it should "show current text display states as settings options" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val config = AppConfig.default
      .copy(surfaceConfig =
        AppConfig.default.surfaceConfig.copy(
          showLineNumbers = false,
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
      "line-number-side"           -> "Left",
      "line-wrap"                  -> "Off",
      "visual-line-navigation"     -> "On",
      "typewriter-scrolling"       -> "Off",
      "focused-text-body"          -> "Off",
      "contextual-toolbar"         -> "Off",
      "contextual-toolbar-display" -> "Text Only"
    )
    options.flatMap(_.selectedIntent) shouldBe List(
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetLineNumbers(false))),
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetLineNumberSide(LineNumberSide.Left))),
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetWordWrap(false))),
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetVisualLineCursorNavigation(true))),
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetTypewriterScrolling(false))),
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetFocusedTextBody(false))),
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetContextualToolbarEnabled(false))),
      CommandIntent.Settings(
        SettingsIntent.TextDisplay(TextDisplayIntent.SetContextualToolbarDisplayMode(ToolbarDisplayMode.TextOnly))
      )
    )
  }

  // Multi-column e-reader layout (issue #1338, Phase 2 / slice 4): the count and gap steppers are always visible in
  // the text-display group, whether or not column mode itself is on -- unlike a setting gated behind a toggle, there
  // is no dependency here to hide behind.
  it should "surface column count and gap as always-visible input rows regardless of column mode" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val columnModeOff     = CommandRunner.empty.activate(registry, AppConfig.default.withColumnMode(false))
    val columnModeOn =
      CommandRunner.empty.activate(registry, AppConfig.default.withColumnMode(true).withColumnCount(Some(3)))

    val textDisplayOff = groupByIdRecursive(columnModeOff.settingsGroups, "settings-text-display")
    val textDisplayOn  = groupByIdRecursive(columnModeOn.settingsGroups, "settings-text-display")

    textDisplayOff.children.map(_.id) should contain allOf ("columns", "column-gap")
    textDisplayOn.children.map(_.id) should contain allOf ("columns", "column-gap")

    val columnsItem = textDisplayOn.children
      .collectFirst { case item: CommandSurfaceItem.InputItem if item.id == "columns" => item }
      .getOrElse(fail("missing columns input item"))
    columnsItem.currentValue shouldBe "3"
    columnsItem.parse("auto") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetColumnCount(None)))
    )
    columnsItem.parse("5") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetColumnCount(Some(5))))
    )
    columnsItem.steppedIntent(1) shouldBe Some(
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetColumnCount(Some(4))))
    )

    val autoColumnsItem = groupByIdRecursive(columnModeOff.settingsGroups, "settings-text-display").children
      .collectFirst { case item: CommandSurfaceItem.InputItem if item.id == "columns" => item }
      .getOrElse(fail("missing columns input item"))
    autoColumnsItem.currentValue shouldBe "auto"
    autoColumnsItem.steppedIntent(1) shouldBe Some(
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetColumnCount(Some(1))))
    )
    autoColumnsItem.steppedIntent(-1) shouldBe None
  }

  // issue #1046: command-runner visible-rows/item-gap-rows/cursor-gap-rows are no longer separate settings rows --
  // Interface Density is the one control that now governs command palette row height/spacing. The underlying config
  // keys still parse as explicit overrides (`ConfigManagerSurfaceLayoutSpec`), they just aren't editable here.
  it should "not surface command runner visible/item-gap/cursor-gap rows as separate interface settings" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withCommandRunnerVisibleRows(Some(9)))

    val interfaceGroup = groupByIdRecursive(runner.settingsGroups, "settings-interface-layout")

    val ids = interfaceGroup.children.map(_.id)
    ids should not contain "command-runner-visible-rows"
    ids should not contain "command-runner-item-gap-rows"
    ids should not contain "command-runner-cursor-gap-rows"
  }

  it should "surface render FPS target under Look's advanced leaf" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withRenderFpsTarget(RenderFpsTarget.Fps120))

    val renderingGroup = groupByIdRecursive(runner.settingsGroups, "settings-look-advanced")
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

  it should "surface drop caps as a toggle in the document defaults settings group" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withDropCapsEnabled(false).withShowAllSettingsRegardlessOfMode(true))

    val documentDefaultsGroup = groupByIdRecursive(runner.settingsGroups, "settings-document-defaults")

    val dropCaps =
      documentDefaultsGroup.children
        .collectFirst { case item: CommandSurfaceItem.OptionItem if item.id == "drop-caps-enabled" => item }
        .getOrElse(fail("missing drop caps option"))

    dropCaps.selectedOption shouldBe "Off"
    dropCaps.options.map(_.intent) shouldBe List(
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetDropCapsEnabled(true))),
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetDropCapsEnabled(false)))
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
    fontFamilyGroup.children.collect { case CommandSurfaceItem.CommandItem(command, _) => command.intent } should
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

  it should "file every key binding under exactly one Keys section" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner            = CommandRunner.empty.activate(registry, AppConfig.default).openSettings
    val keys              = groupByIdRecursive(runner.settingsGroups, "settings-keymap")
    val bindingIds        = descendants(keys).collect { case item: CommandSurfaceItem.InputItem => item.id }
    val expected =
      HotkeyAction.values.size + EditorKeyAction.values.size + CommandRunnerKeyAction.values.size +
        ModalKeyAction.values.size + PanelKeyAction.values.size + PeekKeyAction.values.size

    bindingIds should have size expected.toLong
    bindingIds.distinct should have size expected.toLong
  }
