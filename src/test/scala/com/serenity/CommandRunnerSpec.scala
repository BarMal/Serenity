package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.components.{CommandRunnerComponent, ComponentResult}
import com.serenity.state.models.*
import com.serenity.ui.layout.PanelPosition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def stateWithRunner(runner: CommandRunner): AppState =
    AppState.empty.copy(
      focus = Focus.Surface(SurfaceId("command-runner")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      )
    )

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

  "CommandRegistry" should "register and find commands" in {
    val registry = CommandRegistry.default
    val commands = registry.getAllCommands

    commands should not be empty
    commands.exists(_.name == "save") shouldBe true
    commands.exists(_.name == "open") shouldBe true
    registry.findCommand("open").map(_.label) shouldBe Some("Open File")
  }

  it should "search commands by partial name" in {
    val registry     = CommandRegistry.default
    val saveCommands = registry.searchCommands("sav")

    saveCommands should not be empty
    saveCommands.exists(_.name.contains("save")) shouldBe true
  }

  it should "search commands by description" in {
    val registry     = CommandRegistry.default
    val fileCommands = registry.searchCommands("file")

    fileCommands should not be empty
    fileCommands.exists(_.description.toLowerCase.contains("file")) shouldBe true
  }

  it should "search commands by human-facing label" in {
    val registry       = CommandRegistry.default
    val saveAsCommands = registry.searchCommands("save as")

    saveAsCommands.map(_.name) should contain("save-as")
  }

  "CommandSearcher" should "filter commands based on search term" in {
    val commands = List(
      Command.typed("save", "Save current file", CommandIntent.SaveCurrentFile),
      Command.typed("save-as", "Save file with new name", CommandIntent.SaveCurrentFileAs),
      Command.typed("open", "Open file", CommandIntent.OpenFile),
      Command.typed("quit", "Quit application", CommandIntent.QuitApp)
    )

    val searcher = new CommandSearcher(commands)

    val saveResults = searcher.search("save")
    saveResults.length shouldBe 2
    saveResults.map(_.name) should contain allOf ("save", "save-as")

    val openResults = searcher.search("open")
    openResults.length shouldBe 1
    openResults.head.name shouldBe "open"
  }

  it should "return commands in relevance order" in {
    val commands = List(
      Command.typed("save", "Save current file", CommandIntent.SaveCurrentFile),
      Command.typed("save-as", "Save file with new name", CommandIntent.SaveCurrentFileAs),
      Command.typed("auto-save", "Enable auto save", CommandIntent.ToggleLineNumbers)
    )

    val searcher = new CommandSearcher(commands)
    val results  = searcher.search("save")

    // "save" should come before "save-as" and "auto-save" due to exact match
    results.head.name shouldBe "save"
  }

  it should "limit results to specified count" in {
    val commands = (1 to 10).map(i => Command.typed(s"cmd$i", s"Command $i", CommandIntent.ToggleTheme)).toList
    val searcher = new CommandSearcher(commands)

    val results = searcher.search("cmd", maxResults = 5)
    results.length shouldBe 5
  }

  it should "browse commands by category when search is empty" in {
    val registry = CommandRegistry.default

    val fileCommands     = registry.commandsForCategory(CommandCategory.File)
    val settingsCommands = registry.commandsForCategory(CommandCategory.Settings)

    fileCommands should not be empty
    fileCommands.map(_.category).distinct shouldBe List(CommandCategory.File)
    settingsCommands.exists(_.name == "toggle-theme") shouldBe true
  }

  it should "reuse categorized command lists from the registry" in {
    val registry = CommandRegistry.default

    registry.commandsForCategory(CommandCategory.File) shouldBe theSameInstanceAs(
      registry.commandsForCategory(CommandCategory.File)
    )
  }

  it should "omit redundant top-level typography toggle commands" in {
    val registry     = CommandRegistry.default
    val commandNames = registry.getAllCommands.map(_.name)

    commandNames should not contain "increase-font-size"
    commandNames should not contain "decrease-font-size"
    commandNames should not contain "toggle-ligatures"
  }

  it should "include session persistence commands in the file category" in {
    val registry = CommandRegistry.default

    val fileCommandNames = registry.commandsForCategory(CommandCategory.File).map(_.name)

    fileCommandNames should contain allOf ("save-session", "restore-session", "clear-session")
    registry.findCommand("save-session").map(_.intent) shouldBe Some(CommandIntent.SaveSession)
    registry.findCommand("restore-session").map(_.intent) shouldBe Some(CommandIntent.RestoreSession)
    registry.findCommand("clear-session").map(_.intent) shouldBe Some(CommandIntent.ClearSession)
  }

  "CommandRunner state" should "initialize with empty search and no selection" in {
    val runner = CommandRunner.empty

    runner.searchTerm shouldBe ""
    runner.selectedIndex shouldBe 0
    runner.isActive shouldBe false
    runner.filteredCommands shouldBe empty
  }

  it should "update search term and filter commands" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner            = CommandRunner.empty.activate(registry, AppConfig.default)

    val updated = runner.updateSearchTerm("save")
    updated.searchTerm shouldBe "save"
    updated.filteredCommands should not be empty
    updated.filteredCommands.exists(_.name.contains("save")) shouldBe true
  }

  it should "browse the active category when search is empty and switch to global search once typing begins" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.File)

    runner.activeCategory shouldBe CommandCategory.File
    runner.visibleItems should not be empty
    runner.visibleItems.collect {
      case CommandSurfaceItem.CommandItem(command) => command.category
    }.distinct shouldBe List(CommandCategory.File)

    val searched = runner.updateSearchTerm("theme")
    searched.searchTerm shouldBe "theme"
    searched.visibleItems.exists {
      case CommandSurfaceItem.CommandItem(command) => command.name == "toggle-theme"
      case _                                       => false
    } shouldBe true
  }

  it should "surface motion settings as an expandable group in settings browsing" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)

    val animationGroup = groupByIdRecursive(runner.settingsGroups, "settings-animation")

    animationGroup.label shouldBe "Motion & Animation"
    animationGroup.children.map(_.id) shouldBe List(
      "motion-preset",
      "animation-mode",
      "editor-text-transition",
      "panel-open-transition",
      "panel-close-transition",
      "command-runner-fade",
      "ui-animation",
      "render-fps",
      "animation-duration",
      "animation-steps",
      "element-transition-speed-scale",
      "editor-text-speed-scale",
      "command-runner-speed-scale",
      "ui-speed-scale"
    )
  }

  it should "surface visual appearance settings as an expandable group in settings browsing" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)

    val appearanceGroup = groupByIdRecursive(runner.settingsGroups, "settings-surface-appearance")

    appearanceGroup.label shouldBe "Surface Appearance"
    appearanceGroup.children.map(_.id) shouldBe List("background-style", "material-preset", "blur-radius")
  }

  it should "group related settings into expandable submenu rows" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)

    val groupItems = runner.visibleItems.collect { case group: CommandSurfaceItem.GroupItem => group }

    groupItems.map(_.id) shouldBe List(
      "settings-workspace-layout",
      "settings-document-writing",
      "settings-editor-view",
      "settings-typography",
      "settings-appearance-motion",
      "settings-ui-presets",
      "settings-keymap"
    )
    def group(id: String): CommandSurfaceItem.GroupItem =
      groupItems.find(_.id == id).getOrElse(fail(s"missing group $id"))
    def nestedGroup(id: String): CommandSurfaceItem.GroupItem =
      groupItems
        .flatMap(item => descendants(item).collect { case group: CommandSurfaceItem.GroupItem => group })
        .find(_.id == id)
        .getOrElse(fail(s"missing nested group $id"))

    groupItems.head.label shouldBe "Panels & Workspace"
    groupItems.head.children.map(_.id) shouldBe List(
      "settings-panel-pins",
      "settings-panel-actions"
    )
    val panelPins    = groupById(groupItems.head.children, "settings-panel-pins")
    val panelActions = groupById(groupItems.head.children, "settings-panel-actions")
    panelPins.label shouldBe "Panel Pins"
    panelPins.children.map(_.id) shouldBe List(
      "panel-explorer-pin",
      "panel-outline-pin",
      "panel-diagnostics-pin",
      "panel-markdown-preview-pin"
    )
    panelActions.label shouldBe "Panel Actions"
    panelActions.children.map(_.id) should contain allOf (
      "focus-left-panel",
      "focus-right-panel",
      "focus-bottom-panel",
      "expand-left-panel",
      "expand-right-panel",
      "expand-bottom-panel",
      "unpin-left-panel",
      "unpin-right-panel",
      "unpin-bottom-panel",
      "collapse-expanded-panel"
    )
    group("settings-document-writing").children.map(_.id) shouldBe List(
      "settings-navigation",
      "settings-document-defaults",
      "settings-language",
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
      "settings-animation"
    )
    nestedGroup("settings-cursor").label shouldBe "Cursor"
    nestedGroup("settings-cursor").children.map(_.id) should contain allOf (
      "cursor-mode",
      "cursor-info-bar",
      "cursor-info-bar-placement",
      "cursor-speed-scale"
    )
    nestedGroup("settings-surface-appearance").label shouldBe "Surface Appearance"
    nestedGroup("settings-surface-appearance").children.map(_.id) shouldBe List(
      "background-style",
      "material-preset",
      "blur-radius"
    )
    nestedGroup("settings-interface-layout").label shouldBe "Interface Layout"
    nestedGroup("settings-interface-layout").children.map(_.id) shouldBe List(
      "interface-density",
      "ui-element-gap",
      "ui-corner-radius",
      "command-runner-visible-rows"
    )
    nestedGroup("settings-animation").children.map(_.id) should contain allOf (
      "motion-preset",
      "editor-text-transition",
      "panel-open-transition",
      "panel-close-transition",
      "command-runner-fade",
      "ui-animation",
      "render-fps"
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
      "focused-text-body"
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
      "markdown-view",
      "spellcheck-enabled"
    )
    nestedGroup("settings-language").label shouldBe "Current Buffer Language"
    nestedGroup("settings-language").children.map(_.id) should contain(
      "lang-plain-text"
    )
  }

  it should "surface workspace panel pins as dynamic option rows" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .copy(optionSelections = Map("panel-outline-pin" -> 2, "panel-diagnostics-pin" -> 4))
      .withActiveCategory(CommandCategory.Settings)

    val workspace = runner.visibleItems
      .collectFirst { case group: CommandSurfaceItem.GroupItem if group.id == "settings-workspace-layout" => group }
      .getOrElse(fail("Expected workspace layout settings group"))

    val panelPins  = groupById(workspace.children, "settings-panel-pins")
    val pinOptions = panelPins.children.collect { case option: CommandSurfaceItem.OptionItem => option }

    pinOptions.map(_.id) shouldBe List(
      "panel-explorer-pin",
      "panel-outline-pin",
      "panel-diagnostics-pin",
      "panel-markdown-preview-pin"
    )
    pinOptions.foreach(_.options.map(_.label) shouldBe List("Off", "Top", "Right", "Bottom", "Left"))
    pinOptions.find(_.id == "panel-outline-pin").map(_.selectedOption) shouldBe Some("Right")
    pinOptions.find(_.id == "panel-outline-pin").flatMap(_.selectedIntent) shouldBe
      Some(CommandIntent.SetPanelPin(PanelKind.Outline, Some(PanelPosition.Right)))
    pinOptions.find(_.id == "panel-diagnostics-pin").flatMap(_.selectedIntent) shouldBe
      Some(CommandIntent.SetPanelPin(PanelKind.Diagnostics, Some(PanelPosition.Left)))
  }

  it should "show current text display states as settings options" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val config = AppConfig.default
      .copy(showLineNumbers = false, showGutter = false, wordWrapEnabled = false)
    val runner = CommandRunner.empty
      .activate(registry, config)
      .withActiveCategory(CommandCategory.Settings)

    val textDisplay = groupByIdRecursive(runner.settingsGroups, "settings-text-display")
    val options     = textDisplay.children.collect { case option: CommandSurfaceItem.OptionItem => option }

    options.map(option => option.id -> option.selectedOption) shouldBe List(
      "line-numbers"      -> "Off",
      "gutter"            -> "Off",
      "line-wrap"         -> "Off",
      "focused-text-body" -> "Off"
    )
    options.flatMap(_.selectedIntent) shouldBe List(
      CommandIntent.SetLineNumbers(false),
      CommandIntent.SetGutter(false),
      CommandIntent.SetWordWrap(false),
      CommandIntent.SetFocusedTextBody(false)
    )
  }

  it should "surface command runner visible rows as a typed interface setting" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withCommandRunnerVisibleRows(Some(9)))
      .withActiveCategory(CommandCategory.Settings)

    val interfaceGroup = groupByIdRecursive(runner.settingsGroups, "settings-interface-layout")
    val input = interfaceGroup.children
      .collectFirst { case item: CommandSurfaceItem.InputItem if item.id == "command-runner-visible-rows" => item }
      .getOrElse(fail("missing command runner visible rows input"))

    input.currentValue shouldBe "9"
    input.parse("12") shouldBe Some(CommandIntent.SetCommandRunnerVisibleRows(Some(12)))
    input.parse("auto") shouldBe Some(CommandIntent.SetCommandRunnerVisibleRows(None))
    input.parse("0") shouldBe None
  }

  it should "surface render FPS target as a motion setting" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withRenderFpsTarget(RenderFpsTarget.Fps120))
      .withActiveCategory(CommandCategory.Settings)

    val animationGroup = groupByIdRecursive(runner.settingsGroups, "settings-animation")
    val option = animationGroup.children
      .collectFirst { case item: CommandSurfaceItem.OptionItem if item.id == "render-fps" => item }
      .getOrElse(fail("missing render FPS option"))

    option.selectedOption shouldBe "120 FPS"
    option.options.map(_.label) shouldBe List("30 FPS", "60 FPS", "90 FPS", "120 FPS", "Uncapped")
    option.selectedIntent shouldBe Some(CommandIntent.SetRenderFpsTarget(RenderFpsTarget.Fps120))
  }

  it should "reuse derived settings groups within a command runner state" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)

    runner.settingsGroups shouldBe theSameInstanceAs(runner.settingsGroups)
  }

  it should "reuse derived visible items within a command runner state" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)

    runner.visibleItems shouldBe theSameInstanceAs(runner.visibleItems)
  }

  it should "surface default document mode as a typed document defaults setting" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withDefaultDocumentMode(DefaultDocumentMode.RichText))
      .withActiveCategory(CommandCategory.Settings)

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
      CommandIntent.SetDefaultDocumentMode(DefaultDocumentMode.PlainText),
      CommandIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown),
      CommandIntent.SetDefaultDocumentMode(DefaultDocumentMode.RichText)
    )
  }

  it should "surface rich text inline style inputs in settings" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)

    val richTextGroup = groupByIdRecursive(runner.settingsGroups, "settings-rich-text")
    val inputs        = richTextGroup.children.collect { case item: CommandSurfaceItem.InputItem => item }

    inputs.map(_.id) shouldBe List("rich-text-font-family", "rich-text-font-size", "rich-text-color")
    inputs.head.parse("Serif") shouldBe Some(CommandIntent.SetRichTextFontFamily("Serif"))
    inputs(1).parse("18") shouldBe Some(CommandIntent.SetRichTextFontSize(18.0f))
    inputs(2).parse("#336699") shouldBe Some(CommandIntent.SetRichTextColor("#336699"))
    inputs(2).parse("not-a-colour") shouldBe None
  }

  it should "surface spell-check settings as typed controls" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(
        registry,
        AppConfig.default.withSpellCheck(
          SpellCheckConfig(
            enabled = true,
            languages = List("en", "fr"),
            dictionaryPaths = List("C:\\Dictionaries\\en_US.dic"),
            additionalWords = List("serenity")
          )
        )
      )
      .withActiveCategory(CommandCategory.Settings)

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
    enabledOption.selectedIntent shouldBe Some(CommandIntent.SetSpellCheckEnabled(true))
    inputs.map(_.id) shouldBe List("spellcheck-languages", "spellcheck-dictionaries", "spellcheck-words")
    inputs.head.currentValue shouldBe "en,fr"
    inputs.head.parse("fr,en") shouldBe Some(CommandIntent.SetSpellCheckLanguages(List("fr", "en")))
    inputs(1).currentValue shouldBe "C:\\Dictionaries\\en_US.dic"
    inputs(1).parse("C:\\Dictionaries\\en_US.dic,/usr/share/hunspell/fr.dic") shouldBe Some(
      CommandIntent.SetSpellCheckDictionaryPaths(List("C:\\Dictionaries\\en_US.dic", "/usr/share/hunspell/fr.dic"))
    )
    inputs(2).currentValue shouldBe "serenity"
    inputs(2).parse("Serenity,caf\u00e9") shouldBe Some(CommandIntent.SetSpellCheckWords(List("serenity", "caf\u00e9")))
  }

  it should "surface UI preset save and apply inputs in settings" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withUiPresetNames(List("Drafting", "Research Notes"))
      .withActiveCategory(CommandCategory.Settings)
    val presetGroup = runner.settingsGroups.find(_.id == "settings-ui-presets").getOrElse(fail("missing presets group"))

    presetGroup.children.map(_.id) shouldBe List(
      "settings-preset-select",
      "settings-preset-create",
      "settings-preset-edit"
    )

    val selectPreset = presetGroup.children
      .collectFirst {
        case group: CommandSurfaceItem.GroupItem if group.id == "settings-preset-select" => group
      }
      .getOrElse(fail("missing select preset group"))
    val presetPicker = selectPreset.children
      .collectFirst {
        case item: CommandSurfaceItem.OptionItem if item.id == "ui-preset-select" => item
      }
      .getOrElse(fail("missing combined preset picker"))

    presetPicker.options
      .map(_.label) shouldBe List("Writing", "Documentation", "Code", "Review", "Drafting", "Research Notes")
    presetPicker.options.map(_.intent) should contain(CommandIntent.ApplyUiPreset("Writing"))
    presetPicker.options.map(_.intent) should contain(CommandIntent.ApplyUiPreset("Research Notes"))
    presetPicker.options.headOption.flatMap(_.hint) shouldBe Some(
      "rich text default; dark; subtle motion; typed text reveal; frosted material; frosted background; spacious density; Serif 18pt prose; 1 editor pane; Left outline 28"
    )
    presetPicker.options.takeRight(2).map(_.hint) shouldBe List(
      Some("Saved workspace setup"),
      Some("Saved workspace setup")
    )

    val createPreset = presetGroup.children
      .collectFirst {
        case item: CommandSurfaceItem.GroupItem if item.id == "settings-preset-create" => item
      }
      .getOrElse(fail("missing create preset group"))
    val editPreset = presetGroup.children
      .collectFirst {
        case item: CommandSurfaceItem.GroupItem if item.id == "settings-preset-edit" => item
      }
      .getOrElse(fail("missing edit preset group"))

    createPreset.label shouldBe "Create New Preset"
    createPreset.children.map(_.id) shouldBe List(
      "settings-preset-create-name",
      "settings-preset-active-panels",
      "settings-preset-theme",
      "settings-preset-animations",
      "settings-preset-fonts",
      "settings-preset-document-defaults"
    )
    editPreset.label shouldBe "Edit Preset: Writing"
    editPreset.hint shouldBe Some("Editing Writing")
    editPreset.children.map(_.id) shouldBe List(
      "settings-preset-name",
      "settings-preset-active-panels",
      "settings-preset-theme",
      "settings-preset-animations",
      "settings-preset-fonts",
      "settings-preset-document-defaults"
    )
    val createName = createPreset.children
      .collectFirst {
        case group: CommandSurfaceItem.GroupItem if group.id == "settings-preset-create-name" => group
      }
      .getOrElse(fail("missing create preset name group"))
    createName.children.map(_.id) shouldBe List("ui-preset-create")

    val presetName = editPreset.children
      .collectFirst {
        case group: CommandSurfaceItem.GroupItem if group.id == "settings-preset-name" => group
      }
      .getOrElse(fail("missing preset name group"))
    presetName.label shouldBe "Name"
    presetName.children.map(_.id) shouldBe List(
      "ui-preset-save",
      "ui-preset-apply",
      "ui-preset-duplicate",
      "ui-preset-rename",
      "ui-preset-delete",
      "ui-preset-reset"
    )
    val activePanels = editPreset.children
      .collectFirst {
        case group: CommandSurfaceItem.GroupItem if group.id == "settings-preset-active-panels" => group
      }
      .getOrElse(fail("missing active panels group"))
    activePanels.label shouldBe "Active Panels"
    val workspaceItems = descendants(activePanels)
    workspaceItems.collect {
      case option: CommandSurfaceItem.OptionItem => option.options.map(_.intent)
    }.flatten should contain allOf (
      CommandIntent.SetPanelPin(PanelKind.Outline, Some(PanelPosition.Right)),
      CommandIntent.SetPanelPin(PanelKind.MarkdownPreview, Some(PanelPosition.Right)),
      CommandIntent.SetPanelPin(PanelKind.Explorer, Some(PanelPosition.Left)),
      CommandIntent.SetPanelPin(PanelKind.Diagnostics, Some(PanelPosition.Bottom))
    )
    workspaceItems.collect { case CommandSurfaceItem.CommandItem(command) => command.intent } should contain allOf (
      CommandIntent.FocusPanel(PanelPosition.Left),
      CommandIntent.FocusPanel(PanelPosition.Right),
      CommandIntent.FocusPanel(PanelPosition.Bottom),
      CommandIntent.ExpandPanel(PanelPosition.Left),
      CommandIntent.ExpandPanel(PanelPosition.Right),
      CommandIntent.ExpandPanel(PanelPosition.Bottom),
      CommandIntent.UnpinPanel(PanelPosition.Left),
      CommandIntent.UnpinPanel(PanelPosition.Right),
      CommandIntent.UnpinPanel(PanelPosition.Bottom),
      CommandIntent.CollapseExpandedPanel
    )
    val animations = groupByIdRecursive(List(editPreset), "settings-preset-animations")
    animations.children.map(_.id) shouldBe List(
      "settings-preset-cursor-motion",
      "settings-preset-text-entry-motion",
      "settings-preset-ui-surface-motion"
    )
    val fonts = groupByIdRecursive(List(editPreset), "settings-preset-fonts")
    fonts.children.map(_.id) shouldBe List(
      "settings-preset-editor-typography",
      "settings-preset-code-typography",
      "settings-preset-ui-typography"
    )
    val documentDefaults = groupByIdRecursive(List(editPreset), "settings-preset-document-defaults")
    documentDefaults.children.map(_.id) shouldBe List(
      "settings-preset-new-documents",
      "settings-preset-markdown-preview",
      "settings-preset-spelling"
    )
    descendants(documentDefaults).map(_.id) should contain allOf (
      "default-document-mode",
      "markdown-view",
      "spellcheck-enabled",
      "spellcheck-languages",
      "spellcheck-dictionaries",
      "spellcheck-words"
    )
    descendants(documentDefaults).map(_.id) should not contain "lang-plain-text"
    val theme = groupByIdRecursive(List(editPreset), "settings-preset-theme")
    theme.label shouldBe "Theme & Surface"
    theme.children.map(_.id) shouldBe List("settings-preset-theme-selection", "settings-preset-surface-material")
    descendants(theme).map(_.id) should contain allOf ("background-style", "material-preset", "blur-radius")
    descendants(theme).collect { case CommandSurfaceItem.CommandItem(command) => command.intent } should contain allOf (
      CommandIntent.OpenThemeChooser,
      CommandIntent.ToggleTheme,
      CommandIntent.ReloadTheme
    )

    val inputs = descendants(presetGroup).collect {
      case item: CommandSurfaceItem.InputItem if item.id.startsWith("ui-preset-") => item
    }
    inputs.map(_.id) should contain allOf (
      "ui-preset-create",
      "ui-preset-save",
      "ui-preset-apply",
      "ui-preset-duplicate",
      "ui-preset-rename",
      "ui-preset-delete",
      "ui-preset-reset"
    )
    val createInput = inputs.find(_.id == "ui-preset-create").getOrElse(fail("missing create input"))
    val saveInput   = inputs.find(_.id == "ui-preset-save").getOrElse(fail("missing save input"))
    val applyInput  = inputs.find(_.id == "ui-preset-apply").getOrElse(fail("missing apply input"))
    val dupeInput   = inputs.find(_.id == "ui-preset-duplicate").getOrElse(fail("missing duplicate input"))
    val renameInput = inputs.find(_.id == "ui-preset-rename").getOrElse(fail("missing rename input"))
    val deleteInput = inputs.find(_.id == "ui-preset-delete").getOrElse(fail("missing delete input"))
    val resetInput  = inputs.find(_.id == "ui-preset-reset").getOrElse(fail("missing reset input"))

    createInput.label shouldBe "Create Preset"
    createInput.hint shouldBe "New preset name"
    saveInput.currentValue shouldBe "Writing"
    applyInput.currentValue shouldBe "Writing"
    dupeInput.currentValue shouldBe "Writing -> "
    renameInput.currentValue shouldBe "Writing -> "
    deleteInput.currentValue shouldBe "Writing"
    resetInput.currentValue shouldBe "Writing"
    createInput.parse("Longform Writing") shouldBe Some(CommandIntent.SaveUiPreset("Longform Writing"))
    dupeInput.parse("Writing -> My Writing") shouldBe Some(CommandIntent.DuplicateUiPreset("Writing", "My Writing"))
    renameInput.parse("Draft -> Final") shouldBe Some(CommandIntent.RenameUiPreset("Draft", "Final"))
    deleteInput.parse("Old Preset") shouldBe Some(CommandIntent.DeleteUiPreset("Old Preset"))
    resetInput.parse("Writing") shouldBe Some(CommandIntent.ResetUiPreset("Writing"))
    inputs.foreach { item =>
      item.accepts("", 'W') shouldBe true
      item.accepts("Work", ' ') shouldBe true
    }
  }

  it should "prioritize direct settings child matches over nested preset option matches" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("lang-markdown")

    runner.visibleItems.collectFirst { case group: CommandSurfaceItem.GroupItem => group.id } shouldBe Some(
      "settings-language"
    )
  }

  it should "preserve selected built-in and custom UI presets in the settings submenu" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withUiPresetNames(List("Drafting", "Research Notes"))
      .copy(optionSelections = Map("ui-preset-built-in" -> 2, "ui-preset-custom" -> 1))
      .withActiveCategory(CommandCategory.Settings)

    val presetGroup = runner.settingsGroups.find(_.id == "settings-ui-presets").getOrElse(fail("missing presets group"))
    val presetPicker = descendants(presetGroup)
      .collectFirst {
        case item: CommandSurfaceItem.OptionItem if item.id == "ui-preset-select" => item
      }
      .getOrElse(fail("missing preset picker"))

    presetPicker.options.map(_.label) shouldBe List(
      "Writing",
      "Documentation",
      "Code",
      "Review",
      "Drafting",
      "Research Notes"
    )
    presetPicker.selectedOption shouldBe "Research Notes"

    val inputs = descendants(presetGroup).collect {
      case item: CommandSurfaceItem.InputItem if item.id.startsWith("ui-preset-") => item
    }
    inputs.find(_.id == "ui-preset-save").map(_.currentValue) shouldBe Some("Research Notes")
    inputs.find(_.id == "ui-preset-apply").map(_.currentValue) shouldBe Some("Research Notes")
    inputs.find(_.id == "ui-preset-duplicate").map(_.currentValue) shouldBe Some("Research Notes -> ")
    inputs.find(_.id == "ui-preset-rename").map(_.currentValue) shouldBe Some("Research Notes -> ")
    inputs.find(_.id == "ui-preset-delete").map(_.currentValue) shouldBe Some("Research Notes")
    inputs.find(_.id == "ui-preset-reset").map(_.currentValue) shouldBe Some("Research Notes")
  }

  it should "surface font settings groups ahead of command matches when searching font-related terms" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("font")

    runner.visibleItems.headOption.map(_.id) shouldBe Some("settings-prose-font")
    runner.visibleItems.exists {
      case group: CommandSurfaceItem.GroupItem => group.id == "settings-code-font"
      case _                                   => false
    } shouldBe true
  }

  it should "keep strong command matches ahead of settings groups during search" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("new")

    runner.visibleItems.headOption.map(_.id) shouldBe Some("new")
  }

  it should "handle selection navigation" in {
    val commands = List(
      Command.typed("cmd1", "Command 1", CommandIntent.ToggleTheme),
      Command.typed("cmd2", "Command 2", CommandIntent.ToggleLineNumbers),
      Command.typed("cmd3", "Command 3", CommandIntent.ToggleGutter)
    )
    val runner = CommandRunner.withCommands(commands).activate(CommandRegistry(commands), AppConfig.default)

    val movedDown = runner.moveSelection(1)
    movedDown.selectedIndex shouldBe 1

    val movedUp = movedDown.moveSelection(-1)
    movedUp.selectedIndex shouldBe 0

    // Should wrap around
    val wrapDown = runner.moveSelection(commands.length)
    wrapDown.selectedIndex shouldBe 0
  }

  "CommandRunnerComponent" should "activate command runner on hotkey" in {
    val component    = new CommandRunnerComponent()
    val initialState = AppState.empty

    val result = component.processEvent(ToggleCommandRunner, initialState)

    result shouldBe ComponentResult.noChange
  }

  it should "handle search input" in {
    val component   = new CommandRunnerComponent()
    val activeState = stateWithRunner(CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default))

    val result = component.processEvent(InsertChar('s'), activeState)

    result shouldNot be(ComponentResult.noChange)
    // Should update search term and filter commands
  }

  it should "handle escape to close runner" in {
    val component   = new CommandRunnerComponent()
    val activeState = stateWithRunner(CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default))

    val result = component.processEvent(com.serenity.keystroke.events.Escape, activeState)

    result shouldNot be(ComponentResult.noChange)
    // Should deactivate command runner and restore focus
  }

  it should "handle enter to execute selected command" in {
    val testCommand = Command.typed("test", "Test command", CommandIntent.ToggleLineNumbers)

    val component         = new CommandRunnerComponent()
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner
      .withCommands(List(testCommand))
      .activate(registry, AppConfig.default)
      .updateSearchTerm("test")
    val activeState = stateWithRunner(runner)

    val result = component.processEvent(Enter, activeState)

    result shouldNot be(ComponentResult.noChange)
  }

  "Command model" should "carry typed intents without a custom execution escape hatch" in {
    val testCommand = Command.typed("test", "Test command", CommandIntent.ToggleLineNumbers)

    testCommand.intent shouldBe CommandIntent.ToggleLineNumbers
  }
