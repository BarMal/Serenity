package com.serenity

import java.nio.file.Paths

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.document.RenderedComment
import com.serenity.richtext.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceContentResolverSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val root = Paths.get("/repo")

  private val entries = List(
    DirEntry(root.resolve("src"), "src", isDirectory = true),
    DirEntry(root.resolve("test"), "test", isDirectory = true),
    DirEntry(root.resolve("build.sbt"), "build.sbt", isDirectory = false)
  )

  private val tree = DirectoryTreeData(
    rootPath = root,
    expandedPaths = Set(root, root.resolve("src")),
    entries = Map(
      root -> entries,
      root.resolve("src") -> List(
        DirEntry(root.resolve("src").resolve("main"), "main", isDirectory = true),
        DirEntry(root.resolve("src").resolve("Serenity.scala"), "Serenity.scala", isDirectory = false)
      )
    )
  )

  "SurfaceContentResolver" should "shape the same directory content differently when floating versus pinned" in {
    val content = SurfaceContent.DirectoryListing(root, entries, Some(root.resolve("src")))

    val floating = SurfaceContentResolver.resolve(
      content,
      LayoutRect(0, 0, 24, 20),
      SurfaceRenderMode.Floating
    )
    val pinned = SurfaceContentResolver.resolve(
      content,
      LayoutRect(0, 0, 24, 20),
      SurfaceRenderMode.Pinned
    )

    floating.title shouldBe None
    floating.rows.map(_.plainText) shouldBe List("Directory: repo", "src", "test", "build.sbt")

    pinned.title shouldBe Some("repo")
    pinned.rows.map(_.plainText) shouldBe List("Selected: src", "src", "test", "build.sbt")
  }

  it should "resolve directory trees with an explicit root row and lazy-load markers" in {
    val content = SurfaceContent.DirectoryTree(tree, Some(root.resolve("src")))

    val pinned = SurfaceContentResolver.resolve(
      content,
      LayoutRect(0, 0, 24, 20),
      SurfaceRenderMode.Pinned
    )

    pinned.title shouldBe Some("repo")
    pinned.rows.map(_.plainText) shouldBe List(
      "▾ repo",
      "  ▾ src",
      "    ▹ main",
      "    Serenity.scala",
      "  ▹ test",
      "  build.sbt"
    )
    pinned.rows.count(_.selected) shouldBe 1
    pinned.rows.find(_.selected).map(_.plainText) shouldBe Some("  ▾ src")
  }

  it should "resolve command palettes into search chrome, highlighted rows, and scroll metadata once typing begins" in {
    val commands = List(
      Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)),
      Command.typed("close", "Close current file", CommandIntent.File(FileIntent.CloseCurrentFile)),
      Command.typed("save", "Save current file", CommandIntent.File(FileIntent.SaveCurrentFile)),
      Command.typed("format", "Format current file", CommandIntent.Edit(EditIntent.FormatCurrentFile)),
      Command.typed("find", "Find text in file", CommandIntent.Edit(EditIntent.FindInCurrentFile)),
      Command.typed("replace", "Find and replace text", CommandIntent.Edit(EditIntent.ReplaceInCurrentFile))
    )
    val registry = CommandRegistry(commands)
    val runner = CommandRunner.empty.activate(registry, AppConfig.default).updateSearchTerm("open file")(using registry)

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 40, 10),
      SurfaceRenderMode.Floating
    )

    floating.title shouldBe None
    floating.header.map(_.plainText) shouldBe Some("search: open file")
    floating.header.flatMap(_.cursorColumn) shouldBe Some("search: open file".length)
    floating.rows.exists(_.selected) shouldBe true
    floating.rows.map(_.plainText).head should include("[Edit]")
    floating.rows should have size 2
    floating.rows.head.plainText should include("Open")
    floating.rows.head.plainText should include("Open file")
    floating.footer.map(_.plainText) shouldBe Some("↑↓ navigate • Enter run • Esc dismiss • 1/2")
  }

  it should "render active platform and user shortcut bindings beside core command rows" in {
    val config = AppConfig.default.withHotkeyConfig(
      HotkeyConfig.forOs("Mac OS X").withBinding(HotkeyAction.Find, "ctrl+alt+f")
    )
    val runner = CommandRunner.empty.activate(CommandRegistry.default, config)

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 80, 80),
      SurfaceRenderMode.Floating
    )

    resolved.rows.find(_.plainText.startsWith("Find")).map(_.segments.map(_.text)) shouldBe Some(
      List("Find", "Find text in the current file.", "ctrl+alt+f")
    )
    resolved.rows.find(_.plainText.startsWith("Replace")).map(_.segments.map(_.text)) shouldBe Some(
      List("Replace", "Find and replace text in the current file.", "alt+meta+f")
    )
  }

  // issue #931: category tabs are retired, along with browsing settings groups at the palette root by switching
  // category -- this test's whole subject (a "Tab categories"/"Enter open" footer for that browse mode) no longer
  // exists. Browsing settings groups without a search now goes through `resolveSettingsSurface` (`.openSettings`),
  // covered by "should render nested submenu headers as breadcrumbs" and friends below.

  it should "resolve context menus into a selected command list" in {
    val save = Command.typed("save", "Save file", CommandIntent.File(FileIntent.SaveCurrentFile), label = "Save")
    val find = Command.typed("find", "Find text", CommandIntent.Edit(EditIntent.FindInCurrentFile), label = "Find")
    val menu = ContextMenu(
      title = "editor",
      targetFocus = Focus.EditorPane(PaneId(0)),
      items = List(
        ContextMenuItem(save.name, save.label, save),
        ContextMenuItem(find.name, find.label, find)
      ),
      selectedIndex = 1
    )

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ContextMenu(menu),
      LayoutRect(0, 0, 28, 8),
      SurfaceRenderMode.Floating
    )

    floating.title shouldBe None
    floating.header.map(_.plainText) shouldBe Some("editor")
    floating.rows.map(_.plainText) shouldBe List("Save", "Find")
    floating.rows.map(_.selected) shouldBe List(false, true)
    floating.footer.map(_.plainText) shouldBe Some("2/2")
  }

  it should "reserve configured gap rows between context menu items" in {
    val save = Command.typed("save", "Save file", CommandIntent.File(FileIntent.SaveCurrentFile), label = "Save")
    val find = Command.typed("find", "Find text", CommandIntent.Edit(EditIntent.FindInCurrentFile), label = "Find")
    val menu = ContextMenu(
      title = "editor",
      targetFocus = Focus.EditorPane(PaneId(0)),
      items = List(
        ContextMenuItem(save.name, save.label, save),
        ContextMenuItem(find.name, find.label, find)
      )
    )

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ContextMenu(menu),
      LayoutRect(0, 0, 28, 6),
      SurfaceRenderMode.Floating,
      itemGapRows = 1
    )

    floating.rows.map(_.plainText) shouldBe List("Save")
  }

  it should "resolve multiline comment lenses into editable draft rows" in {
    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.CommentLens(
        CommentLensState(
          RenderedComment(
            sourceLine = 4,
            raw = "/*\n* **Review** this value\n*/",
            inlineMarkdown = "Review this value\nbefore release"
          ),
          draft = "Review this value\nbefore release",
          cursor = "Review this value".length,
          target = None
        )
      ),
      LayoutRect(0, 0, 28, 8),
      SurfaceRenderMode.Floating
    )

    floating.title shouldBe None
    floating.header.map(_.plainText) shouldBe Some("comment")
    floating.rows.map(_.plainText) shouldBe List(
      "Review this value",
      "before release"
    )
    floating.rows.map(_.selected) shouldBe List(true, false)
    floating.rows.head.cursorColumn shouldBe Some("Review this value".length)
  }

  // issue #931: category tabs are retired -- the distributed tab row this asserted on doesn't exist any more (the
  // header is always the live search box, `resolveCommandPalette`'s `Palette` branch). Browsing settings groups
  // without a search still renders `Columns`-layout group rows with no bracket markers, unchanged -- just through
  // `resolveSettingsSurface` (`.openSettings`) instead of the palette's old Settings category tab; see "should
  // render nested submenu headers as breadcrumbs" and the other `resolveSettingsSurface` tests below for that
  // coverage.

  it should "render direct settings search result rows with effective values and source scopes" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("default document")

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 90, 10),
      SurfaceRenderMode.Floating
    )

    val row = floating.rows
      .find(_.plainText.contains("Default Document"))
      .getOrElse(fail("Expected direct document mode result"))

    row.segments.headOption.map(_.text) shouldBe Some("Default Document")
    row.segments.map(_.text) shouldBe List(
      "Default Document",
      "Plain Text",
      "Global",
      "Settings > Document Writing > Document Defaults"
    )
  }

  it should "resolve option rows into label, hint, and selected value columns" in {
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(
        optionSelections = Map("interface-density" -> 1),
        surface = CommandRunnerSurface.Settings(drilled = Some(SettingsSurfaceState(SettingsPage.Group("settings-interface-layout"))))
      )

    val row = SurfaceContentResolver
      .resolve(
        SurfaceContent.CommandPalette(runner),
        LayoutRect(0, 0, 80, 10),
        SurfaceRenderMode.Floating
      )
      .rows
      .headOption
      .getOrElse(fail("Expected interface density option row"))

    row.layout shouldBe OverlayRowLayout.Columns
    row.plainText shouldBe "Interface Density: Compact, comfortable, or spacious Comfortable"
    row.segments.map(_.text) shouldBe List("Interface Density", "Compact, comfortable, or spacious", "Comfortable")
    row.segments.filter(_.selected).map(_.text) shouldBe List("Comfortable")
  }

  it should "render nested submenu headers as breadcrumbs" in {
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(
        surface = CommandRunnerSurface.Settings(drilled = Some(
          SettingsSurfaceState(
            SettingsPage.Group("settings-preset-fonts"),
            List(SettingsPage.Group("settings-preset-edit"), SettingsPage.Group("settings-ui-presets"))
          )
        ))
      )

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 80, 10),
      SurfaceRenderMode.Floating
    )

    val header = resolved.header.getOrElse(fail("Expected breadcrumb header"))
    header.plainText shouldBe "Settings > UI Presets > Edit Preset: Writing > Fonts"
    header.segments.map(_.text) shouldBe List("Settings >", "UI Presets >", "Edit Preset: Writing >", "Fonts")
    header.segments.map(_.selected) shouldBe List(true, true, true, false)
  }

  it should "resolve preset font submenu as grouped typography rows" in {
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(
        surface = CommandRunnerSurface.Settings(drilled = Some(
          SettingsSurfaceState(
            SettingsPage.Group("settings-preset-fonts"),
            List(SettingsPage.Group("settings-preset-edit"), SettingsPage.Group("settings-ui-presets"))
          )
        )
      )
      )

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 80, 10),
      SurfaceRenderMode.Floating
    )

    val groupRows = resolved.rows.filter(_.leadingPadding == 0)
    groupRows.map(_.plainText) shouldBe List("Editor Typography", "Code Typography", "UI Typography")
    groupRows.map(_.segments.map(_.text)) shouldBe List(
      List("Editor Typography", "Prose editor family, size, and ligatures"),
      List("Code Typography", "Code editor family, size, and ligatures"),
      List("UI Typography", "Interface family, size, and ligatures")
    )
    resolved.footer.map(_.plainText) shouldBe Some("Navigate • Open • Back • Dismiss • 1/3")
  }

  // issue #1057: this used to also show a "Theme Selection" row -- Theme Chooser/Creator/Toggle/Reload were one-shot
  // actions with no preset-scoped value of their own, so they are ordinary CommandRegistry commands now, not part
  // of this settings subtree; only Surface Material remains.
  it should "resolve preset theme submenu as a single surface material row" in {
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(
        surface = CommandRunnerSurface.Settings(drilled = Some(
          SettingsSurfaceState(
            SettingsPage.Group("settings-preset-theme"),
            List(SettingsPage.Group("settings-preset-edit"), SettingsPage.Group("settings-ui-presets"))
          )
        )
      )
      )

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 80, 10),
      SurfaceRenderMode.Floating
    )

    val groupRows = resolved.rows.filter(_.leadingPadding == 0)
    groupRows.map(_.plainText) shouldBe List("Surface Material")
    groupRows.map(_.segments.map(_.text)) shouldBe List(
      List("Surface Material", "Background, material, and blur")
    )
    resolved.footer.map(_.plainText) shouldBe Some("Navigate • Open • Back • Dismiss • 1/1")
  }

  it should "resolve preset document defaults submenu as grouped document, preview, and spelling rows" in {
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(
        surface = CommandRunnerSurface.Settings(drilled = Some(
          SettingsSurfaceState(
            SettingsPage.Group("settings-preset-document-defaults"),
            List(SettingsPage.Group("settings-preset-edit"), SettingsPage.Group("settings-ui-presets"))
          )
        )
      )
      )

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 80, 10),
      SurfaceRenderMode.Floating
    )

    val groupRows = resolved.rows.filter(_.leadingPadding == 0)
    groupRows.map(_.plainText) shouldBe List("New Documents", "Markdown Preview", "Spelling")
    groupRows.map(_.segments.map(_.text)) shouldBe List(
      List("New Documents", "Default mode for new buffers"),
      List("Markdown Preview", "Source, split preview, or inline lens"),
      List("Spelling", "Enable, languages, dictionaries, accepted words")
    )
    resolved.footer.map(_.plainText) shouldBe Some("Navigate • Open • Back • Dismiss • 1/3")
  }

  // issue #1059: selecting a nested group ("Create New Preset"/"Edit Preset") within "settings-ui-presets" used to
  // append a hand-written "Preset Preview ..." detail row from the old second-surface resolver. It now expands the
  // capped, generic group preview inline instead (SettingsSurfaceState.previewRows, indented via leadingPadding),
  // the same mechanism every other group uses -- there is no preset-specific detail row left.
  it should "expand the selected preset group's own children inline as the capped group preview" in {
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(
        surface = CommandRunnerSurface.Settings(drilled = Some(SettingsSurfaceState(SettingsPage.Group("settings-ui-presets", 1))))
      )

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 90, 12),
      SurfaceRenderMode.Floating
    )

    val selectedIndex = resolved.rows.indexWhere(_.selected)
    selectedIndex should be >= 0
    val previewRow = resolved.rows.lift(selectedIndex + 1).getOrElse(fail("Expected a group preview row"))
    previewRow.leadingPadding shouldBe 2
    previewRow.segments.map(_.tone) should contain(OverlayTone.Muted)
  }

  it should "return no floating rows for inactive command palettes" in {
    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(CommandRunner.empty),
      LayoutRect(0, 0, 40, 10),
      SurfaceRenderMode.Floating
    )

    resolved.header shouldBe None
    resolved.rows shouldBe Nil
    resolved.footer shouldBe None
  }

  it should "render command runner status messages in the command palette footer" in {
    val registry = CommandRegistry.default
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .copy(statusMessage = Some("Invalid binding: ctrl"))

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 60, 10),
      SurfaceRenderMode.Floating
    )

    resolved.footer.map(_.plainText) shouldBe Some("Invalid binding: ctrl")
  }

  // issue #1057: buffer-language switchers are ordinary CommandRegistry commands now (not a "settings-language"
  // settings group), so this exercises the same long-list scroll-windowing mechanics through the palette's root
  // rendering path instead -- a registry scoped to just the language commands keeps the fixture (and its literal
  // row/footer text) otherwise identical to before.
  it should "scroll a long root command list so the selected language stays visible" in {
    val registry = CommandRegistry(CommandRegistry.default.getAllCommands.filter(_.name.startsWith("lang-")))
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withSelectedVisibleIndex(10)

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 40, 8),
      SurfaceRenderMode.Floating
    )

    floating.rows.map(_.plainText) shouldBe List(
      "JSON - Use JSON mode for the current buffer.",
      "Java - Use Java mode for the current buffer.",
      "JavaScript - Use JavaScript mode for the current buffer.",
      "Kotlin - Use Kotlin mode for the current buffer."
    )
    floating.rows.count(_.selected) shouldBe 1
    floating.rows.find(_.selected).map(_.plainText) shouldBe Some(
      "JavaScript - Use JavaScript mode for the current buffer."
    )
    floating.footer.map(_.plainText) shouldBe Some("↑↓ navigate • Enter run • Esc dismiss • 11/23")
  }

  it should "derive command runner visible rows from the framed surface content contract" in {
    val commands = (1 to 8).toList.map(index =>
      Command.typed(
        s"cmd-$index",
        s"Command number $index",
        CommandIntent.Theme(ThemeIntent.ToggleTheme)
      )
    )
    val registry = CommandRegistry(commands)
    val runner   = CommandRunner.empty.activate(registry, AppConfig.default)
    val rect = LayoutRect(
      x = 0,
      y = 0,
      width = 80,
      height = SurfaceFrameLayout.frameHeightForItemRows(
        itemRows = 5,
        hasHeader = true,
        hasFooter = true,
        borderCells = SurfaceFrameLayout.CommandSurfaceBorderCells
      )
    )

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      rect,
      SurfaceRenderMode.Floating
    )

    floating.header shouldBe defined
    floating.rows should have size 5
    floating.footer shouldBe defined
  }

  it should "reduce command palette capacity when item gaps reserve blank content rows" in {
    val commands =
      (1 to 8).toList.map(index =>
        Command.typed(s"cmd-$index", s"Command number $index", CommandIntent.Theme(ThemeIntent.ToggleTheme))
      )
    val runner = CommandRunner.empty.activate(CommandRegistry(commands), AppConfig.default)
    val rect   = LayoutRect(0, 0, 80, 8)

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      rect,
      SurfaceRenderMode.Floating,
      itemGapRows = 1
    )

    floating.rows should have size 2
    floating.rows.map(_.plainText) shouldBe List(
      "Cmd 1 - Command number 1",
      "Cmd 2 - Command number 2"
    )
  }

  it should "reserve group preview rows without clipping available submenu items or footer" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .copy(surface = CommandRunnerSurface.Settings(drilled = Some(SettingsSurfaceState(SettingsPage.Group("settings-ui-presets", 1)))))
    val items        = runner.submenuItems("settings-ui-presets")
    val preview      = SettingsSurfaceState.previewRows(items, 1)
    val previewCount = preview.rows.size + (if preview.overflowCount > 0 then 1 else 0)
    val rect = LayoutRect(
      x = 0,
      y = 0,
      width = 90,
      height = SurfaceFrameLayout.frameHeightForItemRows(
        itemRows = items.size,
        hasHeader = true,
        hasFooter = true,
        reservedContentRows = previewCount
      )
    )

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      rect,
      SurfaceRenderMode.Floating
    )

    floating.header shouldBe defined
    floating.rows.count(row => row.leadingPadding == 0) shouldBe items.size
    floating.rows.count(row => row.leadingPadding > 0) shouldBe previewCount
    floating.footer shouldBe defined
  }

  // issue #1057/#931: same rationale as the scroll test above -- a language-only registry drives the palette's
  // root search-rendering path (category tabs retired, so the header is always the live search box).
  it should "render root search text and filtered language command results" in {
    val registry          = CommandRegistry(CommandRegistry.default.getAllCommands.filter(_.name.startsWith("lang-")))
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("java")

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 40, 8),
      SurfaceRenderMode.Floating
    )

    floating.header.map(_.plainText) shouldBe Some("search: java")
    // Every result here is CommandCategory.Settings, so each carries the quiet inline category tag search results
    // get once category tabs are gone (issue #931) -- "[Settings] " prefixes every row.
    floating.rows.map(_.plainText) shouldBe List(
      "[Settings] Java - Use Java mode for the current buffer.",
      "[Settings] JavaScript - Use JavaScript mode for the current buffer."
    )
    floating.rows.headOption.map(_.selected) shouldBe Some(true)
    floating.footer.map(_.plainText) shouldBe Some("↑↓ navigate • Enter run • Esc dismiss • 1/2")
  }

  it should "mark font submenu labels with their preview font family" in {
    val family = FontLoader.availableTextFamilies.head
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(surface = CommandRunnerSurface.Settings(drilled = Some(SettingsSurfaceState(SettingsPage.Group("text-font")))))

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 60, 8),
      SurfaceRenderMode.Floating
    )

    val firstRow = floating.rows.head
    firstRow.plainText should include(family)
    firstRow.segments.headOption.flatMap(_.fontFamily) shouldBe Some(family)
  }

  it should "resolve file workflow modals into field rows, suggestion rows, and a directory confirmation footer" in {
    val workflow = FileWorkflowState(
      mode = FileWorkflowMode.SaveAs,
      filename = "notes.scala",
      path = "/tmp/project/new/nested",
      activeField = FileWorkflowField.Path,
      suggestions = List(
        FileWorkflowSuggestion("/tmp/project", isDirectory = true),
        FileWorkflowSuggestion("/tmp/project/new", isDirectory = true)
      ),
      selectedSuggestionIndex = 1,
      missingPathSegments = List("new", "nested"),
      confirmCreateDirectories = true
    )

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    floating.header.map(_.plainText) shouldBe Some("save-as")
    floating.rows should have size 4

    val filenameRow = floating.rows.head
    filenameRow.layout shouldBe OverlayRowLayout.Split
    filenameRow.segments.head.text shouldBe "Filename"
    filenameRow.segments.last.text shouldBe "notes.scala"

    val pathRow = floating.rows(1)
    pathRow.layout shouldBe OverlayRowLayout.Split
    pathRow.selected shouldBe true
    pathRow.segments.head.text shouldBe "Path"
    pathRow.segments.exists(_.tone == OverlayTone.Error) shouldBe true

    val suggestionRows = floating.rows.drop(2)
    suggestionRows.map(_.plainText) shouldBe List("/tmp/project/", "/tmp/project/new/")
    suggestionRows.count(_.selected) shouldBe 1
    suggestionRows.find(_.selected).map(_.plainText) shouldBe Some("/tmp/project/new/")

    floating.footer.map(_.plainText) shouldBe Some("Create directories: new / nested")
  }

  it should "render file workflow status messages as a visible footer when present" in {
    val workflow = FileWorkflowState(
      mode = FileWorkflowMode.Open,
      filename = "missing.scala",
      path = "/tmp/project",
      statusMessage = Some("File not found: /tmp/project/missing.scala")
    )

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    floating.footer.map(_.plainText) shouldBe Some("File not found: /tmp/project/missing.scala")
  }

  it should "leave close workflow rendering to the shared modal composition" in {
    val workflow = CloseWorkflowState(
      scope = CloseScope.Current,
      currentBufferId = BufferId(7),
      currentBufferLabel = "notes.scala",
      selectedChoice = CloseWorkflowChoice.Discard
    )

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ModalWorkflow(Modal.CloseWorkflow(workflow)),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    floating shouldBe ResolvedSurfaceContent()
  }

  it should "render replace workflow modals with separate find and replace rows" in {
    val workflow = ReplaceWorkflowState(
      findText = "needle",
      replacementText = "thread",
      activeField = ReplaceWorkflowField.ReplaceWith,
      selectedAction = ReplaceWorkflowAction.ReplaceNext,
      selectedScope = ReplaceWorkflowScope.Selection,
      statusMessage = Some("3 matches will be replaced")
    )

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    floating.header.map(_.plainText) shouldBe Some("replace")
    floating.rows should have size 4

    val findRow = floating.rows.head
    findRow.layout shouldBe OverlayRowLayout.Split
    findRow.segments.head.text shouldBe "Find"
    findRow.segments.last.text shouldBe "needle"
    findRow.selected shouldBe false

    val replaceRow = floating.rows(1)
    replaceRow.layout shouldBe OverlayRowLayout.Split
    replaceRow.segments.head.text shouldBe "Replace"
    replaceRow.segments.last.text shouldBe "thread"
    replaceRow.selected shouldBe true

    val actionRow = floating.rows(2)
    actionRow.layout shouldBe OverlayRowLayout.Distributed
    actionRow.segments.map(_.text) shouldBe List("Replace Next", "Replace All")
    actionRow.segments.find(_.selected).map(_.text) shouldBe Some("Replace Next")

    val scopeRow = floating.rows(3)
    scopeRow.layout shouldBe OverlayRowLayout.Distributed
    scopeRow.segments.map(_.text) shouldBe List("Current Buffer", "Selection")
    scopeRow.segments.find(_.selected).map(_.text) shouldBe Some("Selection")

    floating.footer.map(_.plainText) shouldBe Some("3 matches will be replaced")
  }

  // ── ThemePicker resolver ──────────────────────────────────────────────────

  it should "render find modals as focused query overlays" in {
    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ModalWorkflow(Modal.Find("needle", Nil, 0)),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    floating.header.map(_.plainText) shouldBe Some("find")
    floating.rows should have size 1

    val queryRow = floating.rows.head
    queryRow.layout shouldBe OverlayRowLayout.Split
    queryRow.selected shouldBe true
    queryRow.cursorColumn shouldBe Some("Find ".length + "needle".length)
    queryRow.segments.map(_.text) shouldBe List("Find", "needle")
    queryRow.segments.last.selected shouldBe true
    floating.footer.map(_.plainText) shouldBe Some("0 matches")
  }

  it should "wrap contextual toolbar items across multiple rows and mark active rich-text formatting" in {
    val bufferId = BufferId(0)
    val selection = Selection(
      anchor = CursorPosition(0, 6),
      focus = CursorPosition(0, 10)
    )
    val richDocument = RichTextDocument(
      List(
        RichTextParagraph(
          runs = List(
            RichTextRun("alpha "),
            RichTextRun(
              "beta",
              RichTextStyle(
                marks = Set(InlineMark.Bold),
                fontFamily = Some("Serif"),
                fontSize = Some(18.0f),
                color = Some("#336699")
              )
            )
          ),
          alignment = ParagraphAlignment.Center,
          role = ParagraphRole.Heading(1)
        )
      )
    ).normalized
    val paneId = PaneId(0)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(
          bufferId -> Buffer
            .fromString(bufferId, "alpha beta")
            .copy(
              editing = EditingState(selection = Some(selection), cursors = List(selection.focus)),
              richText = RichTextState(richTextDocument = Some(richDocument))
            )
        ),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId)
        ),
        focus = Focus.EditorPane(paneId)
      )
    )

    val resolved = SurfaceContentResolver.resolveContextualToolbar(
      ContextualToolbarState(),
      state,
      LayoutRect(0, 0, 24, 8),
      SurfaceRenderMode.Floating
    )

    resolved.rows.length should be > 1
    resolved.rows.foreach(_.layout shouldBe OverlayRowLayout.Distributed)
    val segments = resolved.rows.flatMap(_.segments)
    segments.find(_.text.contains("Bold")).map(_.selected).shouldBe(Some(true))
    segments.exists(_.text == "Font Serif").shouldBe(true)
    segments.exists(_.text == "Family Serif").shouldBe(true)
    segments.exists(_.text == "Size 18").shouldBe(true)
    segments.exists(_.text == "Color Blue").shouldBe(true)
    segments.exists(_.text == "Hex #336699").shouldBe(true)
    segments.exists(_.text == "Role H1").shouldBe(true)
    segments.exists(_.text.contains("Center")).shouldBe(true)
  }

  it should "retain icon-font runs alongside labels in IconAndText toolbars" in {
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withContextualToolbarDisplayMode(ToolbarDisplayMode.IconAndText)
      )
    )

    val resolved = SurfaceContentResolver.resolveContextualToolbar(
      ContextualToolbarState(displayMode = ToolbarDisplayMode.IconAndText),
      state,
      LayoutRect(0, 0, 80, 8),
      SurfaceRenderMode.Floating
    )

    val toolbarItems = ContextualToolbar.itemsFor(state)
    val segments     = resolved.rows.flatMap(_.segments)
    segments.map(_.text) shouldBe toolbarItems.map(item =>
      ContextualToolbar.displayText(item, ToolbarDisplayMode.TextOnly)
    )
    segments.zip(toolbarItems).foreach {
      case (segment, item) =>
        segment.inlineIcon.shouldBe(Some(item.icon))
        segment.inlineIconFontFamily.shouldBe(Some(FontLoader.ToolbarIconFontFamily))
    }
  }

  it should "show heading level 4 in the contextual toolbar role control" in {
    val bufferId = BufferId(0)
    val selection = Selection(
      anchor = CursorPosition(0, 6),
      focus = CursorPosition(0, 10)
    )
    val richDocument = RichTextDocument(
      List(
        RichTextParagraph(
          runs = List(
            RichTextRun("alpha "),
            RichTextRun(
              "beta",
              RichTextStyle(fontFamily = Some("Serif"), fontSize = Some(18.0f), color = Some("#336699"))
            )
          ),
          role = ParagraphRole.Heading(4)
        )
      )
    ).normalized
    val paneId = PaneId(0)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(
          bufferId -> Buffer
            .fromString(bufferId, "alpha beta")
            .copy(
              editing = EditingState(selection = Some(selection), cursors = List(selection.focus)),
              richText = RichTextState(richTextDocument = Some(richDocument))
            )
        ),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId)
        ),
        focus = Focus.EditorPane(paneId)
      )
    )

    val resolved = SurfaceContentResolver.resolveContextualToolbar(
      ContextualToolbarState(),
      state,
      LayoutRect(0, 0, 24, 8),
      SurfaceRenderMode.Floating
    )

    resolved.rows.flatMap(_.segments).exists(_.text == "Role H4").shouldBe(true)
  }

  it should "render find result position when the modal carries match results" in {
    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ModalWorkflow(
        Modal.Find("needle", List(FindResult(2, 4), FindResult(5, 8), FindResult(8, 12)), 1)
      ),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    floating.header.map(_.plainText) shouldBe Some("find")
    floating.rows.head.plainText shouldBe "Find needle"
    floating.rows.map(_.plainText) shouldBe List("Find needle", "1. 3:5", "2. 6:9", "3. 9:13")
    floating.rows(2).selected shouldBe true
    floating.footer.map(_.plainText) shouldBe Some("3 matches, 2/3 at 6:9")
  }

  it should "render a find result window that keeps the selected match visible" in {
    val results = (0 until 6).map(line => FindResult(line, 0)).toList

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ModalWorkflow(Modal.Find("needle", results, 4)),
      LayoutRect(0, 0, 60, 6),
      SurfaceRenderMode.Floating
    )

    floating.rows.map(_.plainText) shouldBe List("Find needle", "4. 4:1", "5. 5:1", "6. 6:1")
    floating.rows(2).selected shouldBe true
    floating.footer.map(_.plainText) shouldBe Some("6 matches, 5/6 at 5:1")
  }

  it should "render an explicit empty result state for find queries with no matches" in {
    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ModalWorkflow(Modal.Find("missing", Nil, 0)),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    floating.header.map(_.plainText) shouldBe Some("find")
    floating.rows.map(_.plainText) shouldBe List("Find missing")
    floating.footer.map(_.plainText) shouldBe Some("0 matches")
  }

  it should "resolve ThemePicker rows with the selected index highlighted" in {
    val picker = ThemePickerState(List("dark", "light", "mocha"), selectedIndex = 1, originalTheme = "dark")

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.ThemePicker(picker),
      LayoutRect(0, 0, 40, 10),
      SurfaceRenderMode.Floating
    )

    resolved.rows should have size 3
    resolved.rows.map(_.plainText) shouldBe List("dark", "light", "mocha")
    resolved.rows.count(_.selected) shouldBe 1
    resolved.rows(1).selected shouldBe true
    resolved.rows(0).selected shouldBe false
  }

  it should "include a title for ThemePicker when pinned" in {
    val picker = ThemePickerState(List("dark"), selectedIndex = 0, originalTheme = "dark")
    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.ThemePicker(picker),
      LayoutRect(0, 0, 30, 6),
      SurfaceRenderMode.Pinned
    )
    resolved.title shouldBe Some("Theme")
  }

  it should "window ThemePicker rows so the selected theme stays visible inside the framed content rect" in {
    val picker = ThemePickerState(
      List("dark", "light", "mocha", "forest", "paper"),
      selectedIndex = 4,
      originalTheme = "dark"
    )
    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.ThemePicker(picker),
      LayoutRect(0, 0, 30, 5),
      SurfaceRenderMode.Floating
    )

    resolved.rows.map(_.plainText) shouldBe List("mocha", "forest", "paper")
    resolved.rows.map(_.selected) shouldBe List(false, false, true)
  }

  // ── FileSearch resolver ───────────────────────────────────────────────────

  it should "resolve FileSearch with query as header and result rows" in {
    val results = List(
      FileSearchResult(BufferId(0), "main.scala", 5, "def foo(x: Int)"),
      FileSearchResult(BufferId(1), "util.scala", 12, "def helper()")
    )
    val search = FileSearchState("def", results, selectedIndex = 0)

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.FileSearch(search),
      LayoutRect(0, 0, 60, 10),
      SurfaceRenderMode.Floating
    )

    resolved.header.map(_.plainText) shouldBe Some("def")
    resolved.header.flatMap(_.cursorColumn) shouldBe Some(3)
    resolved.rows should have size 2
    resolved.rows.head.selected shouldBe true
    resolved.rows(1).selected shouldBe false
    resolved.rows.head.plainText should include("main.scala")
    resolved.rows.head.plainText should include("6") // line + 1
  }

  it should "resolve FileSearch with empty query as header with space" in {
    val search = FileSearchState("", Nil, 0)
    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.FileSearch(search),
      LayoutRect(0, 0, 60, 10),
      SurfaceRenderMode.Floating
    )
    resolved.header.map(_.plainText) shouldBe Some(" ")
    resolved.rows shouldBe Nil
  }

  it should "show when FileSearch has more batches available" in {
    val results = List(FileSearchResult(BufferId(0), "main.scala", 5, "def foo(x: Int)"))
    val search  = FileSearchState("def", results, selectedIndex = 0, hasMoreResults = true)

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.FileSearch(search),
      LayoutRect(0, 0, 60, 10),
      SurfaceRenderMode.Floating
    )

    resolved.footer.map(_.plainText) shouldBe Some("1 loaded, more available")
  }

  it should "window FileSearch results against the shared frame contract when header and footer leave one item row" in {
    val results = List(
      FileSearchResult(BufferId(0), "main.scala", 5, "def foo(x: Int)"),
      FileSearchResult(BufferId(1), "util.scala", 12, "def helper()"),
      FileSearchResult(BufferId(2), "notes.md", 8, "def summary"),
      FileSearchResult(BufferId(3), "tail.scala", 21, "def selected()")
    )
    val search = FileSearchState("def", results, selectedIndex = 3, hasMoreResults = true)

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.FileSearch(search),
      LayoutRect(0, 0, 60, 5),
      SurfaceRenderMode.Floating
    )

    resolved.header.map(_.plainText) shouldBe Some("def")
    resolved.rows.map(_.plainText) shouldBe List("tail.scala:22  def selected()")
    resolved.rows.map(_.selected) shouldBe List(true)
    resolved.footer.map(_.plainText) shouldBe Some("4 loaded, more available")
  }

  it should "resolve Markdown previews as rendered pinned preview shells" in {
    val resolved = SurfaceContentResolver.resolveMarkdownPreview(
      title = "notes.md",
      content = """# Notes
            |
            || Task | Owner |
            || ---- | ----- |
            || Ship | Codex |
            |
            |![Diagram](diagram.png)""".stripMargin,
      rect = LayoutRect(0, 0, 40, 12),
      mode = SurfaceRenderMode.Pinned
    )

    resolved.title shouldBe Some("Preview: notes.md")
    resolved.rows.map(_.plainText) should contain("Notes")
    resolved.rows.exists(_.plainText.contains("Task")) shouldBe true
    resolved.rows.exists(_.plainText.contains("Ship")) shouldBe true
  }

end SurfaceContentResolverSpec
