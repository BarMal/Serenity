package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceContentResolverCommandPaletteSpec extends AnyFlatSpec with Matchers:

  "SurfaceContentResolver" should "resolve command palettes into search chrome, highlighted rows, and scroll metadata once typing begins" in {
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

  it should "leave keyHintRow empty and the footer unchanged when showKeyHints is off" in {
    val registry = CommandRegistry.default
    val runner   = CommandRunner.empty.activate(registry, AppConfig.default)

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 60, 10),
      SurfaceRenderMode.Floating,
      showKeyHints = false
    )

    resolved.keyHintRow shouldBe None
    resolved.footer.map(_.plainText) shouldBe defined
  }

  it should "render a persistent palette key-hint row that survives a status message, distinct from the footer" in {
    val registry     = CommandRegistry.default
    val plainRunner  = CommandRunner.empty.activate(registry, AppConfig.default)
    val statusRunner = plainRunner.copy(statusMessage = Some("Invalid binding: ctrl"))

    val plainResolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(plainRunner),
      LayoutRect(0, 0, 60, 10),
      SurfaceRenderMode.Floating,
      showKeyHints = true
    )
    val statusResolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(statusRunner),
      LayoutRect(0, 0, 60, 10),
      SurfaceRenderMode.Floating,
      showKeyHints = true
    )

    plainResolved.keyHintRow.map(_.plainText) shouldBe Some("↑↓ navigate • Enter run • Esc dismiss")
    plainResolved.footer shouldBe None

    // The key hint stays put and the status message shows up separately as the footer -- neither slot suppresses
    // the other now that the row is persistent chrome (issue #931, Stage 3).
    statusResolved.keyHintRow.map(_.plainText) shouldBe Some("↑↓ navigate • Enter run • Esc dismiss")
    statusResolved.footer.map(_.plainText) shouldBe Some("Invalid binding: ctrl")
  }

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

  it should "render root search text and filtered language command results" in {
    val registry          = CommandRegistry(CommandRegistry.default.getAllCommands.filter(_.name.startsWith("lang-")))
    given CommandRegistry = registry
    // Windows Desktop Publish release-blocker: `updateSearchTerm` searches the settings tree unconditionally (it
    // isn't scoped by `registry` -- settings live outside CommandRegistry entirely, see CommandRunner.visibleItems),
    // and that tree's font-family groups are built from whatever fonts are actually installed on the machine running
    // this test. A real font whose family name happens to contain the search term below (Windows CI ships
    // "Javanese Text") would otherwise leak two spurious rows into the assertion. A deterministic FontFamilyCatalog
    // keeps this test's outcome independent of the host's installed fonts.
    val deterministicFontFamilies = FontLoader.FontFamilyCatalog(
      monospace = List("Deterministic Test Mono"),
      text = List("Deterministic Test Sans"),
      ui = List("Deterministic Test Sans")
    )
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .copy(fontFamilies = deterministicFontFamilies)
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

end SurfaceContentResolverCommandPaletteSpec
