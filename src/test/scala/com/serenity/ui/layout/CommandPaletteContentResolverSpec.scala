package com.serenity.ui.layout

import com.serenity.command.*
import com.serenity.config.AppConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated unit coverage for `CommandPaletteContentResolver` (issue #1421).
  * `SurfaceContentResolverCommandPaletteSpec` covers most of this behavior indirectly through
  * `SurfaceContentResolver.resolve`, but never names this object directly. Lives in this package because the resolver
  * is `private[layout]`.
  */
class CommandPaletteContentResolverSpec extends AnyFlatSpec with Matchers:

  "resolveCommandPalette" should "render an empty title-only chrome row for an inactive palette" in {
    val resolved = CommandPaletteContentResolver.resolveCommandPalette(
      CommandRunner.empty,
      LayoutRect(0, 0, 40, 10),
      SurfaceRenderMode.Floating,
      itemGapRows = 0.0,
      itemTargetRows = 1,
      showKeyHints = false
    )

    resolved.rows shouldBe Nil
    resolved.header shouldBe None
  }

  it should "route to resolveSettingsSurface whenever the runner's surface is Settings" in {
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(surface = CommandRunnerSurface.Settings())

    val viaDispatch = CommandPaletteContentResolver.resolveCommandPalette(
      runner,
      LayoutRect(0, 0, 80, 12),
      SurfaceRenderMode.Floating,
      itemGapRows = 0.0,
      itemTargetRows = 1,
      showKeyHints = false
    )
    val direct = CommandPaletteContentResolver.resolveSettingsSurface(
      runner,
      LayoutRect(0, 0, 80, 12),
      itemGapRows = 0.0,
      itemTargetRows = 1,
      showKeyHints = false
    )

    viaDispatch shouldBe direct
    viaDispatch.title shouldBe Some("Settings")
  }

  it should "put the live search term (empty or not) in the header, never a category tab row" in {
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)

    val resolved = CommandPaletteContentResolver.resolveCommandPalette(
      runner,
      LayoutRect(0, 0, 60, 10),
      SurfaceRenderMode.Floating,
      itemGapRows = 0.0,
      itemTargetRows = 1,
      showKeyHints = false
    )

    resolved.header.map(_.plainText) shouldBe Some("search: ")
    resolved.header.flatMap(_.cursorColumn) shouldBe Some("search: ".length)
  }

  it should "tag search results with their category once a search term is present" in {
    val commands = List(
      Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile), CommandCategory.File),
      Command.typed(
        "close",
        "Close current file",
        CommandIntent.File(FileIntent.CloseCurrentFile),
        CommandCategory.File
      )
    )
    val runner = CommandRunner.empty
      .activate(CommandRegistry(commands), AppConfig.default)
      .updateSearchTerm("open")(using CommandRegistry(commands))

    val resolved = CommandPaletteContentResolver.resolveCommandPalette(
      runner,
      LayoutRect(0, 0, 60, 10),
      SurfaceRenderMode.Floating,
      itemGapRows = 0.0,
      itemTargetRows = 1,
      showKeyHints = false
    )

    // `updateSearchTerm` also searches the app-wide settings tree unconditionally, independent of the small
    // registry passed here (see `SurfaceContentResolverCommandPaletteSpec`'s "render root search text..." comment on the same
    // behavior), so other unrelated rows can legitimately appear alongside the command match this test targets, and
    // `bindingFor` can attach a real default hotkey (e.g. "ctrl+o") looked up by intent -- so this only pins the
    // category tag and description this resolver is responsible for, not the full row text.
    val openRow = resolved.rows
      .find(row => row.plainText.startsWith("[File] Open") && row.plainText.contains("Open file"))
    openRow shouldBe defined
  }

  "resolveSettingsSurface" should "title the surface \"Settings\" and breadcrumb-header the root group" in {
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)

    val resolved = CommandPaletteContentResolver.resolveSettingsSurface(
      runner,
      LayoutRect(0, 0, 90, 12),
      itemGapRows = 0.0,
      itemTargetRows = 2,
      showKeyHints = false
    )

    resolved.title shouldBe Some("Settings")
    resolved.header shouldBe defined
  }

  it should "populate the persistent key-hint row only when showKeyHints is true" in {
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)

    val withHints = CommandPaletteContentResolver.resolveSettingsSurface(
      runner,
      LayoutRect(0, 0, 90, 12),
      itemGapRows = 0.0,
      itemTargetRows = 2,
      showKeyHints = true
    )
    val withoutHints = CommandPaletteContentResolver.resolveSettingsSurface(
      runner,
      LayoutRect(0, 0, 90, 12),
      itemGapRows = 0.0,
      itemTargetRows = 2,
      showKeyHints = false
    )

    withHints.keyHintRow shouldBe defined
    withHints.footer shouldBe None
    withoutHints.keyHintRow shouldBe None
    withoutHints.footer shouldBe defined
  }

  "toggleRow" should "render a checked glyph and the label when checked" in {
    val item = CommandSurfaceItem.ToggleItem(
      id = "test-toggle",
      label = "Enable widget",
      checked = false,
      category = CommandCategory.Settings
    )

    val row = CommandPaletteContentResolver.toggleRow(item, checked = true, selected = false)

    row.plainText shouldBe "[x] Enable widget"
    row.segments.map(_.text) shouldBe List("[x]", "Enable widget")
  }

  it should "render an unchecked glyph when unchecked" in {
    val item = CommandSurfaceItem.ToggleItem(
      id = "test-toggle",
      label = "Enable widget",
      checked = true,
      category = CommandCategory.Settings
    )

    val row = CommandPaletteContentResolver.toggleRow(item, checked = false, selected = false)

    row.plainText shouldBe "[ ] Enable widget"
    row.segments.map(_.text) shouldBe List("[ ]", "Enable widget")
  }

  it should "read the checked state passed in, not the item's own baked-in checked field" in {
    val item = CommandSurfaceItem.ToggleItem(
      id = "test-toggle",
      label = "Enable widget",
      checked = true,
      category = CommandCategory.Settings
    )

    // `checked` is supplied by the caller (the runner's `effectiveChecked`, after any in-place flip) rather than
    // read off `item.checked` directly -- the same relationship `inputRow` has with its `editingText` parameter.
    val row = CommandPaletteContentResolver.toggleRow(item, checked = false, selected = false)

    row.plainText shouldBe "[ ] Enable widget"
  }

  it should "append a muted hint segment when the item has one" in {
    val item = CommandSurfaceItem.ToggleItem(
      id = "test-toggle",
      label = "Enable widget",
      checked = false,
      category = CommandCategory.Settings,
      hint = Some("Restart required")
    )

    val row = CommandPaletteContentResolver.toggleRow(item, checked = false, selected = false)

    row.segments.last.text shouldBe "Restart required"
    row.segments.last.tone shouldBe OverlayTone.Muted
  }

  it should "mark the row selected when selected is true" in {
    val item = CommandSurfaceItem.ToggleItem(
      id = "test-toggle",
      label = "Enable widget",
      checked = false,
      category = CommandCategory.Settings
    )

    val row = CommandPaletteContentResolver.toggleRow(item, checked = false, selected = true)

    row.selected shouldBe true
  }

  "inputRow" should "render the raw item value when not being edited" in {
    val item = CommandSurfaceItem.InputItem(
      id = "font-size",
      label = "Size",
      hint = "Points",
      currentValue = "12",
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text => text.toFloatOption.map(size => CommandIntent.RichText(RichTextIntent.SetRichTextFontSize(size))),
      category = CommandCategory.Edit
    )

    val row = CommandPaletteContentResolver.inputRow(item, selected = false, editingText = None)

    row.plainText shouldBe "Size: Points 12"
    row.cursorColumn shouldBe None
    row.segments.map(_.text) shouldBe List("Size", "Points", "12")
  }

  it should "show the editing draft, an end-of-text cursor, and no error tone for a valid in-progress edit" in {
    val item = CommandSurfaceItem.InputItem(
      id = "font-size",
      label = "Size",
      hint = "Points",
      currentValue = "12",
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.toFloatOption
          .filter(_ >= 1.0f)
          .map(size => CommandIntent.RichText(RichTextIntent.SetRichTextFontSize(size))),
      category = CommandCategory.Edit
    )

    val row = CommandPaletteContentResolver.inputRow(item, selected = true, editingText = Some("18"))

    row.plainText shouldBe "Size: Points 18"
    row.cursorColumn shouldBe Some("Size: Points 18".length)
    row.segments.last.tone shouldBe OverlayTone.Normal
    row.segments.last.selected shouldBe true
  }

  it should "mark an unparseable in-progress edit with an error tone" in {
    val item = CommandSurfaceItem.InputItem(
      id = "font-size",
      label = "Size",
      hint = "Points",
      currentValue = "12",
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.toFloatOption
          .filter(_ >= 1.0f)
          .map(size => CommandIntent.RichText(RichTextIntent.SetRichTextFontSize(size))),
      category = CommandCategory.Edit
    )

    val row = CommandPaletteContentResolver.inputRow(item, selected = true, editingText = Some("nope"))

    row.segments.last.tone shouldBe OverlayTone.Error
  }

  it should "append a visible, muted default-value hint for a row that has one (issue #1056)" in {
    val item = CommandSurfaceItem.InputItem(
      id = "font-size",
      label = "Size",
      hint = "Points",
      currentValue = "18",
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text => text.toFloatOption.map(size => CommandIntent.RichText(RichTextIntent.SetRichTextFontSize(size))),
      category = CommandCategory.Edit,
      defaultValue = Some("12")
    )

    val row = CommandPaletteContentResolver.inputRow(item, selected = false, editingText = None)

    row.plainText shouldBe "Size: Points 18 (default: 12)"
    row.segments.last.text shouldBe "(default: 12)"
    row.segments.last.tone shouldBe OverlayTone.Muted
  }

  it should "show no default-value hint for a row with no default" in {
    val item = CommandSurfaceItem.InputItem(
      id = "rich-text-font-size",
      label = "Selection Font Size",
      hint = "Points (8.0-48.0)",
      currentValue = "",
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text => text.toFloatOption.map(size => CommandIntent.RichText(RichTextIntent.SetRichTextFontSize(size))),
      category = CommandCategory.Edit
    )

    val row = CommandPaletteContentResolver.inputRow(item, selected = false, editingText = None)

    row.segments.map(_.text) shouldBe List("Selection Font Size", "Points (8.0-48.0)", "")
  }
