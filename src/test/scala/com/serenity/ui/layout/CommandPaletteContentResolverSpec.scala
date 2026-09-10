package com.serenity.ui.layout

import com.serenity.command.*
import com.serenity.config.AppConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated unit coverage for `CommandPaletteContentResolver` (issue #1421). `SurfaceContentResolverSpec` covers most
  * of this behavior indirectly through `SurfaceContentResolver.resolve`, but never names this object directly. Lives
  * in this package because the resolver is `private[layout]`.
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
      Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)),
      Command.typed("close", "Close current file", CommandIntent.File(FileIntent.CloseCurrentFile))
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

    resolved.rows.map(_.plainText) shouldBe List("[File] Open - Open file")
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

  "inputRow" should "render the raw item value when not being edited" in {
    val item = CommandSurfaceItem.InputItem(
      id = "font-size",
      label = "Size",
      hint = "Points",
      currentValue = "12",
      isDecimal = true,
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
      isDecimal = true,
      parse = text => text.toFloatOption.filter(_ >= 1.0f).map(size => CommandIntent.RichText(RichTextIntent.SetRichTextFontSize(size))),
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
      isDecimal = true,
      parse = text => text.toFloatOption.filter(_ >= 1.0f).map(size => CommandIntent.RichText(RichTextIntent.SetRichTextFontSize(size))),
      category = CommandCategory.Edit
    )

    val row = CommandPaletteContentResolver.inputRow(item, selected = true, editingText = Some("nope"))

    row.segments.last.tone shouldBe OverlayTone.Error
  }
