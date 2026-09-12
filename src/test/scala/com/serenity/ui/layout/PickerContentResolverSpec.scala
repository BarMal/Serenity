package com.serenity.ui.layout

import com.serenity.command.*
import com.serenity.state.models.*
import com.serenity.ui.theme.Theme
import com.serenity.ui.theme.config.ThemeCreatorState
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated unit coverage for `PickerContentResolver` (issue #1421). `SurfaceContentResolverReferencePanelsSpec`
  * covers the theme picker, file search, and context menu behavior indirectly through `SurfaceContentResolver.resolve`;
  * this adds `resolveThemeCreator` coverage, entirely absent elsewhere, and pins each of the others by directly naming
  * this object. Lives in this package because the resolver is `private[layout]`.
  */
class PickerContentResolverSpec extends AnyFlatSpec with Matchers:

  // ── resolveThemePicker ──────────────────────────────────────────────────────

  "resolveThemePicker" should "window rows around the selected theme" in {
    val state =
      ThemePickerState(List("dark", "light", "mocha", "forest", "paper"), selectedIndex = 4, originalTheme = "dark")

    val resolved = PickerContentResolver.resolveThemePicker(state, LayoutRect(0, 0, 30, 5), SurfaceRenderMode.Floating)

    resolved.rows.map(_.plainText) shouldBe List("mocha", "forest", "paper")
    resolved.rows.map(_.selected) shouldBe List(false, false, true)
  }

  it should "title pinned picker output with the fixed \"Theme\" title" in {
    val state    = ThemePickerState(List("dark"), selectedIndex = 0, originalTheme = "dark")
    val resolved = PickerContentResolver.resolveThemePicker(state, LayoutRect(0, 0, 30, 6), SurfaceRenderMode.Pinned)

    resolved.title shouldBe Some("Theme")
  }

  // ── resolveThemeCreator ─────────────────────────────────────────────────────

  "resolveThemeCreator" should "render each row as label/path/value columns, selecting the current row" in {
    val state = ThemeCreatorState.fromTheme(Theme.dark).copy(selectedIndex = 1)

    val resolved =
      PickerContentResolver.resolveThemeCreator(state, LayoutRect(0, 0, 60, 20), SurfaceRenderMode.Floating)

    resolved.title shouldBe None
    resolved.header.map(_.plainText) shouldBe Some("theme creator")
    val selectedRow = resolved.rows.find(_.selected).getOrElse(fail("Expected one selected row"))
    selectedRow.layout shouldBe OverlayRowLayout.Columns
    selectedRow.cursorColumn shouldBe Some(selectedRow.plainText.length)
  }

  it should "mark an invalid color value row with an error tone" in {
    val state = ThemeCreatorState.fromTheme(Theme.dark)
    val invalidColorState = state
      .selectPath("ui.foreground")
      .replaceSelectedValue("not-a-color")

    val resolved =
      PickerContentResolver.resolveThemeCreator(invalidColorState, LayoutRect(0, 0, 60, 20), SurfaceRenderMode.Floating)

    val row = resolved.rows.find(_.plainText.startsWith("Foreground")).getOrElse(fail("Expected a Foreground row"))
    row.segments.last.tone shouldBe OverlayTone.Error
  }

  it should "render the status message footer in red" in {
    val state = ThemeCreatorState.fromTheme(Theme.dark).withStatus("Invalid color")

    val resolved =
      PickerContentResolver.resolveThemeCreator(state, LayoutRect(0, 0, 60, 20), SurfaceRenderMode.Floating)

    resolved.footer.map(_.plainText) shouldBe Some("Invalid color")
    resolved.footer.flatMap(_.foregroundColor) shouldBe Some(java.awt.Color.RED)
  }

  it should "omit the footer when there is no status message" in {
    val state = ThemeCreatorState.fromTheme(Theme.dark)
    val resolved =
      PickerContentResolver.resolveThemeCreator(state, LayoutRect(0, 0, 60, 20), SurfaceRenderMode.Floating)

    resolved.footer shouldBe None
  }

  // ── resolveFileSearch ────────────────────────────────────────────────────────

  "resolveFileSearch" should "show a single space as header text for an empty query" in {
    val state    = FileSearchState("", Nil, selectedIndex = 0)
    val resolved = PickerContentResolver.resolveFileSearch(state, LayoutRect(0, 0, 60, 10), SurfaceRenderMode.Floating)

    resolved.header.map(_.plainText) shouldBe Some(" ")
    resolved.rows shouldBe Nil
  }

  it should "position the header cursor at the end of a non-empty query" in {
    val state    = FileSearchState("def", Nil, selectedIndex = 0)
    val resolved = PickerContentResolver.resolveFileSearch(state, LayoutRect(0, 0, 60, 10), SurfaceRenderMode.Floating)

    resolved.header.flatMap(_.cursorColumn) shouldBe Some(3)
  }

  // ── resolveContextMenu ───────────────────────────────────────────────────────

  "resolveContextMenu" should "use the menu title for both the resolved header and the pinned title" in {
    val save = Command.typed("save", "Save file", CommandIntent.File(FileIntent.SaveCurrentFile), label = "Save")
    val menu = ContextMenu(
      title = "editor",
      targetFocus = Focus.EditorPane(PaneId(0)),
      items = List(ContextMenuItem(save.name, save.label, save))
    )

    val resolved = PickerContentResolver.resolveContextMenu(
      menu,
      LayoutRect(0, 0, 28, 8),
      SurfaceRenderMode.Pinned,
      itemGapRows = 0.0
    )

    resolved.title shouldBe Some("editor")
    resolved.header.map(_.plainText) shouldBe Some("editor")
  }

  it should "omit the footer entirely for an empty menu" in {
    val menu = ContextMenu(title = "empty", targetFocus = Focus.EditorPane(PaneId(0)), items = Nil)

    val resolved = PickerContentResolver.resolveContextMenu(
      menu,
      LayoutRect(0, 0, 28, 8),
      SurfaceRenderMode.Floating,
      itemGapRows = 0.0
    )

    resolved.rows shouldBe Nil
    resolved.footer shouldBe None
  }
