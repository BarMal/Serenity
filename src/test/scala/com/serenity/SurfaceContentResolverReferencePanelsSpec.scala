package com.serenity

import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceContentResolverReferencePanelsSpec extends AnyFlatSpec with Matchers:

  "SurfaceContentResolver" should "resolve ThemePicker rows with the selected index highlighted" in {
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

  it should "render the shortcuts-help reference as group headings followed by their entries" in {
    val groups = List(
      ShortcutHelpGroup("Global", List(ShortcutHelpEntry("Save", "ctrl+s"), ShortcutHelpEntry("Quit", "ctrl+q"))),
      ShortcutHelpGroup("Editor", List(ShortcutHelpEntry("Move Left", "left")))
    )

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.ShortcutsHelp(groups),
      LayoutRect(0, 0, 30, 20),
      SurfaceRenderMode.Pinned
    )

    resolved.title shouldBe Some("Keyboard Shortcuts")
    resolved.rows.map(_.plainText) shouldBe List(
      "Global",
      "Save: ctrl+s",
      "Quit: ctrl+q",
      "Editor",
      "Move Left: left"
    )
  }

  it should "clip the shortcuts-help reference to the rect it is given rather than overflow it" in {
    val groups = List(
      ShortcutHelpGroup(
        "Global",
        List.tabulate(10)(index => ShortcutHelpEntry(s"Action $index", s"key$index"))
      )
    )

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.ShortcutsHelp(groups),
      LayoutRect(0, 0, 30, 5),
      SurfaceRenderMode.Pinned
    )

    resolved.rows.size shouldBe 3
  }

end SurfaceContentResolverReferencePanelsSpec
