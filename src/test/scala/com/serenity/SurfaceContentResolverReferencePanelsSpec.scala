package com.serenity

import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceContentResolverReferencePanelsSpec extends AnyFlatSpec with Matchers:

  "SurfaceContentResolver" should "resolve Markdown previews as rendered pinned preview shells" in {
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
