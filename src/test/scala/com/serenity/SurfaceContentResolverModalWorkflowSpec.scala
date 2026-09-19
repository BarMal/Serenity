package com.serenity

import com.serenity.command.*
import com.serenity.document.RenderedComment
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceContentResolverModalWorkflowSpec extends AnyFlatSpec with Matchers:

  "SurfaceContentResolver" should "resolve context menus into a selected command list" in {
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

  it should "resolve ModalWorkflow content to empty rows, leaving rendering to the shared modal composition" in {
    // `ModalSurfaceComposition` (issue #819) is now the sole source of truth for what a `ModalWorkflow` surface
    // paints -- see `ModalSurfaceCompositionSpec` for coverage of each modal kind's actual content.
    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ModalWorkflow(Modal.Find("needle", Nil, 0)),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    floating shouldBe ResolvedSurfaceContent()
  }

end SurfaceContentResolverModalWorkflowSpec
