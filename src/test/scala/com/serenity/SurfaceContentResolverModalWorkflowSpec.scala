package com.serenity

import com.serenity.document.RenderedComment
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceContentResolverModalWorkflowSpec extends AnyFlatSpec with Matchers:

  "SurfaceContentResolver" should "resolve ContextMenu content to empty rows, leaving rendering to the shared context menu composition" in {
    // `ContextMenuSurfaceComposition` (issue #819, slice 2) is now the sole source of truth for what a `ContextMenu`
    // surface paints -- see `ContextMenuSurfaceCompositionSpec` for coverage of its actual content, selection, footer,
    // and gap-row windowing (the "reserve configured gap rows between context menu items" case this replaces).
    val menu = ContextMenu(title = "editor", targetFocus = Focus.EditorPane(PaneId(0)), items = Nil)

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ContextMenu(menu),
      LayoutRect(0, 0, 28, 8),
      SurfaceRenderMode.Floating
    )

    floating shouldBe ResolvedSurfaceContent()
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
