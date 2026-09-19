package com.serenity

import com.serenity.document.RenderedComment
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `CommentLensSurfaceComposition` (issue #819, slice 3): the floating comment lens resolved into one
  * paint plan, the same pattern `ContextMenuSurfaceComposition`/`ContextualToolbarSurfaceComposition` already establish
  * for the other migrated surfaces. Row content stays sourced from `SurfaceContentResolver.commentLensRows` -- the
  * same, separately-tested row builder `SurfaceContentResolverModalWorkflowSpec` exercises via the dispatcher -- so
  * this object owns row position and frame sizing only. The lens has no per-row hit targets (see
  * `CommentLensMouseHitTesting`'s own doc comment), so `forLens` is covered only for its paint boxes here.
  */
class CommentLensSurfaceCompositionSpec extends AnyFlatSpec with Matchers:

  private def lensState(draft: String, cursor: Int): CommentLensState =
    CommentLensState(
      comment = RenderedComment(sourceLine = 4, raw = draft, inlineMarkdown = draft),
      draft = draft,
      cursor = cursor,
      target = None
    )

  "forLens" should "map the resolved header and draft rows to Text paint boxes, matching SurfaceContentResolver's rows" in {
    val lens = lensState("Review this value\nbefore release", cursor = "Review this value".length)
    val rect = LayoutRect(0, 0, 28, 8)

    val expectedRows = SurfaceContentResolver
      .resolve(SurfaceContent.CommentLens(lens), rect, SurfaceRenderMode.Floating)
      .rows

    val resolved = CommentLensSurfaceComposition.forLens(lens, rect)

    resolved.paintBoxes.map(_.kind).toSet shouldBe Set(SurfacePaintKind.Text)
    val headerBox = resolved.paintBoxes.headOption.getOrElse(fail("Expected a header paint box"))
    headerBox.text shouldBe Some("comment")

    val rowBoxes = resolved.paintBoxes.drop(1)
    rowBoxes.map(_.text) shouldBe expectedRows.map(row => Some(row.plainText))
    rowBoxes.map(_.selected) shouldBe expectedRows.map(_.selected)
    rowBoxes.map(_.cursorOffset) shouldBe expectedRows.map(_.cursorColumn)
  }

  it should "emit no hit regions, since the lens body is a single click-anywhere-to-edit target" in {
    val lens     = lensState("hello", cursor = 5)
    val resolved = CommentLensSurfaceComposition.forLens(lens, LayoutRect(0, 0, 28, 8))

    resolved.hitRegions shouldBe empty
    resolved.focusOrder shouldBe empty
  }

  "frameHeight" should "match the pre-migration formula: draft line count plus header/border chrome, clamped to [4, 8]" in {
    CommentLensSurfaceComposition.frameHeight(lensState("one line", cursor = 0)) shouldBe 4
    CommentLensSurfaceComposition.frameHeight(lensState("one\ntwo\nthree", cursor = 0)) shouldBe 6
    CommentLensSurfaceComposition.frameHeight(
      lensState((1 to 10).map(_.toString).mkString("\n"), cursor = 0)
    ) shouldBe 8
  }

end CommentLensSurfaceCompositionSpec
