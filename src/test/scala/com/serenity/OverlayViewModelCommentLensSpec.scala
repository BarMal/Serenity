package com.serenity

import com.serenity.document.RenderedComment
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.OverlayViewModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Split out of `OverlayViewModelSpec` to keep that file under the architecture check's line-count target -- covers
  * just the `CommentLens` composition wiring added in issue #819, slice 3.
  */
class OverlayViewModelCommentLensSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  "OverlayViewModel.fromState" should
    "resolve a CommentLens surface's composition through the same compositionFor dispatch as every other composed surface (issue #819, slice 3)" in {
      val lens = CommentLensState(
        comment = RenderedComment(sourceLine = 4, raw = "Review this value", inlineMarkdown = "Review this value"),
        draft = "Review this value",
        cursor = "Review this value".length,
        target = None
      )
      val buffer = Buffer
        .fromString(bufferId, "one\ntwo\nthree")
        .copy(editing = EditingState(List(CursorPosition(1, 2))))
      val pane = EditorPane.withBuffer(paneId, bufferId)
      val state = AppState.initial.copy(
        persisted = AppState.initial.persisted.copy(
          buffers = Map(bufferId -> buffer),
          bufferOrder = List(bufferId),
          layout = Layout(
            editorPanes = Map(paneId -> pane),
            activeEditorPaneId = Some(paneId),
            workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
          ),
          focus = Focus.Surface(SurfaceId("comment-lens"))
        ),
        runtime = AppState.initial.runtime.copy(
          uiSurfaces = List(
            UiSurface(
              SurfaceId("comment-lens"),
              SurfaceContent.CommentLens(lens),
              SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.AboveCursor)
            )
          )
        )
      )
      val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

      val overlay =
        OverlayViewModel.fromState(state, layout).aboveCursor.getOrElse(fail("Expected a comment lens overlay"))

      // Content is painted entirely from `CommentLensSurfaceComposition` (issue #819, slice 3); draft text and
      // cursor are read from its paint boxes, not the plain-rows `rows`/`header` fields this content no longer
      // populates.
      overlay.header shouldBe None
      overlay.rows shouldBe Nil
      val composition = overlay.composition.getOrElse(fail("Expected a comment lens composition"))
      composition.paintBoxes.map(_.text) shouldBe List(Some("comment"), Some("Review this value"))
      composition.paintBoxes.lastOption.flatMap(_.cursorOffset) shouldBe Some("Review this value".length)
    }

end OverlayViewModelCommentLensSpec
