package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{RendererEntryPoints, RendererHighlights}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A resolved comment is put away: its range stops being highlighted until resolved comments are shown (#1903). */
class ResolvedCommentHighlightSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def stateWith(comment: DocumentComment, showResolved: Boolean): AppState =
    val buffer = Buffer
      .fromString(bufferId, "alpha beta gamma")
      .copy(
        editing = EditingState(List(CursorPosition(0, 0))),
        annotations = Annotations(documentComments = List(comment))
      )
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = Theme.light,
        config = com.serenity.config.AppConfig.default.withSyntaxHighlighting(false)
      ),
      runtime = AppState.initial.runtime.copy(resolvedCommentsVisible = showResolved)
    )

  private def highlightedCells(state: AppState, caches: RenderCaches): Int =
    val surface    = new MockRenderSurface(100, 30)
    val background = RendererHighlights.commentHighlightBackground(state.persisted.theme)
    RendererEntryPoints.render(state, cursorVisible = false, surface, ViewportSize(100, 30), caches)
    (0 until surface.width).count(x => "beta".contains(surface.getChar(x, 1)) && surface.getBg(x, 1) == background)

  private val open     = DocumentComment(CursorPosition(0, 6), CursorPosition(0, 10), "Review this", id = CommentId(1))
  private val resolved = open.resolve

  "An unresolved comment" should "be highlighted" in {
    highlightedCells(stateWith(open, showResolved = false), RenderCaches.create()) shouldBe 4
  }

  "A resolved comment" should "not be highlighted while resolved comments are hidden" in {
    highlightedCells(stateWith(resolved, showResolved = false), RenderCaches.create()) shouldBe 0
  }

  it should "be highlighted while resolved comments are shown" in {
    highlightedCells(stateWith(resolved, showResolved = true), RenderCaches.create()) shouldBe 4
  }

  it should "come and go with the toggle even when the frame caches are reused" in {
    val caches = RenderCaches.create()

    highlightedCells(stateWith(resolved, showResolved = false), caches) shouldBe 0
    highlightedCells(stateWith(resolved, showResolved = true), caches) shouldBe 4
    highlightedCells(stateWith(resolved, showResolved = false), caches) shouldBe 0
  }
