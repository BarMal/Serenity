package com.serenity

import com.serenity.document.RenderedComment
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.components.{CommentLensComponent, ComponentResult}
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommentLensComponentSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(0)
  private val comment  = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 7), "Initial")
  private val lensId   = SurfaceId("comment-lens")

  private def baseState: AppState =
    val buffer = Buffer
      .fromString(bufferId, "Opening paragraph")
      .copy(
        cursors = List(CursorPosition(0, 3)),
        documentComments = List(comment)
      )
    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(lensId),
      uiSurfaces = List(
        UiSurface(
          lensId,
          SurfaceContent.CommentLens(
            CommentLensState(
              RenderedComment(0, "Initial", "Initial"),
              "Initial",
              "Initial".length,
              Some(comment)
            )
          ),
          SurfacePresentation.Floating(Some(CursorPosition(0, 3)), SurfacePlacement.AboveCursor)
        )
      )
    )

  private val component = CommentLensComponent()

  "CommentLensComponent" should "edit the comment draft without changing the buffer" in {
    val result = component.processEvent(ModalInsertChar('!'), baseState)

    result match
      case ComponentResult.StateChange(update) =>
        val updated = update(baseState)
        updated.buffers(bufferId).documentComments shouldBe List(comment)
        commentLens(updated).draft shouldBe "Initial!"
        commentLens(updated).cursor shouldBe "Initial!".length
      case other => fail(s"Expected StateChange, got $other")
  }

  it should "move within and delete from the draft" in {
    val moved   = stateAfter(component.processEvent(ModalNavigate(Direction.Left), baseState), baseState)
    val deleted = stateAfter(component.processEvent(ModalDeleteBackward, moved), moved)

    commentLens(deleted).draft shouldBe "Initil"
    commentLens(deleted).cursor shouldBe 5
    deleted.buffers(bufferId).documentComments shouldBe List(comment)
  }

  it should "dismiss without saving on Escape" in {
    val state     = baseState.copy(focusHistory = List(Focus.EditorPane(paneId)))
    val edited    = stateAfter(component.processEvent(ModalInsertChar('!'), state), state)
    val dismissed = stateAfter(component.processEvent(ModalDismiss, edited), edited)

    dismissed.commentLensSurface shouldBe None
    dismissed.focus shouldBe Focus.EditorPane(paneId)
    dismissed.focusHistory shouldBe Nil
    dismissed.buffers(bufferId).documentComments shouldBe List(comment)
  }

  it should "save an authored comment and dismiss on Enter" in {
    val edited = stateAfter(component.processEvent(ModalInsertChar('!'), baseState), baseState)
    val saved  = stateAfter(component.processEvent(ModalSubmit, edited), edited)

    saved.commentLensSurface shouldBe None
    saved.focus shouldBe Focus.EditorPane(paneId)
    saved.buffers(bufferId).documentComments shouldBe List(comment.copy(text = "Initial!"))
    saved.buffers(bufferId).isDirty shouldBe true
  }

  private def stateAfter(result: ComponentResult, state: AppState): AppState =
    result match
      case ComponentResult.StateChange(update) => update(state)
      case other                               => fail(s"Expected StateChange, got $other")

  private def commentLens(state: AppState): CommentLensState =
    state.commentLensSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommentLens(lens) => Some(lens)
          case _                                => None
      }
      .getOrElse(fail("Expected comment lens"))

end CommentLensComponentSpec
