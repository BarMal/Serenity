package com.serenity

import com.serenity.document.{CommentRendering, RenderedComment}
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
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
        editing = EditingState(List(CursorPosition(0, 3))),
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
        focus = Focus.Surface(lensId)
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            lensId,
            SurfaceContent.CommentLens(
              CommentLensState(
                RenderedComment(0, "Initial", "Initial"),
                "Initial",
                "Initial".length,
                Some(CommentLensTarget(0, comment))
              )
            ),
            SurfacePresentation.Floating(Some(CursorPosition(0, 3)), SurfacePlacement.AboveCursor)
          )
        )
      )
    )

  private val component = CommentLensComponent()

  "CommentLensComponent" should "edit the comment draft without changing the buffer" in {
    val result = component.processEvent(ModalInsertChar('!'), baseState)

    result match
      case ComponentResult.StateChange(update) =>
        val updated = update(baseState)
        updated.persisted.buffers(bufferId).annotations.documentComments shouldBe List(comment)
        commentLens(updated).draft shouldBe "Initial!"
        commentLens(updated).cursor shouldBe "Initial!".length
      case other => fail(s"Expected StateChange, got $other")
  }

  it should "move within and delete from the draft" in {
    val moved   = stateAfter(component.processEvent(ModalNavigate(Direction.Left), baseState), baseState)
    val deleted = stateAfter(component.processEvent(ModalDeleteBackward, moved), moved)

    commentLens(deleted).draft shouldBe "Initil"
    commentLens(deleted).cursor shouldBe 5
    deleted.persisted.buffers(bufferId).annotations.documentComments shouldBe List(comment)
  }

  it should "dismiss without saving on Escape" in {
    val state     = baseState.copy(runtime = baseState.runtime.copy(focusHistory = List(Focus.EditorPane(paneId))))
    val edited    = stateAfter(component.processEvent(ModalInsertChar('!'), state), state)
    val dismissed = stateAfter(component.processEvent(ModalDismiss, edited), edited)

    dismissed.commentLensSurface shouldBe None
    dismissed.persisted.focus shouldBe Focus.EditorPane(paneId)
    dismissed.runtime.focusHistory shouldBe Nil
    dismissed.persisted.buffers(bufferId).annotations.documentComments shouldBe List(comment)
  }

  it should "save an authored comment and dismiss on Enter" in {
    val edited = stateAfter(component.processEvent(ModalInsertChar('!'), baseState), baseState)
    val saved  = stateAfter(component.processEvent(ModalSubmit, edited), edited)

    saved.commentLensSurface shouldBe None
    saved.persisted.focus shouldBe Focus.EditorPane(paneId)
    saved.persisted.buffers(bufferId).annotations.documentComments shouldBe List(comment.copy(text = "Initial!"))
    saved.persisted.buffers(bufferId).document.isDirty shouldBe true
  }

  it should "ignore edit events while read-only" in {
    val readOnlyState = withLensMode(baseState, CommentLensMode.ReadOnly)

    val result = component.processEvent(ModalInsertChar('!'), readOnlyState)

    result shouldBe ComponentResult.noChange
  }

  it should "still dismiss without saving on Escape while read-only" in {
    val readOnlyState =
      withLensMode(baseState, CommentLensMode.ReadOnly)
        .copy(runtime = baseState.runtime.copy(focusHistory = List(Focus.EditorPane(paneId))))

    val dismissed = stateAfter(component.processEvent(ModalDismiss, readOnlyState), readOnlyState)

    dismissed.commentLensSurface shouldBe None
    dismissed.persisted.focus shouldBe Focus.EditorPane(paneId)
    dismissed.persisted.buffers(bufferId).annotations.documentComments shouldBe List(comment)
  }

  "A comment lens on a source-code comment" should "open read-only, so typing is never silently discarded" in {
    val buffer = Buffer
      .fromString(bufferId, "// a source note\nval x = 1")
      .copy(editing = EditingState(List(CursorPosition(0, 4))))
    val editor = baseState.copy(
      persisted = baseState.persisted.copy(
        buffers = Map(bufferId -> buffer.copy(document = buffer.document.copy(language = Some(LanguageId.Scala)))),
        focus = Focus.EditorPane(paneId)
      ),
      runtime = baseState.runtime.copy(uiSurfaces = Nil)
    )

    val opened = CommentRendering.openLensAtCursor(editor)

    commentLens(opened).target shouldBe None
    commentLens(opened).mode shouldBe CommentLensMode.ReadOnly
    component.processEvent(ModalInsertChar('!'), opened) shouldBe ComponentResult.noChange
  }

  it should "ignore edit events even when constructed editable" in {
    val untargeted = withLens(baseState)(_.copy(target = None, mode = CommentLensMode.Editable))

    component.processEvent(ModalInsertChar('!'), untargeted) shouldBe ComponentResult.noChange
    component.processEvent(ModalSubmit, untargeted) shouldBe ComponentResult.noChange
  }

  "Saving an emptied comment draft" should "delete the comment rather than save placeholder text" in {
    val emptied = withLens(baseState)(_.copy(draft = "  ", cursor = 2))

    val saved = stateAfter(component.processEvent(ModalSubmit, emptied), emptied)

    saved.commentLensSurface shouldBe None
    saved.persisted.buffers(bufferId).annotations.documentComments shouldBe Nil
    saved.persisted.buffers(bufferId).document.isDirty shouldBe true
  }

  "Deleting in the comment draft" should "remove a whole emoji grapheme on Backspace" in {
    val thumbsUp  = "\uD83D\uDC4D\uD83C\uDFFD"
    val draft     = s"Hi $thumbsUp"
    val withEmoji = withLens(baseState)(_.copy(draft = draft, cursor = draft.length))

    val deleted = stateAfter(component.processEvent(ModalDeleteBackward, withEmoji), withEmoji)

    commentLens(deleted).draft shouldBe "Hi "
    commentLens(deleted).cursor shouldBe 3
  }

  it should "remove a whole emoji grapheme on Delete" in {
    val thumbsUp  = "\uD83D\uDC4D\uD83C\uDFFD"
    val withEmoji = withLens(baseState)(_.copy(draft = s"$thumbsUp!", cursor = 0))

    val deleted = stateAfter(component.processEvent(ModalDeleteForward, withEmoji), withEmoji)

    commentLens(deleted).draft shouldBe "!"
    commentLens(deleted).cursor shouldBe 0
  }

  it should "snap a cursor inside a surrogate pair to the grapheme boundary before deleting" in {
    val thumbsUp  = "\uD83D\uDC4D"
    val withEmoji = withLens(baseState)(_.copy(draft = s"a$thumbsUp", cursor = 2))

    val deleted = stateAfter(component.processEvent(ModalDeleteBackward, withEmoji), withEmoji)

    commentLens(deleted).draft shouldBe thumbsUp
    commentLens(deleted).cursor shouldBe 0
  }

  "Saving a comment draft" should "change only the targeted one of two identical comments" in {
    val twins      = withComments(baseState, List(comment, comment))
    val secondTwin = withLens(twins)(_.copy(target = Some(CommentLensTarget(1, comment))))
    val edited     = stateAfter(component.processEvent(ModalInsertChar('!'), secondTwin), secondTwin)
    val saved      = stateAfter(component.processEvent(ModalSubmit, edited), edited)

    saved.persisted.buffers(bufferId).annotations.documentComments shouldBe
      List(comment, comment.copy(text = "Initial!"))
  }

  it should "still reach a comment whose range an edit shifted while the lens was open" in {
    val edited  = stateAfter(component.processEvent(ModalInsertChar('!'), baseState), baseState)
    val shifted = DocumentComment(CursorPosition(0, 2), CursorPosition(0, 9), "Initial")
    val moved   = withComments(edited, List(shifted))

    val saved = stateAfter(component.processEvent(ModalSubmit, moved), moved)

    saved.persisted.buffers(bufferId).annotations.documentComments shouldBe List(shifted.copy(text = "Initial!"))
  }

  it should "leave the comments alone when the targeted slot no longer holds the opened comment" in {
    val other    = DocumentComment(CursorPosition(0, 8), CursorPosition(0, 17), "Unrelated")
    val replaced = withComments(baseState, List(other))

    val saved = stateAfter(component.processEvent(ModalSubmit, replaced), replaced)

    saved.commentLensSurface shouldBe None
    saved.persisted.buffers(bufferId).annotations.documentComments shouldBe List(other)
  }

  private def withComments(state: AppState, comments: List[DocumentComment]): AppState =
    val buffer = state.persisted.buffers(bufferId)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(
          bufferId,
          buffer.copy(annotations = buffer.annotations.copy(documentComments = comments))
        )
      )
    )

  private def withLensMode(state: AppState, mode: CommentLensMode): AppState =
    withLens(state)(_.copy(mode = mode))

  private def withLens(state: AppState)(change: CommentLensState => CommentLensState): AppState =
    state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.map {
      case surface if surface.id == lensId =>
        surface.content match
          case SurfaceContent.CommentLens(lens) =>
            surface.copy(content = SurfaceContent.CommentLens(change(lens)))
          case _ => surface
      case surface => surface
    }))

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
