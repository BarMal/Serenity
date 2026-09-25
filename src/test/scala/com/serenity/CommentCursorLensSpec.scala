package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** #1550: the floating comment lens (#1222) only ever opened from a mouse click, and a click's cursor move never
  * dismissed a lens once one was open. Both directions should behave identically for a keyboard-driven cursor move:
  * moving the cursor into a commented range opens the same read-only lens a click does, and moving it back out (by any
  * further interaction, not just a click) dismisses it.
  */
class CommentCursorLensSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def makeStateManager() =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

  private val comment = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 5), "A note about hello")

  /** Cursor starts at column 6 (just past "hello"), one step outside the comment's inclusive `[0, 5]` range. */
  private def withCommentedBuffer(sm: StateManager): BufferId =
    val bufferId = sm.bufferManager.createBuffer("hello world", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      val buffer = state.persisted.buffers(bufferId)
      state.copy(persisted =
        state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            buffer.copy(
              document = buffer.document.copy(language = Some(LanguageId.Scala)),
              annotations = buffer.annotations.copy(documentComments = List(comment)),
              editing = EditingState(List(CursorPosition(0, 6)))
            )
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()
    bufferId

  private def commentLensState(state: AppState): Option[CommentLensState] =
    state.commentLensSurface.flatMap {
      _.content match
        case SurfaceContent.CommentLens(lens) => Some(lens)
        case _                                => None
    }

  "Moving the cursor into a commented range with the keyboard" should "open the comment lens read-only" in {
    val sm       = makeStateManager()
    val bufferId = withCommentedBuffer(sm)

    sm.applyEvent(MoveLeft).unsafeRunSync() // column 6 -> 5, entering the comment's inclusive [0, 5] range

    val state  = sm.getCurrentState.unsafeRunSync()
    val buffer = state.persisted.buffers(bufferId)
    buffer.editing.cursorPositions.headOption shouldBe Some(CursorPosition(0, 5))

    val lens = commentLensState(state).getOrElse(fail("Expected the comment lens to open on keyboard cursor move"))
    lens.mode shouldBe CommentLensMode.ReadOnly
    lens.target shouldBe Some(comment)
    state.persisted.focus shouldBe Focus.Surface(SurfaceId("comment-lens"))
  }

  it should "not open a lens for a keyboard cursor move that stays outside the commented range" in {
    val sm = makeStateManager()
    withCommentedBuffer(sm)

    sm.applyEvent(MoveRight).unsafeRunSync() // column 6 -> 7, still outside [0, 5]

    sm.getCurrentState.unsafeRunSync().commentLensSurface shouldBe None
  }

  "A comment lens opened by a keyboard cursor move into the range" should
    "close once the cursor moves back out via the keyboard" in {
      val sm       = makeStateManager()
      val bufferId = withCommentedBuffer(sm)

      sm.applyEvent(MoveLeft).unsafeRunSync() // column 6 -> 5, entering the range
      commentLensState(sm.getCurrentState.unsafeRunSync()) shouldBe defined

      // Column 5 is inside the editor pane, so re-focus it before moving further -- the lens itself now holds focus.
      sm.updateState(state => state.copy(persisted = state.persisted.copy(focus = Focus.EditorPane(PaneId(0)))))
        .unsafeRunSync()
      sm.applyEvent(MoveRight).unsafeRunSync() // column 5 -> 6, leaving the range

      val state = sm.getCurrentState.unsafeRunSync()
      state.persisted.buffers(bufferId).editing.cursorPositions.headOption shouldBe Some(CursorPosition(0, 6))
      state.commentLensSurface shouldBe None
    }

  "A comment lens opened by a mouse click" should "close once a click elsewhere moves the cursor out of range" in {
    import com.serenity.ui.layout.*

    val sm       = makeStateManager()
    val bufferId = withCommentedBuffer(sm)

    def paneRect(): LayoutRect =
      val state  = sm.getCurrentState.unsafeRunSync()
      val layout = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
      LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    val openRect = paneRect()
    sm.applyEvent(MouseClick(openRect.x + 2, openRect.y + 1)).unsafeRunSync()
    commentLensState(sm.getCurrentState.unsafeRunSync()) shouldBe defined

    // Well below the single line of text -- and below the floating lens rendered above it -- so this click reaches
    // the editor rather than being swallowed as landing inside the still-open floating lens.
    val elsewhereRect = paneRect()
    sm.applyEvent(MouseClick(elsewhereRect.x + 8, elsewhereRect.y + 10)).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    state.persisted.buffers(bufferId).editing.cursorPositions.headOption should not be Some(CursorPosition(0, 2))
    state.commentLensSurface shouldBe None
  }

end CommentCursorLensSpec
