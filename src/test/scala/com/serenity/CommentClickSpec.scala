package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.CommentDisplayMode
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Mouse-hit-testing specs for #1222: clicking a highlighted comment range opens the floating comment lens read-only,
  * and a further click inside its body transitions it to the existing editable state.
  */
class CommentClickSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def makeStateManager() =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

  private val comment = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 5), "A note about hello")

  private def withCommentedBuffer(sm: StateManager): BufferId =
    val bufferId = sm.createBuffer("hello world").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      val buffer = state.persisted.buffers(bufferId)
      state.copy(persisted =
        state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            buffer.copy(
              document = buffer.document.copy(language = Some(LanguageId.Scala)),
              annotations = buffer.annotations.copy(documentComments = List(comment))
            )
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()
    bufferId

  /** The point that clicks buffer column `column` on line 0, following the same `paneRect.x + column`, `paneRect.y + 1`
    * convention `MouseClickSpec`'s word/line-selection specs use.
    */
  private def bufferColumnPoint(sm: StateManager, column: Int): (Int, Int) =
    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))
    (paneRect.x + column, paneRect.y + 1)

  private def commentLensState(state: AppState): Option[CommentLensState] =
    state.commentLensSurface.flatMap {
      _.content match
        case SurfaceContent.CommentLens(lens) => Some(lens)
        case _                                => None
    }

  "Clicking a commented range in floating display mode" should "open the comment lens read-only" in {
    val sm = makeStateManager()
    withCommentedBuffer(sm)
    val (x, y) = bufferColumnPoint(sm, 2) // inside the comment's [0, 5] range

    sm.applyEvent(MouseClick(x, y)).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val lens  = commentLensState(state).getOrElse(fail("Expected comment lens"))
    lens.mode shouldBe CommentLensMode.ReadOnly
    lens.target shouldBe Some(comment)
    state.persisted.focus shouldBe Focus.Surface(SurfaceId("comment-lens"))
  }

  it should "still move the cursor to the clicked position" in {
    val sm       = makeStateManager()
    val bufferId = withCommentedBuffer(sm)
    val (x, y)   = bufferColumnPoint(sm, 2)

    sm.applyEvent(MouseClick(x, y)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(CursorPosition(0, 2))
  }

  it should "not open a lens for a click outside any commented range" in {
    val sm = makeStateManager()
    withCommentedBuffer(sm)
    val (x, y) = bufferColumnPoint(sm, 8) // inside "world", outside the comment's [0, 5] range

    sm.applyEvent(MouseClick(x, y)).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().commentLensSurface shouldBe None
  }

  it should "not open a lens on a double-click word selection inside a commented range" in {
    val sm = makeStateManager()
    withCommentedBuffer(sm)
    val (x, y) = bufferColumnPoint(sm, 2)

    sm.applyEvent(MouseClick(x, y, clickCount = 2)).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().commentLensSurface shouldBe None
  }

  it should "not open a lens on a shift-click range selection inside a commented range" in {
    val sm = makeStateManager()
    withCommentedBuffer(sm)
    val (x, y) = bufferColumnPoint(sm, 2)

    sm.applyEvent(MouseClick(x, y, shiftDown = true)).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().commentLensSurface shouldBe None
  }

  it should "not open a lens when margin display mode is selected instead" in {
    val sm = makeStateManager()
    withCommentedBuffer(sm)
    sm.updateState(state =>
      state.copy(persisted =
        state.persisted.copy(config = state.persisted.config.withCommentDisplayMode(CommentDisplayMode.Margin))
      )
    ).unsafeRunSync()
    val (x, y) = bufferColumnPoint(sm, 2)

    sm.applyEvent(MouseClick(x, y)).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().commentLensSurface shouldBe None
  }

  "A read-only floating comment lens" should "transition to editable on a click inside its body" in {
    val sm = makeStateManager()
    withCommentedBuffer(sm)
    val (openX, openY) = bufferColumnPoint(sm, 2)
    sm.applyEvent(MouseClick(openX, openY)).unsafeRunSync()

    val opened = sm.getCurrentState.unsafeRunSync()
    commentLensState(opened).map(_.mode) shouldBe Some(CommentLensMode.ReadOnly)
    val (x, y) = lensBodyPoint(opened)

    sm.applyEvent(MouseClick(x, y)).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    commentLensState(after).map(_.mode) shouldBe Some(CommentLensMode.Editable)
    commentLensState(after).map(_.target) shouldBe Some(Some(comment))
    after.persisted.focus shouldBe Focus.Surface(SurfaceId("comment-lens"))
  }

  it should "remain dismissible with Escape while read-only" in {
    val sm = makeStateManager()
    withCommentedBuffer(sm)
    val (x, y) = bufferColumnPoint(sm, 2)
    sm.applyEvent(MouseClick(x, y)).unsafeRunSync()
    commentLensState(sm.getCurrentState.unsafeRunSync()) shouldBe defined

    sm.applyEvent(Escape).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().commentLensSurface shouldBe None
  }

  private def lensBodyPoint(state: AppState): (Int, Int) =
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val surface  = state.commentLensSurface.getOrElse(fail("Expected comment lens surface"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val contract = EditorLayoutContract.from(state, viewport, layout)
    val rect     = contract.overlayRect(surface.id).getOrElse(fail("Expected comment lens overlay rect"))
    (rect.x, rect.y)

end CommentClickSpec
