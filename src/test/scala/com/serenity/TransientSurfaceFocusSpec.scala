package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.lsp.model.LspPosition
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** #1940: a peek (LSP hover, definition, references) is shown without taking focus, so the key that dismisses it still
  * reaches the editor instead of being swallowed.
  */
class TransientSurfaceFocusSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId = PaneId(0)

  private def makeStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("TransientSurfaceFocusSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def withBuffer(sm: StateManager): BufferId =
    val bufferId = sm.createBuffer("hello\nworld", None).unsafeRunSync()
    sm.setBufferForPane(paneId, bufferId).unsafeRunSync()
    sm.updateState { state =>
      val buffer = state.persisted.buffers(bufferId)
      state.copy(persisted =
        state.persisted.copy(buffers =
          state.persisted.buffers.updated(bufferId, buffer.copy(editing = EditingState(List(CursorPosition(0, 0)))))
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()
    bufferId

  private def hovered(sm: StateManager): AppState =
    sm.applyEvent(LspEvent.LspHoverReceived("def hello: String", CursorPosition(0, 0))).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync()

  private def definitionShown(sm: StateManager): AppState =
    sm.applyEvent(
      LspEvent.LspDefinitionReceived("hello", "file:///workspace/Hello.scala", LspPosition(3, 4), CursorPosition(0, 0))
    ).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync()

  private def text(state: AppState, bufferId: BufferId): String =
    state.persisted.buffers(bufferId).document.content.collect()

  private def caret(state: AppState, bufferId: BufferId): Option[CursorPosition] =
    state.persisted.buffers(bufferId).editing.cursorPositions.headOption

  "An LSP hover peek" should "show without taking focus from the editor" in {
    val sm = makeStateManager()
    withBuffer(sm)

    val shown = hovered(sm)

    shown.peekSurface.map(_.content) shouldBe Some(SurfaceContent.QuickInfo("def hello: String"))
    shown.persisted.focus shouldBe Focus.EditorPane(paneId)
  }

  it should "insert a typed character and dismiss the peek" in {
    val sm       = makeStateManager()
    val bufferId = withBuffer(sm)
    hovered(sm)

    sm.applyEvent(InsertChar('x')).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    text(after, bufferId) shouldBe "xhello\nworld"
    after.peekSurface shouldBe None
    after.persisted.focus shouldBe Focus.EditorPane(paneId)
  }

  it should "close on Escape without the key reaching anything else" in {
    val sm       = makeStateManager()
    val bufferId = withBuffer(sm)
    hovered(sm)

    sm.applyEvent(Escape).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    after.peekSurface shouldBe None
    text(after, bufferId) shouldBe "hello\nworld"
    after.persisted.focus shouldBe Focus.EditorPane(paneId)
  }

  "A definition peek" should "let an arrow key move the caret as it dismisses the peek" in {
    val sm       = makeStateManager()
    val bufferId = withBuffer(sm)
    definitionShown(sm).peekSurface shouldBe defined

    sm.applyEvent(MoveRight).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    caret(after, bufferId) shouldBe Some(CursorPosition(0, 1))
    after.peekSurface shouldBe None
  }

  it should "let a vertical arrow move the caret as it dismisses the peek" in {
    val sm       = makeStateManager()
    val bufferId = withBuffer(sm)
    definitionShown(sm).peekSurface shouldBe defined

    sm.applyEvent(MoveDown).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    caret(after, bufferId).map(_.line) shouldBe Some(1)
    after.peekSurface shouldBe None
  }

  "A peek that does hold focus" should "pass the key that dismisses it on to the editor pane" in {
    val sm       = makeStateManager()
    val bufferId = withBuffer(sm)
    val peekId   = hovered(sm).peekSurface.map(_.id).getOrElse(fail("Expected a hover peek"))
    sm.updateState(_.pushFocus(Focus.Surface(peekId))).unsafeRunSync()

    sm.applyEvent(PeekInputEvent.Navigate(Direction.Right)).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    caret(after, bufferId) shouldBe Some(CursorPosition(0, 1))
    after.peekSurface shouldBe None
    after.persisted.focus shouldBe Focus.EditorPane(paneId)
  }

  "A modal surface" should "keep the keys it is given rather than pass them to the editor" in {
    val sm       = makeStateManager()
    val bufferId = withBuffer(sm)
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val runnerId =
      sm.getCurrentState.unsafeRunSync().commandRunnerSurface.map(_.id).getOrElse(fail("Expected the command runner"))

    sm.applyEvent(InsertChar('x')).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    text(after, bufferId) shouldBe "hello\nworld"
    after.persisted.focus shouldBe Focus.Surface(runnerId)
  }

end TransientSurfaceFocusSpec
