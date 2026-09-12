package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Leaf, Rope}
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class MouseClickSelectionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def makeStateManager() =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

  // `Rope` is sealed, so a test double can no longer extend it directly; it delegates to a real `Leaf`/`Node` tree
  // while itself extending the still-open `Leaf` purely to satisfy the type system -- every method that matters for
  // this test forwards to `delegate` rather than using anything inherited from `Leaf`.
  final class NonCollectingRope(delegate: Rope) extends Leaf(delegate.collect()):
    override def weight: Int =
      delegate.weight

    override def height: Int =
      delegate.height

    override val newlineCount: Int =
      delegate.newlineCount

    override val lastLineLength: Int =
      delegate.lastLineLength

    override val endsWithNewline: Boolean =
      delegate.endsWithNewline

    override def isWeightBalanced: Boolean =
      delegate.isWeightBalanced

    override def isHeightBalanced: Boolean =
      delegate.isHeightBalanced

    override def rebalance: Rope =
      this

    override def index(i: Int): Option[Char] =
      delegate.index(i)

    override def splitAt(index: Int): Option[(Rope, Rope)] =
      delegate.splitAt(index)

    override def lineCount: Int =
      delegate.lineCount

    override def getLine(lineIndex: Int): Option[String] =
      delegate.getLine(lineIndex)

    override def lineColumnToOffset(line: Int, column: Int): Int =
      delegate.lineColumnToOffset(line, column)

    override def offsetToLineColumn(offset: Int): (Int, Int) =
      delegate.offsetToLineColumn(offset)

    override def collect(): String =
      throw AssertionError("mouse word selection should not materialise the whole buffer")

  object NonCollectingRope:
    def apply(delegate: Rope): NonCollectingRope = new NonCollectingRope(delegate)

  "MouseClick" should "create a selection while dragging inside an editor pane" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("alpha\nbravo\ncharlie", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            state.persisted
              .buffers(bufferId)
              .copy(document = state.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MousePress(paneRect.x + 1, paneRect.y + 1)).unsafeRunSync()
    sm.applyEvent(MouseDrag(paneRect.x + 3, paneRect.y + 2)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(CursorPosition(1, 3))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(0, 1), CursorPosition(1, 3)))
    buffer.editing.selections shouldBe Nil
  }

  it should "start a new drag selection from the latest press instead of reusing an old anchor" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("alpha\nbravo\ncharlie", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            state.persisted
              .buffers(bufferId)
              .copy(document = state.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MousePress(paneRect.x + 1, paneRect.y + 1)).unsafeRunSync()
    sm.applyEvent(MouseDrag(paneRect.x + 3, paneRect.y + 2)).unsafeRunSync()
    sm.applyEvent(MousePress(paneRect.x + 2, paneRect.y + 2)).unsafeRunSync()
    sm.applyEvent(MouseDrag(paneRect.x + 5, paneRect.y + 2)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(CursorPosition(1, 5))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(1, 2), CursorPosition(1, 5)))
    buffer.editing.selections shouldBe Nil
  }

  it should "select the clicked word on double click" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("alpha beta gamma", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            state.persisted
              .buffers(bufferId)
              .copy(document = state.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MouseClick(paneRect.x + 7, paneRect.y + 1, clickCount = 2)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(CursorPosition(0, 10))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(0, 6), CursorPosition(0, 10)))
    buffer.editing.selections shouldBe Nil
  }

  it should "select the clicked word on double click without materialising the whole buffer" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("alpha beta gamma", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            state.persisted
              .buffers(bufferId)
              .copy(
                document = state.persisted
                  .buffers(bufferId)
                  .document
                  .copy(content = NonCollectingRope(Rope("alpha beta gamma")), language = Some(LanguageId.Scala))
              )
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MouseClick(paneRect.x + 7, paneRect.y + 1, clickCount = 2)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(CursorPosition(0, 10))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(0, 6), CursorPosition(0, 10)))
    buffer.editing.selections shouldBe Nil
  }

  it should "select the clicked line on triple click" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("alpha\nbeta gamma\ncharlie", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            state.persisted
              .buffers(bufferId)
              .copy(document = state.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MouseClick(paneRect.x + 2, paneRect.y + 2, clickCount = 3)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(CursorPosition(1, 10))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(1, 0), CursorPosition(1, 10)))
    buffer.editing.selections shouldBe Nil
  }

  it should "extend the current selection from the existing anchor on shift-click" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("alpha\nbeta gamma\ncharlie", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            state.persisted
              .buffers(bufferId)
              .copy(
                document = state.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)),
                editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 2)))
              )
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MouseClick(paneRect.x + 4, paneRect.y + 2, shiftDown = true)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(CursorPosition(1, 4))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(0, 2), CursorPosition(1, 4)))
    buffer.editing.selections shouldBe Nil
  }

  it should "preserve the original anchor while extending with shift-drag" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("alpha\nbeta gamma\ncharlie", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            state.persisted
              .buffers(bufferId)
              .copy(
                document = state.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)),
                editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 2)))
              )
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MousePress(paneRect.x + 4, paneRect.y + 2, shiftDown = true)).unsafeRunSync()
    sm.applyEvent(MouseDrag(paneRect.x + 5, paneRect.y + 3, shiftDown = true)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(CursorPosition(2, 5))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(0, 2), CursorPosition(2, 5)))
    buffer.editing.selections shouldBe Nil
  }

  it should "collapse multi-cursor state to the clicked cursor" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("alpha\nbeta\ngamma", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            state.persisted
              .buffers(bufferId)
              .copy(
                document = state.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)),
                editing = state.persisted
                  .buffers(bufferId)
                  .editing
                  .copy(
                    cursors = List(CursorPosition(0, 1), CursorPosition(2, 3)),
                    multiCursorVerticalStates = List(
                      VerticalCursorState(CursorPosition(0, 1), 1, 1.0f),
                      VerticalCursorState(CursorPosition(2, 3), 3, 3.0f)
                    )
                  )
              )
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MouseClick(paneRect.x + 2, paneRect.y + 2)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    buffer.editing.cursors shouldBe List(CursorPosition(1, 2))
    buffer.editing.selection shouldBe None
    buffer.editing.selections shouldBe Nil
    buffer.editing.multiCursorVerticalStates shouldBe Nil
  }

  it should "collapse multi-selection state to a single drag selection" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("alpha\nbeta\ngamma", None).unsafeRunSync()
    val first    = Selection(CursorPosition(0, 0), CursorPosition(0, 2))
    val second   = Selection(CursorPosition(2, 0), CursorPosition(2, 2))
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            state.persisted
              .buffers(bufferId)
              .copy(
                document = state.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)),
                editing = state.persisted
                  .buffers(bufferId)
                  .editing
                  .copy(
                    cursors = List(first.focus, second.focus),
                    selection = Some(first),
                    selections = List(first, second)
                  )
              )
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MousePress(paneRect.x + 1, paneRect.y + 1)).unsafeRunSync()
    sm.applyEvent(MouseDrag(paneRect.x + 3, paneRect.y + 2)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    buffer.editing.cursors shouldBe List(CursorPosition(1, 3))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(0, 1), CursorPosition(1, 3)))
    buffer.editing.selections shouldBe Nil
  }
