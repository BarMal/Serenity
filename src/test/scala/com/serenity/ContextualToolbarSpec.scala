package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.richtext.InlineMark
import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutEngine, SurfaceFrameLayout, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ContextualToolbarSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  "Contextual toolbar" should "toggle on, keep command runner stacked beneath it, and restore toolbar focus when the runner closes" in {
    val stateManager = createStateManager("ContextualToolbarSpec-stack")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val opened         = stateManager.getCurrentState.unsafeRunSync()
    val toolbarSurface = opened.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar"))
    opened.focus shouldBe Focus.Surface(toolbarSurface.id)

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val withRunner = stateManager.getCurrentState.unsafeRunSync()
    val stackIds = LayoutEngine
      .calculateLayoutWithUI(withRunner, withRunner.viewportSize.getOrElse(fail("Expected viewport size")))
      .belowCursorOverlayStack
      .map(_._1)
    stackIds.take(2) shouldBe List(
      toolbarSurface.id,
      withRunner.commandRunnerSurface.getOrElse(fail("Expected command runner")).id
    )

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val afterClose = stateManager.getCurrentState.unsafeRunSync()
    afterClose.focus shouldBe Focus.Surface(toolbarSurface.id)
  }

  it should "execute the focused formatting command on Enter" in {
    val stateManager = createStateManager("ContextualToolbarSpec-enter")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val selection = Selection(CursorPosition(0, 6), CursorPosition(0, 10))
        val nextBuffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha beta"),
            selection = Some(selection),
            cursors = List(selection.focus)
          )
        state.copy(buffers = state.buffers.updated(bufferId, nextBuffer))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(state)
    val buffer   = state.buffers(bufferId)
    buffer.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .map(_.style.marks)
      .shouldBe(Some(Set(InlineMark.Bold)))
  }

  it should "dismiss on Escape and restore editor focus" in {
    val stateManager = createStateManager("ContextualToolbarSpec-escape")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    stateManager.applyEvent(Escape).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.contextualToolbarSurface shouldBe None
    state.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "highlight and execute toolbar items with the mouse" in {
    val stateManager = createStateManager("ContextualToolbarSpec-mouse")

    stateManager.applyEvent(ResizeEvent(ViewportSize(160, 40))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val selection = Selection(CursorPosition(0, 6), CursorPosition(0, 10))
        val nextBuffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha beta"),
            selection = Some(selection),
            cursors = List(selection.focus)
          )
        state.copy(buffers = state.buffers.updated(bufferId, nextBuffer))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val before = stateManager.getCurrentState.unsafeRunSync()
    val point  = toolbarItemPoint(before, itemIndex = 1)

    stateManager.applyEvent(MouseMove(point.x, point.y)).unsafeRunSync()

    toolbarStateFrom(stateManager.getCurrentState.unsafeRunSync()).focusedIndex shouldBe 1

    stateManager.applyEvent(MouseClick(point.x, point.y)).unsafeRunSync()

    val after    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(after)
    after
      .buffers(bufferId)
      .richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .map(_.style.marks)
      .shouldBe(Some(Set(InlineMark.Italic)))
  }

  private case class Point(x: Int, y: Int)

  private def toolbarItemPoint(state: AppState, itemIndex: Int): Point =
    val rect        = toolbarRect(state)
    val contentRect = SurfaceFrameLayout(rect).contentRect
    val slotWidth   = math.max(1, contentRect.width / ContextualToolbar.proseItems.length)
    Point(
      x = contentRect.x + (slotWidth * itemIndex) + math.max(0, slotWidth / 2),
      y = contentRect.y
    )

  private def toolbarRect(state: AppState) =
    val viewport = state.viewportSize.getOrElse(fail("Expected viewport size"))
    val surface  = state.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar surface"))
    LayoutEngine
      .calculateLayoutWithUI(state, viewport)
      .belowCursorOverlayStack
      .collectFirst { case (`surface`.id, rect) => rect }
      .getOrElse(fail("Expected toolbar overlay rect"))

  private def toolbarStateFrom(state: AppState): ContextualToolbarState =
    state.contextualToolbarSurface
      .flatMap {
        _.content match
          case SurfaceContent.ContextualToolbar(toolbarState) => Some(toolbarState)
          case _                                              => None
      }
      .getOrElse(fail("Expected contextual toolbar state"))

  private def activeBufferId(state: AppState): BufferId =
    state.layout.activeEditorPaneId
      .flatMap(state.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .getOrElse(fail("Expected active buffer"))
