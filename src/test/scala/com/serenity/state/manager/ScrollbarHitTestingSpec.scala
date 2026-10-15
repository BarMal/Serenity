package com.serenity.state.manager

import com.serenity.keystroke.events.{MouseButton, MouseDrag, MousePress}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{ReducerResult, Transition}
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The scrollbar's press and drag handlers as pure transitions over a constructed `AppState`, hit-testing against the
  * very scene the renderer paints the bar from.
  */
class ScrollbarHitTestingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewportSize = ViewportSize(100, 30)
  private val paneId       = PaneId(0)
  private val bufferId     = BufferId(1)

  private def stateOf(lines: Int, topLine: Int = 0): AppState =
    val text    = (0 until lines).map(line => s"line $line").mkString("\n")
    val base    = Buffer.fromString(bufferId, text)
    val buffer  = base.copy(viewport = base.viewport.scrolledTo(topLine, 0, 0))
    val initial = AppState.initial(com.serenity.config.AppConfig.conventional)
    val state = initial.copy(
      runtime = initial.runtime.copy(viewportSize = Some(viewportSize)),
      persisted = initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), paneId)))
        ),
        focus = Focus.EditorPane(paneId)
      )
    )
    LayoutEngine.syncViewportDimensions(state, viewportSize)

  private def sceneOf(state: AppState): UiSceneSnapshot = AuthoritativeUiScene().forState(state, viewportSize)

  private def run[A](state: AppState)(transition: Transition[A]): (ReducerResult, A) =
    MouseTransition.run(state)(transition)

  private def metricsOf(state: AppState): CellMetrics = MouseHitTestGeometry.floatingCellMetrics(state)

  private def stripOf(state: AppState, scene: UiSceneSnapshot): PixelRect =
    ScrollbarLayout.strip(scene.paneLayouts(paneId).contentRect, metricsOf(state))

  /** Where the bar's track starts and how tall it is, in pixels. */
  private def trackOf(state: AppState, scene: UiSceneSnapshot): (Int, Int) =
    val strip = stripOf(state, scene)
    (strip.yPx, strip.heightPx)

  private def thumbOf(state: AppState, scene: UiSceneSnapshot): ScrollThumb =
    ScrollbarGeometry
      .thumb(scene.paneScrolls(paneId).extent, trackOf(state, scene)._2)
      .getOrElse(fail("expected a thumb"))

  private def pressAt(state: AppState, scene: UiSceneSnapshot, alongTrackPx: Int): MousePress =
    val metrics = metricsOf(state)
    val strip   = stripOf(state, scene)
    val pixelX  = strip.xPx + strip.widthPx / 2
    val pixelY  = strip.yPx + alongTrackPx
    MousePress(metrics.toCol(pixelX), metrics.toRow(pixelY), Some(pixelX), Some(pixelY))

  private def dragTo(state: AppState, scene: UiSceneSnapshot, alongTrackPx: Int): MouseDrag =
    val press = pressAt(state, scene, alongTrackPx)
    MouseDrag(press.col, press.row, press.pixelX, press.pixelY)

  private def viewportOf(state: AppState): Viewport = state.persisted.buffers(bufferId).viewport

  "A scrollbar" should "be found under a pointer over its strip and nowhere beside it" in {
    val state   = stateOf(300)
    val scene   = sceneOf(state)
    val strip   = stripOf(state, scene)
    val metrics = metricsOf(state)

    def pressAtPixel(x: Int, y: Int) = MousePress(metrics.toCol(x), metrics.toRow(y), Some(x), Some(y))

    ScrollbarHitTesting.barAt(pressAtPixel(strip.xPx, strip.yPx + 3), state, scene).map(_.paneId) shouldBe Some(paneId)
    ScrollbarHitTesting.barAt(pressAtPixel(strip.rightPx - 1, strip.yPx + 3), state, scene).map(_.paneId) shouldBe
      Some(paneId)
    ScrollbarHitTesting.barAt(pressAtPixel(strip.xPx - 1, strip.yPx + 3), state, scene) shouldBe None
    ScrollbarHitTesting.barAt(pressAtPixel(strip.rightPx, strip.yPx + 3), state, scene) shouldBe None
    ScrollbarHitTesting.barAt(pressAtPixel(strip.xPx, strip.bottomPx), state, scene) shouldBe None
  }

  it should "leave the text in the same cell to the text" in {
    val state   = stateOf(300)
    val scene   = sceneOf(state)
    val strip   = stripOf(state, scene)
    val metrics = metricsOf(state)
    val cell    = metrics.toCol(strip.xPx)
    val beside  = metrics.toPixelX(cell)

    beside should be < strip.xPx
    ScrollbarHitTesting.barAt(
      MousePress(cell, metrics.toRow(strip.yPx + 3), Some(beside), Some(strip.yPx + 3)),
      state,
      scene
    ) shouldBe None
  }

  it should "need the pointer's pixel column to tell, and decline without it" in {
    val state   = stateOf(300)
    val scene   = sceneOf(state)
    val strip   = stripOf(state, scene)
    val metrics = metricsOf(state)

    ScrollbarHitTesting.barAt(
      MousePress(metrics.toCol(strip.xPx), metrics.toRow(strip.yPx + 3)),
      state,
      scene
    ) shouldBe None
  }

  private def terminalState(lines: Int, topLine: Int = 0): AppState =
    val state = stateOf(lines, topLine)
    LayoutEngine.syncViewportDimensions(
      state.copy(runtime = state.runtime.copy(capabilities = com.serenity.frontend.FrontendCapabilities.tui())),
      viewportSize
    )

  "A terminal's scrollbar" should "be its reserved column, found by cell" in {
    val state  = terminalState(300)
    val scene  = sceneOf(state)
    val column = scene.paneLayouts(paneId).scrollbarRect

    scene.paneScrolls should contain key paneId
    ScrollbarHitTesting.barAt(MousePress(column.x, column.y + 2), state, scene).map(_.paneId) shouldBe Some(paneId)
    ScrollbarHitTesting.barAt(MousePress(column.x - 1, column.y + 2), state, scene) shouldBe None
    ScrollbarHitTesting.barAt(MousePress(column.x, column.bottom), state, scene) shouldBe None
  }

  it should "page the view when its track is pressed below the thumb, and take hold of the thumb where it is pressed" in {
    val state  = terminalState(300)
    val scene  = sceneOf(state)
    val column = scene.paneLayouts(paneId).scrollbarRect
    val extent = scene.paneScrolls(paneId).extent
    val thumb  = ScrollbarGeometry.thumb(extent, column.height * 2, 2).getOrElse(fail("expected a thumb"))

    val (paged, _) = run(state)(ScrollbarHitTesting.press(MousePress(column.x, column.bottom - 1), state, scene))
    val (held, _) =
      run(state)(ScrollbarHitTesting.press(MousePress(column.x, column.y + thumb.offsetPx / 2), state, scene))

    viewportOf(paged.state).topLine shouldBe extent.pageRows
    held.state.runtime.pointerGesture.scrollbarGrab.map(_.paneId) shouldBe Some(paneId)
  }

  "A press on the thumb" should "take hold of it where it was pressed, without scrolling" in {
    val state = stateOf(300)
    val scene = sceneOf(state)
    val thumb = thumbOf(state, scene)

    val (result, claimed) =
      run(state)(ScrollbarHitTesting.press(pressAt(state, scene, thumb.offsetPx + 10), state, scene))

    claimed shouldBe true
    result.effects shouldBe Nil
    result.state.runtime.pointerGesture.scrollbarGrab shouldBe Some(ScrollbarGrab(paneId, 10))
    viewportOf(result.state) shouldBe viewportOf(state)
  }

  "A press below the thumb" should "page the view down and hold nothing" in {
    val state  = stateOf(300)
    val scene  = sceneOf(state)
    val extent = scene.paneScrolls(paneId).extent
    val thumb  = thumbOf(state, scene)

    val (result, claimed) = run(state)(ScrollbarHitTesting.press(pressAt(state, scene, thumb.endPx + 30), state, scene))

    claimed shouldBe true
    viewportOf(result.state).topLine shouldBe extent.pageRows
    viewportOf(result.state).topVisualLine shouldBe 0
    result.state.runtime.pointerGesture.scrollbarGrab shouldBe None
  }

  "A press above the thumb" should "page the view back up" in {
    val state  = stateOf(300, topLine = 100)
    val scene  = sceneOf(state)
    val extent = scene.paneScrolls(paneId).extent

    val (result, claimed) = run(state)(ScrollbarHitTesting.press(pressAt(state, scene, alongTrackPx = 2), state, scene))

    claimed shouldBe true
    viewportOf(result.state).topLine shouldBe 100 - extent.pageRows
  }

  it should "stop at the top of the document" in {
    val state = stateOf(300, topLine = 5)
    val scene = sceneOf(state)

    val (result, _) = run(state)(ScrollbarHitTesting.press(pressAt(state, scene, alongTrackPx = 0), state, scene))

    viewportOf(result.state).topLine shouldBe 0
  }

  "A drag with the thumb held" should "carry the view to the pointer's place along the track" in {
    val state        = stateOf(300)
    val scene        = sceneOf(state)
    val extent       = scene.paneScrolls(paneId).extent
    val thumb        = thumbOf(state, scene)
    val (_, trackPx) = trackOf(state, scene)
    val held = run(state)(ScrollbarHitTesting.press(pressAt(state, scene, thumb.offsetPx + 10), state, scene))._1.state
    val travel = trackPx - thumb.lengthPx

    val (toEnd, claimedEnd) = run(held)(ScrollbarHitTesting.drag(dragTo(held, scene, travel + 10), held, scene))
    val (toMiddle, _)       = run(held)(ScrollbarHitTesting.drag(dragTo(held, scene, travel / 2 + 10), held, scene))
    val (toStart, _) = run(toEnd.state)(ScrollbarHitTesting.drag(dragTo(toEnd.state, scene, 0), toEnd.state, scene))

    claimedEnd shouldBe true
    viewportOf(toEnd.state).topLine shouldBe extent.lastFirstRow
    viewportOf(toMiddle.state).topLine shouldBe (extent.lastFirstRow / 2 +- 1)
    viewportOf(toStart.state).topLine shouldBe 0
  }

  it should "keep holding the thumb as the view moves" in {
    val state = stateOf(300)
    val scene = sceneOf(state)
    val thumb = thumbOf(state, scene)
    val held  = run(state)(ScrollbarHitTesting.press(pressAt(state, scene, thumb.offsetPx + 10), state, scene))._1.state

    val (dragged, _) = run(held)(ScrollbarHitTesting.drag(dragTo(held, scene, thumb.offsetPx + 100), held, scene))

    dragged.state.runtime.pointerGesture.scrollbarGrab shouldBe Some(ScrollbarGrab(paneId, 10))
  }

  it should "change nothing while the pointer stays where the thumb already is" in {
    val state = stateOf(300)
    val scene = sceneOf(state)
    val thumb = thumbOf(state, scene)
    val held  = run(state)(ScrollbarHitTesting.press(pressAt(state, scene, thumb.offsetPx + 10), state, scene))._1.state

    val (result, claimed) = run(held)(ScrollbarHitTesting.drag(dragTo(held, scene, thumb.offsetPx + 10), held, scene))

    claimed shouldBe true
    result.state shouldBe theSameInstanceAs(held)
  }

  "A drag with nothing held" should "be left to whoever else wants it" in {
    val state = stateOf(300)
    val scene = sceneOf(state)

    run(state)(ScrollbarHitTesting.drag(dragTo(state, scene, 40), state, scene)) shouldBe ((
      ReducerResult(state, Nil),
      false
    ))
  }

  "A press in the text" should "be declined" in {
    val state = stateOf(300)
    val scene = sceneOf(state)
    val rect  = scene.paneLayouts(paneId).contentRect

    run(state)(ScrollbarHitTesting.press(MousePress(rect.x + 3, rect.y + 2), state, scene)) shouldBe
      ((ReducerResult(state, Nil), false))
  }

  "A press on a bar with nothing to scroll" should "be claimed so it does not reach the text, and change nothing" in {
    val state = stateOf(5)
    val scene = sceneOf(state)

    run(state)(ScrollbarHitTesting.press(pressAt(state, scene, alongTrackPx = 40), state, scene)) shouldBe
      ((ReducerResult(state, Nil), true))
  }

  "A press with another button" should "be declined" in {
    val state = stateOf(300)
    val scene = sceneOf(state)
    val press = pressAt(state, scene, alongTrackPx = 40).copy(button = MouseButton.Secondary)

    run(state)(ScrollbarHitTesting.press(press, state, scene)) shouldBe ((ReducerResult(state, Nil), false))
  }
