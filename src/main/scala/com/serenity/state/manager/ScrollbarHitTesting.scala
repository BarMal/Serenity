package com.serenity.state.manager

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.keystroke.events.{MouseButton, MouseClick, MouseDrag, MouseInputEvent, MousePress}
import com.serenity.state.models.{AppState, PaneId, ScrollbarGrab}
import com.serenity.state.reducers.{ReducerResult, Transition}
import com.serenity.ui.layout.{
  CellMetrics,
  PaneScroll,
  PixelRect,
  ScrollbarGeometry,
  ScrollbarLayout,
  TrackHit,
  UiSceneSnapshot
}

/** State the event pipeline exposes for scrolling an editor pane from its scrollbar, as a capability record like
  * [[TabBarDragHitTestingPort]].
  */
final private[manager] case class ScrollbarHitTestingPort(
    currentState: IO[AppState],
    applyReducerResult: (ReducerResult, AppState) => IO[Unit],
    authoritativeScene: AuthoritativeUiScene
)

/** Press, drag and click on an editor pane's scrollbar. A press on the thumb takes hold of it (`scrollbarGrab`) and
  * each `MouseDrag` after it carries the thumb to the pointer; a press on the track pages towards the pointer. A bar
  * the pointer is over claims every press and click on it, so none of them reaches the text beside it.
  *
  * The scroll position comes from the same [[UiSceneSnapshot]] the renderer paints the bar from, so the thumb under the
  * pointer is the thumb on screen.
  */
final private[manager] class ScrollbarHitTesting(port: ScrollbarHitTestingPort):

  def handleScrollbarPress(press: MousePress, state: AppState): IO[Boolean] =
    commit(state)(ScrollbarHitTesting.press(press, state, _))

  def handleScrollbarDrag(drag: MouseDrag, state: AppState): IO[Boolean] =
    commit(state)(ScrollbarHitTesting.drag(drag, state, _))

  def claimsClick(click: MouseClick, state: AppState): Boolean =
    scene(state).exists(ScrollbarHitTesting.barAt(click, state, _).isDefined)

  private def scene(state: AppState): Option[UiSceneSnapshot] =
    state.runtime.viewportSize.map(port.authoritativeScene.forState(state, _))

  private def commit(state: AppState)(transition: UiSceneSnapshot => Transition[Boolean]): IO[Boolean] =
    scene(state).fold(IO.pure(false))(found =>
      MouseTransition.commit(port.currentState, port.applyReducerResult)(transition(found))
    )

private[manager] object ScrollbarHitTesting:

  /** The bar under a pointer: the pane it scrolls, the strip it occupies in pixels, and where that pane is scrolled. A
    * terminal bar is placed to half a row, so its track is counted in half-rows.
    */
  final case class Bar(paneId: PaneId, strip: PixelRect, scroll: PaneScroll, terminal: Boolean):
    private def unitsPerPixel: Int   = if terminal then 2 else 1
    def trackTopPx: Int              = strip.yPx
    def trackHeightPx: Int           = strip.heightPx * unitsPerPixel
    def alongTrack(pixels: Int): Int = pixels * unitsPerPixel
    def minThumbPx: Int              = if terminal then 2 else ScrollbarGeometry.MinThumbPx

  private def isTerminal(state: AppState): Boolean = state.runtime.capabilities.isCellGrid

  /** A terminal's pixel is a cell, so its bar and pointer are measured in cells. */
  private def metricsOf(state: AppState): CellMetrics =
    if isTerminal(state) then CellMetrics.cellUnit else MouseHitTestGeometry.floatingCellMetrics(state)

  /** Needs the pointer's pixel column: the strip is thinner than the cell under it, so a cell alone cannot say whether
    * the pointer is on the bar or on the text beside it.
    */
  def barAt(event: MouseInputEvent, state: AppState, scene: UiSceneSnapshot): Option[Bar] =
    val metrics = metricsOf(state)
    for
      pixelX <- event.pixelX.orElse(Option.when(isTerminal(state))(event.col))
      pixelY = event.pixelY.getOrElse(metrics.toPixelY(event.row) + metrics.lineHeight / 2)
      bar <- scene.paneScrolls.keysIterator.flatMap(barOf(_, scene, metrics)).find(_.strip.contains(pixelX, pixelY))
    yield bar

  private def barOf(paneId: PaneId, scene: UiSceneSnapshot, metrics: CellMetrics): Option[Bar] =
    for
      layout <- scene.paneLayouts.get(paneId)
      scroll <- scene.paneScrolls.get(paneId)
    yield Bar(paneId, ScrollbarLayout.stripOf(layout, metrics), scroll, layout.scrollbarRect.width > 0)

  def press(press: MousePress, state: AppState, scene: UiSceneSnapshot): Transition[Boolean] =
    if press.button != MouseButton.Primary then Transition.pure(false)
    else
      barAt(press, state, scene) match
        case None => Transition.pure(false)
        case Some(bar) =>
          val pointerPx = pointerAlong(press, bar, metricsOf(state))
          ScrollbarGeometry.hit(bar.scroll.extent, bar.trackHeightPx, pointerPx, bar.minThumbPx) match
            case None => Transition.pure(true)
            case Some(hit) =>
              val grab = hit match
                case TrackHit.OnThumb(offsetPx) => Some(ScrollbarGrab(bar.paneId, offsetPx))
                case _                          => None
              Transition
                .modify(current =>
                  holding(scrolledTo(current, bar, ScrollbarGeometry.firstRowAfterPress(bar.scroll.extent, hit)), grab)
                )
                .as(true)

  def drag(drag: MouseDrag, state: AppState, scene: UiSceneSnapshot): Transition[Boolean] =
    if drag.button != MouseButton.Primary then Transition.pure(false)
    else
      val held = for
        grab <- state.runtime.pointerGesture.scrollbarGrab
        bar  <- barOf(grab.paneId, scene, metricsOf(state))
      yield (grab, bar)
      held match
        case None => Transition.pure(false)
        case Some((grab, bar)) =>
          val pointerPx = pointerAlong(drag, bar, metricsOf(state))
          val firstRow =
            ScrollbarGeometry.firstRowForDrag(
              bar.scroll.extent,
              bar.trackHeightPx,
              pointerPx,
              grab.grabOffsetPx,
              bar.minThumbPx
            )
          Transition.modify(scrolledTo(_, bar, firstRow)).as(true)

  /** Pixels down the track, from the event's own pixel row when it has one: a cell row is too coarse to drag a thumb
    * by.
    */
  private def pointerAlong(event: MouseInputEvent, bar: Bar, metrics: CellMetrics): Int =
    bar.alongTrack(event.pixelY.getOrElse(metrics.toPixelY(event.row) + metrics.lineHeight / 2) - bar.trackTopPx)

  private def holding(state: AppState, grab: Option[ScrollbarGrab]): AppState =
    val pointer = state.runtime.pointerGesture
    if pointer.scrollbarGrab == grab then state
    else state.copy(runtime = state.runtime.copy(pointerGesture = pointer.copy(scrollbarGrab = grab)))

  private def scrolledTo(state: AppState, bar: Bar, firstRow: Int): AppState =
    val (topLine, topVisualLine) = bar.scroll.viewportTop(firstRow)
    val scrolled = for
      bufferId <- state.persisted.layout.editorPanes.get(bar.paneId).flatMap(_.bufferId)
      buffer   <- state.persisted.buffers.get(bufferId)
      viewport = buffer.viewport
      if viewport.topLine != topLine || viewport.topVisualLine != topVisualLine
    yield state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(
          bufferId,
          buffer.copy(viewport = viewport.scrolledTo(topLine, viewport.leftColumn, topVisualLine))
        )
      )
    )
    scrolled.getOrElse(state)
