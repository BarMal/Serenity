package com.serenity.perf

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{Event, ResizeEvent}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManagerTestFacade.{createBuffer, switchToPane, updateState}
import com.serenity.state.manager.{CursorViewport, StateManager}
import com.serenity.state.models.{AppState, BufferId, PaneId}
import com.serenity.{TestTemp, setBufferForPane, setCursorPosition, splitPaneHorizontal, splitPaneVertical}
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.{NoOpFactory, NoOpLogger}

/** A live `StateManager` with a prose buffer in each of 2 or 4 panes, resized to the laptop grid, every pane's cursor
  * in the middle of its document and scrolled into view, the way a session that has been reading and editing has them.
  * The first pane takes the keys. Two panes sit side by side; four are two columns of two (#2481).
  */
final private[perf] class SplitPaneFixture private (val manager: StateManager, val panes: List[PaneId])(using
    IORuntime
):

  def current: AppState = manager.getCurrentState.unsafeRunSync()

  /** The state before `event` and after it. */
  def transition(event: Event): (AppState, AppState) =
    val before = current
    manager.applyEvent(event).unsafeRunSync()
    (before, current)

  /** The state before a keystroke and after it. */
  def typing(): (AppState, AppState) = TypingTransition.of(manager)

private[perf] object SplitPaneFixture:

  given Balance = Balance.default

  def open(paneCount: Int)(using IORuntime): SplitPaneFixture =
    require(Set(2, 4)(paneCount), s"$paneCount panes: the fixture builds 2 or 4")
    given LoggerFactory[IO] = NoOpFactory[IO]
    val config              = AppConfig.conventional
    val manager = StateManager(
      NoOpLogger[IO],
      sessionRootOverride = Some(TestTemp.directory("serenity-split-pane-frames")),
      dictionaryCache = WarmDictionary.cacheFor(config)
    ).unsafeRunSync()
    val shown = (for
      _ <- manager.updateState(state => state.copy(persisted = state.persisted.copy(config = config)))
      buffers <- (1 to paneCount).toList.traverse(index =>
        manager.createBuffer(BenchmarkFixtures.loremIpsumProse(paragraphs = 300 + index), None)
      )
      first <- manager.getCurrentState.map(_.persisted.layout.editorPanes.keys.head)
      _     <- manager.setBufferForPane(first, buffers.head)
      shown <- showTheRest(manager, first, buffers)
      _     <- manager.applyEvent(ResizeEvent(LaptopFrameBenchmarks.viewport))
      _     <- shown.traverse_(placeCursor(manager, _))
      _     <- manager.updateState(state => buffers.foldLeft(state)((s, id) => scrolledToCursor(id)(s)))
      _     <- manager.switchToPane(first)
      _     <- manager.runtimeLifecycle.awaitEffects
    yield shown).unsafeRunSync()
    new SplitPaneFixture(manager, shown)

  private def showTheRest(manager: StateManager, first: PaneId, buffers: List[BufferId]): IO[List[PaneId]] =
    buffers match
      case _ :: second :: Nil =>
        manager.splitPaneHorizontal(first, Some(second)).map(first :: _ :: Nil)
      case _ :: second :: third :: fourth :: Nil =>
        for
          right  <- manager.splitPaneHorizontal(first, Some(second))
          below  <- manager.splitPaneVertical(first, Some(third))
          belowR <- manager.splitPaneVertical(right, Some(fourth))
        yield List(first, right, below, belowR)
      case _ => IO.pure(List(first))

  private def placeCursor(manager: StateManager, pane: PaneId): IO[Unit] =
    for
      state <- manager.getCurrentState
      line = state.persisted.layout
        .editorPanes(pane)
        .bufferId
        .flatMap(state.persisted.buffers.get)
        .fold(0)(_.document.content.lineCount / 2)
      _ <- manager.setCursorPosition(pane, line, 40)
    yield ()

  private def scrolledToCursor(id: BufferId)(state: AppState): AppState =
    state.persisted.buffers.get(id).flatMap(buffer => buffer.editing.cursorPositions.headOption.map(buffer -> _)) match
      case Some((buffer, cursor)) =>
        val placed = buffer.copy(viewport = CursorViewport.adjustForCursor(buffer, state, cursor))
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(id, placed)))
      case None => state
