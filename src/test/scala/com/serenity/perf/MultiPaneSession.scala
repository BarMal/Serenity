package com.serenity.perf

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{Event, ResizeEvent}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.{createBuffer, switchToPane, updateState}
import com.serenity.state.models.{AppState, BufferId, PaneId}
import com.serenity.{TestTemp, setBufferForPane, setCursorPosition, splitPaneHorizontal, splitPaneVertical}
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.{NoOpFactory, NoOpLogger}

/** A live `StateManager` with one prose buffer in each of 1, 2 or 4 panes, set up the way
  * [[LaptopFrameBenchmarks.stateManagerHolding]] does for one: resized to the laptop grid, every pane's cursor in the
  * middle of its document and scrolled into view, so editing and scrolling skip no work a real session pays for. The
  * first pane takes the keys. Two panes are side by side; four are two columns of two (#2478).
  */
final class MultiPaneSession private (val manager: StateManager, val panes: List[PaneId])(using IORuntime):

  def current: AppState = manager.getCurrentState.unsafeRunSync()

  /** The state before a keystroke and after it, as [[TypingTransition]] gives them for one pane. */
  def typing(): (AppState, AppState) = TypingTransition.of(manager)

  /** The state before `event` and after it. */
  def transition(event: Event): (AppState, AppState) =
    val before = current
    manager.applyEvent(event).unsafeRunSync()
    (before, current)

object MultiPaneSession:

  given Balance = Balance.default

  def open(panes: Int, size: MultiBufferFixture.Size)(using IORuntime): MultiPaneSession =
    require(Set(1, 2, 4)(panes), s"$panes panes: the rows ask for 1, 2 or 4")
    given LoggerFactory[IO] = NoOpFactory[IO]
    val config              = AppConfig.conventional
    val manager = StateManager(
      NoOpLogger[IO],
      sessionRootOverride = Some(TestTemp.directory("serenity-scaling-benchmarks")),
      dictionaryCache = WarmDictionary.cacheFor(config)
    ).unsafeRunSync()
    val opened = (for
      _ <- manager.updateState(state => state.copy(persisted = state.persisted.copy(config = config)))
      buffers <- (1 to panes).toList.traverse(index =>
        manager.createBuffer(MultiBufferFixture.document(MultiBufferFixture.Kind.Prose, index, size), None)
      )
      first <- manager.getCurrentState.map(_.persisted.layout.editorPanes.keys.head)
      _     <- manager.setBufferForPane(first, buffers.head)
      shown <- showTheRest(manager, first, buffers)
      _     <- manager.applyEvent(ResizeEvent(LaptopFrameBenchmarks.viewport))
      _     <- shown.traverse_(placeCursor(manager, _))
      _ <- manager.updateState(state =>
        buffers.foldLeft(state)((s, id) => LaptopFrameBenchmarks.scrolledToCursor(id)(s))
      )
      _ <- manager.switchToPane(first)
      // Every fixture is built before the first benchmark is timed, so an analysis still running here would overlap it.
      _ <- manager.runtimeLifecycle.awaitEffects
    yield shown).unsafeRunSync()
    new MultiPaneSession(manager, opened)

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
