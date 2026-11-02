package com.serenity.perf

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ConcurrentHashMap, Executor, Semaphore, TimeUnit}

import scala.jdk.CollectionConverters.*

import com.serenity.state.manager.{RenderCaches, ScrollCounting}
import com.serenity.state.models.{AppState, PaneId}
import com.serenity.ui.layout.PaneScroll

/** How long each pane's scrollbar waits for its late count (#2449): the first frame of a state is drawn with the
  * counting on `executor`, and every pane whose document is over the exact limit reports when its lines are all
  * measured. The panes' counts share one thread in the app, so the last bar's wait is the sum of those before it.
  */
private[perf] object ScrollbarTiming:

  private val Patience = TimeUnit.SECONDS.toNanos(60)

  def withScrollbar(state: AppState): AppState =
    state.copy(persisted = state.persisted.copy(config = state.persisted.config.withScrollbar(true)))

  /** The panes whose lines the frame does not all measure and whose bar is not estimated at once: a document of more
    * lines than its pane shows and no more than `PaneScroll.ExactLineLimit`.
    */
  private def lateBars(state: AppState): List[PaneId] =
    state.persisted.layout.orderedPaneIds.filter { paneId =>
      state.persisted.layout.editorPanes
        .get(paneId)
        .flatMap(_.bufferId)
        .flatMap(state.persisted.buffers.get)
        .exists(buffer =>
          buffer.document.content.lineCount > buffer.viewport.visibleLines &&
            buffer.document.content.lineCount <= PaneScroll.ExactLineLimit
        )
    }

  /** The nanoseconds from the start of the frame to each late bar's count, from fresh caches so nothing is counted yet.
    */
  def waits(state: AppState, executor: Executor): Map[PaneId, Long] =
    val stamps  = new ConcurrentHashMap[PaneId, java.lang.Long]()
    val arrived = new Semaphore(0)
    val started = new AtomicReference(0L)
    val counting = ScrollCounting.deferred(
      executor,
      paneId =>
        val _ = stamps.put(paneId, System.nanoTime() - started.get)
        arrived.release()
    )
    val caches = RenderCaches.create(scrollCounting = counting)
    started.set(System.nanoTime())
    val _        = LaptopFrameBenchmarks.renderedFrame(state, caches, LaptopFrameBenchmarks.frameImage())
    val expected = lateBars(state)
    val all      = arrived.tryAcquire(expected.size, Patience, TimeUnit.NANOSECONDS)
    assert(all, s"only ${stamps.size} of ${expected.size} late bars were counted within a minute")
    stamps.asScala.view.mapValues(_.longValue).toMap

  def lastBarNanos[K](waits: Map[K, Long]): Long = waits.values.max

  /** The waits from the first bar to the last. */
  def arrivalOrder[K](waits: Map[K, Long]): List[Long] = waits.values.toList.sorted
