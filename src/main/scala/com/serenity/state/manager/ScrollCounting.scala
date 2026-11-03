package com.serenity.state.manager

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import java.util.concurrent.{ConcurrentHashMap, Executor}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import com.serenity.frontend.FrontendCapabilities
import com.serenity.state.models.PaneId
import com.serenity.ui.layout.{PaneScroll, ScrollKey, VisualRowCounts}

/** How a pane's scrollbar gets the row count of the whole document (#2445). `Inline` measures every line inside the
  * frame that needs the bar, which is what the scene has always done. `Deferred` measures only the lines on screen
  * there, leaves the bar out of the scene, and measures the rest on an executor.
  */
sealed trait ScrollCounting:

  /** The scene just handed out has `paneId`'s bar waiting on `rows`, laid out as `key` says. */
  private[manager] def count(paneId: PaneId, rows: VisualRowCounts, key: ScrollKey): Unit

  /** The scene just handed out has `paneId`'s bar counted, so nothing is left for an earlier count to do. */
  private[manager] def counted(paneId: PaneId, scroll: PaneScroll, key: ScrollKey): Unit

  /** The bar `paneId` last had exactly, to stay on show while the pane is resized and the count for its new width has
    * not landed. Not offered for another text: that total would be wrong.
    */
  private[manager] def shownWhile(paneId: PaneId, key: ScrollKey): Option[PaneScroll]

  /** The scene just handed out has a bar for exactly `paneIds`: a pane that lost its bar or was closed needs no count.
    */
  private[manager] def keepOnly(paneIds: Set[PaneId]): Unit

  private[manager] def isDeferred: Boolean

object ScrollCounting:

  /** How long a window must keep its size before the rows at that size are counted. 150 ms is what the repo already
    * waits for editing to settle (`document.analysis_debounce_ms`); it is a guess for resizing, not a measurement.
    */
  val ResizeQuietPeriod: FiniteDuration = 150.millis

  case object Inline extends ScrollCounting:
    private[manager] def count(paneId: PaneId, rows: VisualRowCounts, key: ScrollKey): Unit = ()
    private[manager] def counted(paneId: PaneId, scroll: PaneScroll, key: ScrollKey): Unit  = ()
    private[manager] def shownWhile(paneId: PaneId, key: ScrollKey): Option[PaneScroll]     = None
    private[manager] def keepOnly(paneIds: Set[PaneId]): Unit                               = ()
    private[manager] def isDeferred: Boolean                                                = false

  /** `onCounted` is told, from the executor's thread, which pane's lines are now all measured. A count that a newer
    * scene for the same pane replaced stops before its next line and tells nobody. A count for a new width waits until
    * `quiet` says the width has settled.
    */
  def deferred(executor: Executor, onCounted: PaneId => Unit, quiet: QuietTimer = QuietTimer.shared): ScrollCounting =
    new Deferred(executor, onCounted, quiet)

  /** A graphical frontend counts late and tells `notice`; a terminal draws its bar from the same scene but has no
    * repaint of a bar alone, so it keeps counting inside the frame.
    */
  def forFrontend(capabilities: FrontendCapabilities, executor: Executor, notice: ScrollCountNotice): ScrollCounting =
    if capabilities.isCellGrid then Inline else deferred(executor, notice.deliver)

  final private class Deferred(executor: Executor, onCounted: PaneId => Unit, quiet: QuietTimer) extends ScrollCounting:

    final private case class Wanted(rows: VisualRowCounts, replaced: AtomicBoolean, timer: AtomicReference[() => Unit]):

      def cancel(): Unit =
        replaced.set(true)
        timer.get()()

    final private case class Exact(scroll: PaneScroll, key: ScrollKey)

    private val wanted    = new ConcurrentHashMap[PaneId, Wanted]()
    private val lastExact = new ConcurrentHashMap[PaneId, Exact]()
    private val widths    = new ConcurrentHashMap[PaneId, Integer]()

    private[manager] def isDeferred: Boolean = true

    private[manager] def keepOnly(paneIds: Set[PaneId]): Unit =
      wanted.keySet.asScala.filterNot(paneIds).foreach(paneId => Option(wanted.remove(paneId)).foreach(_.cancel()))
      lastExact.keySet.removeIf(paneId => !paneIds(paneId)): Unit
      widths.keySet.removeIf(paneId => !paneIds(paneId)): Unit

    private[manager] def shownWhile(paneId: PaneId, key: ScrollKey): Option[PaneScroll] =
      Option(lastExact.get(paneId))
        .filter(exact => exact.key.widthPx != key.widthPx && exact.key.version == key.version)
        .map(_.scroll)

    private[manager] def counted(paneId: PaneId, scroll: PaneScroll, key: ScrollKey): Unit =
      val _ = widths.put(paneId, Integer.valueOf(key.widthPx))
      val _ = lastExact.put(paneId, Exact(scroll, key))
      Option(wanted.remove(paneId)).foreach(_.cancel())

    private[manager] def count(paneId: PaneId, rows: VisualRowCounts, key: ScrollKey): Unit =
      if !Option(wanted.get(paneId)).exists(_.rows eq rows) then
        val resizing = Option(widths.put(paneId, Integer.valueOf(key.widthPx))).exists(_.intValue != key.widthPx)
        val next     = Wanted(rows, new AtomicBoolean(false), new AtomicReference(() => ()))
        Option(wanted.put(paneId, next)).foreach(_.cancel())
        def start(): Unit =
          if !next.replaced.get then
            executor.execute { () =>
              rows.measureRest(next.replaced.get)
              if !next.replaced.get && wanted.remove(paneId, next) then onCounted(paneId)
            }
        if resizing then next.timer.set(quiet.after(ResizeQuietPeriod)(() => start())) else start()
