package com.serenity.state.manager

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{ConcurrentHashMap, Executor}

import com.serenity.state.models.PaneId
import com.serenity.ui.layout.VisualRowCounts

/** How a pane's scrollbar gets the row count of the whole document (#2445). `Inline` measures every line inside the
  * frame that needs the bar, which is what the scene has always done. `Deferred` measures only the lines on screen
  * there, leaves the bar out of the scene, and measures the rest on an executor.
  */
sealed trait ScrollCounting:

  /** A scene was just built for `paneId` with its bar counted, so nothing is left for an earlier count to do. */
  private[manager] def counted(paneId: PaneId): Unit

  /** A scene was just built for `paneId` with its bar waiting on `rows`. */
  private[manager] def count(paneId: PaneId, rows: VisualRowCounts): Unit

  private[manager] def isDeferred: Boolean

object ScrollCounting:

  case object Inline extends ScrollCounting:
    private[manager] def counted(paneId: PaneId): Unit                      = ()
    private[manager] def count(paneId: PaneId, rows: VisualRowCounts): Unit = ()
    private[manager] def isDeferred: Boolean                                = false

  /** `onCounted` is told, from the executor's thread, which pane's lines are now all measured. A count that a newer
    * scene for the same pane replaced stops before its next line and tells nobody.
    */
  def deferred(executor: Executor, onCounted: PaneId => Unit): ScrollCounting = new Deferred(executor, onCounted)

  final private class Deferred(executor: Executor, onCounted: PaneId => Unit) extends ScrollCounting:

    private val running = new ConcurrentHashMap[PaneId, AtomicBoolean]()

    private[manager] def isDeferred: Boolean = true

    private[manager] def counted(paneId: PaneId): Unit =
      Option(running.remove(paneId)).foreach(_.set(true))

    private[manager] def count(paneId: PaneId, rows: VisualRowCounts): Unit =
      val replaced = new AtomicBoolean(false)
      Option(running.put(paneId, replaced)).foreach(_.set(true))
      executor.execute { () =>
        rows.measureRest(replaced.get)
        if !replaced.get && running.remove(paneId, replaced) then onCounted(paneId)
      }
