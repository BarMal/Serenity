package com.serenity.state.manager

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.ReentrantLock

import cats.effect.{IO, Resource}
import com.serenity.state.models.PaneId

/** Where a background count says that a pane's rows are all measured. The state manager is built before the render loop
  * that wants to hear this, so the loop listens afterwards; a pane counted before anyone listens is dropped, as the
  * loop draws a frame once it starts.
  *
  * A count can land after the loop has gone. `close` ends the listening, and shares a lock with `deliver` so that it
  * returns only once a delivery already handing off has finished: whatever the listener needs is still there for it.
  */
final class ScrollCountNotice:

  private val listener = new AtomicReference[PaneId => Unit](ScrollCountNotice.nobody)
  private val handOff  = new ReentrantLock

  def listen(onCounted: PaneId => Unit): Unit = listener.set(onCounted)

  def deliver(paneId: PaneId): Unit =
    handOff.lock()
    try listener.get()(paneId)
    finally handOff.unlock()

  def close(): Unit =
    handOff.lock()
    try listener.set(ScrollCountNotice.nobody)
    finally handOff.unlock()

object ScrollCountNotice:

  private val nobody: PaneId => Unit = _ => ()

  /** A notice that is closed when the resource is released, so acquire it after what its listener depends on. */
  val scoped: Resource[IO, ScrollCountNotice] =
    Resource.make(IO(new ScrollCountNotice))(notice => IO(notice.close()))
