package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicReference

/** The latest recorded picture, handed from the thread that records frames to the one that draws them. A picture that a
  * newer one replaced is closed only after the next draw has finished, never while a draw might still be reading it.
  */
final private[skia] class PictureSlot[A](close: A => Unit):

  final private case class State(current: Option[A], replaced: List[A], closed: Int)

  private val state = new AtomicReference(State(None, Nil, 0))

  def publish(next: A): Unit =
    val _ = state.updateAndGet(held => State(Some(next), held.current.toList ++ held.replaced, held.closed))

  /** Draws the latest picture, if there is one, and then closes the ones it replaced. */
  def drawLatest(draw: A => Unit): Boolean =
    state.get().current.fold(false) { picture =>
      draw(picture)
      val retired = state.getAndUpdate(held => held.copy(replaced = Nil, closed = held.closed + held.replaced.length))
      retired.replaced.foreach(close)
      true
    }

  def closeAll(): Unit =
    val held = state.getAndUpdate(held => State(None, Nil, held.closed + held.current.size + held.replaced.length))
    (held.current.toList ++ held.replaced).foreach(close)

  def closedCount: Int = state.get().closed
