package com.serenity.state.manager

import java.util.concurrent.atomic.AtomicReference

import com.serenity.state.models.PaneId

/** Where a background count says that a pane's rows are all measured. The state manager is built before the render loop
  * that wants to hear this, so the loop listens afterwards; a pane counted before anyone listens is dropped, as the
  * loop draws a frame once it starts.
  */
final class ScrollCountNotice:

  private val listener = new AtomicReference[PaneId => Unit](_ => ())

  def listen(onCounted: PaneId => Unit): Unit = listener.set(onCounted)

  def deliver(paneId: PaneId): Unit = listener.get()(paneId)
