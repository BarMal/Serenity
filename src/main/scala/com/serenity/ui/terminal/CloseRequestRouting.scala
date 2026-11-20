package com.serenity.ui.terminal

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.host.WindowControl

/** Where a request to close the window goes: to the runtime's callback, which asks the quit workflow and may be answered
  * Cancel, so another request can follow. Only with no callback (smoke test, startup probe) does it end the run through
  * the latch.
  */
trait CloseRequestRouting extends WindowControl:

  protected val closeLatch = new CountDownLatch(1)

  private val onCloseRequestedRef = new AtomicReference[Option[() => Unit]](None)

  override def setOnCloseRequested(cb: () => Unit): Unit = onCloseRequestedRef.set(Some(cb))

  protected def requestClose(): Unit = CloseRequestRouting.route(onCloseRequestedRef.get(), closeLatch)

object CloseRequestRouting:

  private[serenity] def route(onCloseRequested: Option[() => Unit], latch: CountDownLatch): Unit =
    onCloseRequested.fold(latch.countDown())(_.apply())
