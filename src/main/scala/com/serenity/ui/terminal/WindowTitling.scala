package com.serenity.ui.terminal

import java.util.concurrent.atomic.AtomicReference
import javax.swing.SwingUtilities

import com.serenity.ui.host.WindowControl

/** Sets the window's title on the event-dispatch thread, and only when it is not the title last set: the runtime offers
  * it on every frame.
  */
trait WindowTitling extends WindowControl:

  private val lastSet = new AtomicReference[Option[String]](None)

  protected def applyTitle(title: String): Unit

  protected def onEventThread(run: Runnable): Unit = SwingUtilities.invokeLater(run)

  override def setTitle(title: String): Unit =
    if !lastSet.getAndSet(Some(title)).contains(title) then onEventThread(() => applyTitle(title))
