package com.serenity.testkit

import java.awt.{EventQueue, GraphicsEnvironment, Toolkit, Window}
import javax.swing.SwingUtilities

/** Specs that show a real window share one display and the JVM-wide `RepaintManager`, so two running at once see each
  * other's windows in a screen capture and each other's buffer requests. They take this lock for as long as their
  * window is up. When the body is done, and before the lock is let go, the event thread is brought up to date and the
  * toolkit's requests are flushed, so a window the body asked to close late is gone before the next spec's window is up
  * and its screen capture starts.
  */
object DisplayExclusive:
  private val lock = new Object

  def apply[A](body: => A): A = lock.synchronized(settled(body))

  /** The body's result with every window still showing once it is done, listed while the lock is still held, so none
    * can be another spec's.
    */
  def withShowingWindowsAfter[A](body: => A): (A, List[Window]) =
    lock.synchronized {
      val result = settled(body)
      (result, Window.getWindows.filter(_.isShowing).toList)
    }

  /** The body's result, once whatever it queued on the event thread has run. */
  private def settled[A](body: => A): A =
    try body
    finally
      if !GraphicsEnvironment.isHeadless && !EventQueue.isDispatchThread then
        SwingUtilities.invokeAndWait(() => ())
        Toolkit.getDefaultToolkit.sync()
