package com.serenity.testkit

import java.awt.Window

/** Specs that show a real window share one display and the JVM-wide `RepaintManager`, so two running at once see each
  * other's windows in a screen capture and each other's buffer requests. They take this lock for as long as their
  * window is up.
  */
object DisplayExclusive:
  private val lock = new Object

  def apply[A](body: => A): A = lock.synchronized(body)

  /** The body's result with every window still showing once it is done, listed while the lock is still held, so none
    * can be another spec's.
    */
  def withShowingWindowsAfter[A](body: => A): (A, List[Window]) =
    lock.synchronized {
      val result = body
      (result, Window.getWindows.filter(_.isShowing).toList)
    }
