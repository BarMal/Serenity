package com.serenity.testkit

/** Specs that show a real window share one display and the JVM-wide `RepaintManager`, so two running at once see each
  * other's windows in a screen capture and each other's buffer requests. They take this lock for as long as their
  * window is up.
  */
object DisplayExclusive:
  private val lock = new Object

  def apply[A](body: => A): A = lock.synchronized(body)
