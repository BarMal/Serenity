package com.serenity.diagnostics

import scala.jdk.CollectionConverters.*

/** `IOApp.main` starts the program first and registers its shutdown hook afterwards, then parks the main thread waiting
  * for the program. A signal that arrives before the registration ends the JVM at once, with no cancel and no wait, so
  * a child that announces itself from inside the program must not do so until the main thread is parked.
  */
object IoAppReadiness:

  def hookInstalled(mainFrames: List[StackTraceElement]): Boolean =
    mainFrames.exists(frame =>
      frame.getClassName == "java.util.concurrent.ArrayBlockingQueue" && frame.getMethodName == "take"
    ) &&
      mainFrames.exists(_.getClassName.startsWith("cats.effect.IOApp"))

  def currentMainFrames(): List[StackTraceElement] =
    Thread.getAllStackTraces.asScala
      .collectFirst { case (thread, frames) if thread.getName == "main" => frames.toList }
      .getOrElse(Nil)
