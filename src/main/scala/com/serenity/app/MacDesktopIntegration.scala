package com.serenity.app

import java.awt.Desktop
import java.nio.file.Path
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import scala.jdk.CollectionConverters.*

import cats.effect.{IO, Resource}

/** The one place Serenity calls `java.awt.Desktop.set*Handler`, so each handler has a single owner.
  *
  * The JDK keeps one handler per kind for the life of the JVM, and a restart reuses the JVM, so the handlers are
  * registered once and route to whichever launch currently holds the target. Where the platform lacks a handler (all of
  * it, away from macOS) nothing is registered and the target simply never hears from the desktop.
  */
final class MacDesktopIntegration extends DesktopEvents:
  private val registered = AtomicBoolean(false)
  private val current    = AtomicReference[Option[DesktopTarget]](None)

  def attach(target: DesktopTarget): Resource[IO, Unit] =
    Resource.make(IO.blocking { registerHandlers(); current.set(Some(target)) })(_ => IO(current.set(None)))

  private def registerHandlers(): Unit =
    if registered.compareAndSet(false, true) && Desktop.isDesktopSupported then
      val desktop = Desktop.getDesktop
      if desktop.isSupported(Desktop.Action.APP_OPEN_FILE) then
        desktop.setOpenFileHandler(event => routeOpen(event.getFiles.asScala.map(_.toPath).toList))
      if desktop.isSupported(Desktop.Action.APP_QUIT_HANDLER) then
        desktop.setQuitHandler((_, response) => routeQuit(response))

  private def routeOpen(paths: List[Path]): Unit =
    current.get.foreach(_.onOpen(paths))

  /** With no launch to run an orderly quit, the desktop's own quit is the right answer. */
  private def routeQuit(response: java.awt.desktop.QuitResponse): Unit =
    current.get match
      case Some(target) =>
        target.onQuit(QuitResponse(IO.blocking(response.performQuit()), IO.blocking(response.cancelQuit())))
      case None => response.performQuit()
