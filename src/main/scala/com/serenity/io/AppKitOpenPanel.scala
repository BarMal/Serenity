package com.serenity.io

import java.nio.file.Path
import java.util.Locale

import scala.concurrent.duration.*
import scala.util.control.NonFatal

import cats.effect.IO

/** macOS's single Open..., which takes a file or a folder: an `NSOpenPanel` with both choices on, run on the AppKit
  * main thread.
  *
  * The calling thread is a blocking-pool thread, never the event dispatch thread. The EDT stays free to serve what AWT
  * sends it while the panel is up, and nothing the main thread does during the panel waits on the EDT: the panel has no
  * delegate, so AppKit makes no calls into Java. The panel is application-modal in AppKit itself, so the window behind
  * it takes no input without the EDT having to block. The JDK's own file dialog follows the same shape: `CFileDialog`
  * runs `nativeRunFileDialog` on a thread of its own, which waits for `safeSaveOrLoad` on the main thread.
  */
private[io] object AppKitOpenPanel:

  private val StartTimeout = 10.seconds

  trait OnMainThread:
    def apply[A](body: => A): Either[Throwable, A]

  final class OpenPanelFailure(message: String) extends RuntimeException(message)

  def isAvailable(osName: String, probe: () => Boolean = nativeProbe): Boolean =
    osName.toLowerCase(Locale.ROOT).contains("mac") && attempt(probe())

  def choose(initialDirectory: Option[Path]): IO[Option[Path]] =
    chooseWith(onAppKitThread, OpenPanelScript.run(JnaObjectiveC, _))(initialDirectory)

  def chooseWith(onMainThread: OnMainThread, script: Option[Path] => Either[String, Option[Path]])(
    initialDirectory: Option[Path]
  ): IO[Option[Path]] =
    IO.blocking(onMainThread(script(initialDirectory))).flatMap {
      case Right(Right(chosen)) => IO.pure(chosen)
      case Right(Left(message)) => IO.raiseError(new OpenPanelFailure(message))
      case Left(failure)        => IO.raiseError(failure)
    }

  private val onAppKitThread: OnMainThread = new OnMainThread:
    def apply[A](body: => A): Either[Throwable, A] =
      MainThreadHandoff.run(CoreFoundationMainLoop.schedule, StartTimeout)(body)

  // Cheap and side-effect free: the runtime must load, know NSOpenPanel, and CoreFoundation must be reachable.
  private def nativeProbe(): Boolean =
    JnaObjectiveC.classNamed("NSOpenPanel") != 0L && CoreFoundationMainLoop.isLoadable

  private def attempt(probe: => Boolean): Boolean =
    try probe
    catch
      case NonFatal(_)     => false
      case _: LinkageError => false
