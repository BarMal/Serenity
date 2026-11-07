package com.serenity.diagnostics

import scala.concurrent.duration.*

import cats.effect.unsafe.IORuntimeConfig
import cats.effect.{ExitCode, IO, IOApp, Resource}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** Run by [[ShutdownBoundSpec]] in a separate JVM: an `IOApp` shaped like `Main`, whose shutdown can be made to hang.
  * The first argument names what to do; `child.hookTimeoutSeconds` and `child.dumpAfterSeconds` shorten the bounds so
  * the spec does not wait the real 8 and 10 seconds.
  */
object ShutdownBoundChild extends IOApp:

  private def seconds(property: String): Option[FiniteDuration] =
    Option(System.getProperty(property)).map(_.toInt.seconds)

  override def runtimeConfig: IORuntimeConfig =
    seconds("child.hookTimeoutSeconds").fold(super.runtimeConfig)(
      ShutdownWatchdog.withHookTimeout(super.runtimeConfig, _)
    )

  private given Logger[IO]             = NoOpLogger[IO]
  private given ledger: ShutdownLedger = ShutdownLedger.recording()

  private val watchdog = new ShutdownWatchdog(ledger)

  private val report: String => Unit = text => System.out.println(text)

  private val stuckRelease: Resource[IO, Unit] =
    ShutdownTrace.traced("stuck window")(Resource.make(IO.unit)(_ => IO.never))

  private val ready: IO[Unit] = IO(System.out.println("READY"))

  private val hookInstalled: IO[Unit] =
    IO(IoAppReadiness.hookInstalled(IoAppReadiness.currentMainFrames())).flatMap(installed =>
      if installed then IO.unit else IO.sleep(5.millis) >> hookInstalled
    )

  def run(args: List[String]): IO[ExitCode] =
    args.headOption match
      case Some("signal") =>
        IO(watchdog.installSignalDump(seconds("child.dumpAfterSeconds").getOrElse(8.seconds), report)) >>
          stuckRelease.use(_ => hookInstalled >> ready >> IO.never)
      case Some("quit-stuck") =>
        watchdog.armHalt(2.seconds, ShutdownWatchdog.HaltExitCode, report) >>
          stuckRelease.use(_ => ready).as(ExitCode.Success)
      case Some("quit-clean") =>
        watchdog.armHalt(2.seconds, ShutdownWatchdog.HaltExitCode, report) >>
          Resource.unit[IO].use(_ => ready).as(ExitCode.Success)
      case Some("disarmed") =>
        watchdog.armHalt(1.second, ShutdownWatchdog.HaltExitCode, report) >> watchdog.disarm >>
          ready >> IO.sleep(3.seconds).as(ExitCode.Success)
      case other => IO.raiseError(new IllegalArgumentException(s"unknown mode $other"))
