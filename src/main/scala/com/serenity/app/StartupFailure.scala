package com.serenity.app

import java.nio.file.Path
import java.time.Instant

import cats.effect.IO
import com.serenity.diagnostics.{CrashRecord, CrashReport, RuntimeIdentity}

/** A failure before the editor was running. A packaged GUI app has no terminal to print to, so the failure is written
  * to a crash file and shown in a dialog that says where the file is.
  */
object StartupFailure:

  val Title: String = "Serenity could not start"

  /** @param report
    *   Everything in the crash file, for the dialog's "Copy report".
    * @param crashFile
    *   Where the report was written; empty when the log directory was not writable.
    */
  final case class Notice(title: String, message: String, report: String, logDirectory: Path, crashFile: Option[Path])

  def message(error: Throwable, crashFile: Option[Path], logDirectory: Path): String =
    val reason = Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getName)
    val where = crashFile.fold(s"The crash report could not be saved. Logs are in $logDirectory.")(file =>
      s"A crash report was saved to $file. Logs are in $logDirectory."
    )
    s"Serenity could not start: $reason\n$where"

  /** Never fails: a launch that is already failing has nothing better to do with a second failure than print it. The
    * crash is acknowledged here, so the next launch does not report a closing the user has just been told about.
    */
  def report(
    error: Throwable,
    identity: RuntimeIdentity,
    store: CrashRecord,
    at: Instant,
    display: Notice => IO[Unit],
    console: String => IO[Unit]
  ): IO[Unit] =
    val crashReport = CrashReport.render(identity, at, "Serenity failed during startup.", Some(error), store.directory)
    for
      recorded <- store.recordCrash(crashReport)
      _        <- store.acknowledge
      kept = recorded.map(_ => store.acknowledgedCrashFile)
      text = message(error, kept, store.directory)
      _ <- console(text).handleError(_ => ())
      _ <- display(Notice(Title, text, crashReport, store.directory, kept)).handleError(_ => ())
    yield ()
