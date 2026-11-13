package com.serenity.app

import com.serenity.app.RenderBackendSelection.{Decision, Probe}

/** The words for the fallback a user did not ask for: Skia was wanted, its native library would not load, and the
  * window became the terminal.
  */
object SkiaFallback:

  private val MissingLibrary = """([\w.+-]+\.so(?:\.[\d.]+)?): cannot open shared object file""".r.unanchored

  /** The one line the terminal's header says: what could not load, and that Serenity is in the terminal instead. */
  def header(decision: Decision): Option[String] =
    decision.skiaFailure.map { cause =>
      val what = missingLibrary(cause).fold(oneLine(cause))(library => s"the system library $library is missing")
      s"Skia could not load: $what. Running in the terminal."
    }

  private def oneLine(text: String): String =
    text.linesIterator.map(_.trim).filter(_.nonEmpty).mkString(" ")

  /** Skiko reports a library the dynamic loader cannot resolve as a wrapper that names only its own file; the library
    * that is missing is in the `UnsatisfiedLinkError` it wraps. The causes ride in `detail`, apart from `cause`, which
    * is what the log line has always shown.
    */
  def failure(error: Throwable): Probe.Failed =
    val causes = Iterator
      .iterate(Option(error.getCause))(_.flatMap(next => Option(next.getCause)))
      .takeWhile(_.isDefined)
      .flatten
      .take(MaxCauses)
      .toList
      .filterNot(_ eq error)
    Probe.Failed(describe(error), Option.when(causes.nonEmpty)(causes.map(describe).mkString("; caused by ")))

  private val MaxCauses = 5

  private def describe(error: Throwable): String =
    s"${error.getClass.getSimpleName}: ${Option(error.getMessage).getOrElse("no message")}"

  def missingLibrary(cause: String): Option[String] =
    MissingLibrary.findFirstMatchIn(cause).map(_.group(1))
