package com.serenity.ui.terminal

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import scala.concurrent.duration.*

import cats.effect.IO
import org.typelevel.log4cats.Logger

/** What a window knows of its own canvas paints, as one line for the log. */
final case class PaintDiagnostics(
    paints: Int,
    firstPaintMillis: Option[Long],
    showing: Boolean,
    canvasWidth: Int,
    canvasHeight: Int
):

  def line(label: String): String =
    s"[FIRST-PAINT] $label: paints=$paints firstPaintMs=${firstPaintMillis.fold("none")(_.toString)} " +
      s"showing=$showing canvas=${canvasWidth}x$canvasHeight"

/** Counts the canvas's paints and times the first, so a window that stays unpainted (#2404) says whether it was ever
  * painted. Recording is two atomic updates in `paintComponent`; nothing here changes what is painted.
  */
final class PaintProbe(nanoTime: () => Long = () => System.nanoTime()):
  private val created    = nanoTime()
  private val paints     = new AtomicInteger(0)
  private val firstPaint = new AtomicLong(PaintProbe.NotPainted)

  def painted(): Unit =
    val _ = firstPaint.compareAndSet(PaintProbe.NotPainted, nanoTime())
    val _ = paints.incrementAndGet()

  def diagnostics(showing: Boolean, width: Int, height: Int): PaintDiagnostics =
    val first = firstPaint.get()
    PaintDiagnostics(
      paints.get(),
      Option.when(first != PaintProbe.NotPainted)((first - created) / PaintProbe.NanosPerMilli),
      showing,
      width,
      height
    )

  private[terminal] val NotPainted    = Long.MinValue
  private[terminal] val NanosPerMilli = 1_000_000L

object PaintProbe:
  private[terminal] val NotPainted    = Long.MinValue
  private[terminal] val NanosPerMilli = 1_000_000L

object SwingPaintProbe:

  /** The window's paint record when its first frame has been rendered, and again a couple of seconds on. */
  def report(window: SwingWindow, logger: Logger[IO]): IO[Unit] =
    logger.info(window.paintDiagnostics.line("at the first frame")) >>
      (IO.sleep(2.seconds) >> logger.info(window.paintDiagnostics.line("2 s later"))).start.void
