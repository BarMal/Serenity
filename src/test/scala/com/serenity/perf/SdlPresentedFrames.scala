package com.serenity.perf

import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.{ConcurrentLinkedQueue, Semaphore, TimeUnit}

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import cats.effect.{IO, Resource}
import com.serenity.diagnostics.FrameTimings
import com.serenity.ui.renderer.RenderSurface
import com.serenity.ui.sdl.{Sdl, SdlError, SdlGlWindow}
import com.serenity.ui.skia.{GlFrameSettings, SkiaGlPresenter}

/** The clock readings the presenter's own hooks give: `beforeSwap` runs on the SDL thread just before the swap and
  * `onRendered` just after it. `finish` is `glFinish` or nothing; run after the draw and before the swap, it moves the
  * GPU's work into those rows instead of leaving it to surface in the swap. With `timings`, the app's own key-latency
  * trace is told when the frame was handed over, when its present started and when the swap returned.
  */
final private[perf] class SdlPresentWatch(
    clock: () => Long,
    finish: () => Unit,
    timings: Option[FrameTimings] = None
):
  private val paintStartedAt = new AtomicLong(0L)
  private val drawn          = new AtomicLong(0L)
  private val presentStart   = new AtomicLong(0L)
  private val presentEnd     = new AtomicLong(0L)
  private val swapEnd        = new AtomicLong(0L)
  private val rendered       = new Semaphore(0)

  def markDrawn(): Unit =
    finish()
    drawn.set(clock())

  def markPublished(): Unit = timings.foreach(_.framePublished())

  def markPresentStart(): Unit =
    presentStart.set(clock())
    timings.foreach(frameTimings => paintStartedAt.set(frameTimings.paintStarted()))

  def beforeSwap(sdl: Sdl): Unit =
    finish()
    presentEnd.set(clock())

  def afterSwap(): Unit =
    swapEnd.set(clock())
    timings.foreach(_.paintFinished(paintStartedAt.get))
    rendered.release()

  def awaitRendered(timeout: FiniteDuration): Boolean = rendered.tryAcquire(timeout.toMillis, TimeUnit.MILLISECONDS)

  def stamps(requested: Long): FrameStamps =
    FrameStamps(requested, drawn.get, presentStart.get, presentEnd.get, swapEnd.get)

/** A presenter on a real GL window, driven one frame at a time and stamped at each step (#2380). Every step is queued
  * on the SDL thread behind the one before it, so the stamps are taken where the work happens; the benchmark's own
  * thread only waits for the swap to return before it starts the next frame.
  */
final private[perf] class SdlPresentedFrames private (
    window: SdlGlWindow,
    presenter: SkiaGlPresenter,
    watch: SdlPresentWatch,
    settings: GlFrameSettings,
    clock: () => Long
):
  private val taken = new ConcurrentLinkedQueue[FrameStamps]()

  def presentedFrames: Int = presenter.presentedFrames

  /** Draws one frame with `render`, presents it and waits for the swap to return. */
  def frame(render: RenderSurface => Unit): Unit =
    val requested = clock()
    val drawing   = presenter.draw(settings)(render)
    window.post(_ => watch.markDrawn())
    Await.result(drawing, SdlPresentedFrames.FrameTimeout) match
      case Left(reason) => throw new IllegalStateException(reason)
      case Right(())    => ()
    Await.result(presenter.drained(), SdlPresentedFrames.FrameTimeout)
    window.post(_ => watch.markPresentStart())
    watch.markPublished()
    presenter.requestPresent()
    if !watch.awaitRendered(SdlPresentedFrames.FrameTimeout) then
      throw new IllegalStateException("the frame was never swapped")
    val _ = taken.add(watch.stamps(requested))

  /** The stamps of the frames presented since the last call, which starts the next run. */
  def takeStamps(): Vector[FrameStamps] =
    val kept = taken.asScala.toVector
    taken.clear()
    kept

  /** The stamps of `frames` runs of `step`, after `warmup` runs that are not kept. */
  def measure(warmup: Int, frames: Int)(step: () => Unit): Vector[FrameStamps] =
    Vector.fill(warmup)(()).foreach(_ => step())
    taken.clear()
    Vector.fill(frames)(()).foreach(_ => step())
    taken.asScala.toVector

private[perf] object SdlPresentedFrames:

  private val FrameTimeout = 10.seconds

  /** `finish` runs on the SDL thread after each draw and before each swap. */
  def open(
    window: SdlGlWindow,
    settings: GlFrameSettings,
    finish: Option[() => Unit],
    frameTimings: Option[FrameTimings] = None
  ): Resource[IO, Either[SdlError, SdlPresentedFrames]] =
    val watch = SdlPresentWatch(System.nanoTime, finish.getOrElse(() => ()), frameTimings)
    SkiaGlPresenter
      .open(window, watch.beforeSwap)
      .map(_.map { presenter =>
        presenter.onRendered(() => watch.afterSwap())
        new SdlPresentedFrames(window, presenter, watch, settings, System.nanoTime)
      })

  def setSwapInterval(window: SdlGlWindow, interval: Int): IO[Either[SdlError, Unit]] =
    window.onSdlThread(_.glSetSwapInterval(interval))
