package com.serenity.ui.skia

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

import scala.concurrent.{Future, Promise}
import scala.util.Try

import cats.effect.{IO, Resource}
import com.serenity.ui.color.RenderColor
import com.serenity.ui.renderer.RenderSurface
import com.serenity.ui.sdl.{Sdl, SdlError, SdlGlWindow}
import org.jetbrains.skia.{BlendMode, Paint, Rect}

/** A caret to fill over a presented frame, in logical pixels. */
final case class GlCaret(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: RenderColor)

/** Draws frames onto a GL window and puts them on screen (#2380). Frames are drawn by the renderer straight onto a
  * persistent offscreen surface ([[SkiaGlFrames]]); presenting draws that whole surface onto the window's framebuffer,
  * since a swap leaves the back buffer undefined, fills the carets over it, and swaps. The carets are never part of the
  * persistent frame, so a blink presents again without drawing a frame.
  *
  * Everything here runs on the window's SDL thread, in the order the calls were made from whatever thread makes them.
  * Drawing is never skipped or merged: the persistent surface depends on each frame's damage. Only presenting is
  * merged: a run of requests that has not started yet is one present, of the state after the last draw before it.
  */
final class SkiaGlPresenter private (
    window: SdlGlWindow,
    context: SkiaGlContext,
    targets: SkiaGlTargets,
    frames: SkiaGlFrames,
    beforeSwap: Sdl => Unit
):

  private val carets         = new AtomicReference(List.empty[GlCaret])
  private val listener       = new AtomicReference(Option.empty[() => Unit])
  private val drawn          = new AtomicReference(Option.empty[(GlFrameSettings, SkiaGlTargets.Made)])
  private val queuedPresents = new AtomicInteger(0)
  private val presented      = new AtomicInteger(0)
  private val closed         = new AtomicBoolean(false)

  /** Queues `render` to draw a frame onto the persistent surface. Completes when it has run, with why it could not. */
  def draw(settings: GlFrameSettings)(render: RenderSurface => Unit): Future[Either[String, Unit]] =
    if closed.get then Future.successful(Left(SkiaGlPresenter.Closed))
    else
      onSdlThread { _ =>
        if closed.get then Left(SkiaGlPresenter.Closed)
        else
          val result = frames.draw(settings)(render)
          result.foreach { _ =>
            val widthPx  = SkiaRenderSurface.deviceDimension(settings.logicalWidthPx, settings.deviceScale)
            val heightPx = SkiaRenderSurface.deviceDimension(settings.logicalHeightPx, settings.deviceScale)
            targets.ensure(widthPx, heightPx, settings.deviceScale).foreach(made => drawn.set(Some((settings, made))))
          }
          result
      }

  /** Asks for the latest drawn frame to be put on the window; nothing is drawn before the first frame. */
  def requestPresent(): Unit =
    if !closed.get then
      val _ = queuedPresents.incrementAndGet()
      window.post(sdl => if queuedPresents.decrementAndGet() == 0 && !closed.get then present(sdl))

  /** Sets the carets to fill over the frame and presents it again. */
  def showCarets(next: List[GlCaret]): Unit =
    carets.set(next)
    requestPresent()

  /** `callback` runs on the SDL thread after every swap. */
  def onRendered(callback: () => Unit): Unit = listener.set(Some(callback))

  def presentedFrames: Int = presented.get

  /** Completes once everything queued before this call has run. */
  def drained(): Future[Unit] = onSdlThread(_ => ())

  /** Runs `use` on the SDL thread with the surfaces of the last frame drawn, for a spec to look at. */
  private[skia] def withTargets[A](use: Option[SkiaGlTargets.Made] => A): Future[A] =
    onSdlThread(_ => use(drawn.get.map(_._2)))

  private def onSdlThread[A](task: Sdl => A): Future[A] =
    val result = Promise[A]()
    window.post(sdl => result.complete(Try(task(sdl))))
    result.future

  private def present(sdl: Sdl): Unit =
    drawn.get.foreach { (settings, made) =>
      val canvas = made.window.getCanvas
      val copy   = new Paint()
      copy.setBlendMode(BlendMode.SRC)
      made.offscreen.draw(canvas, 0, 0, copy)
      copy.close()
      val ink   = new Paint()
      val scale = settings.deviceScale.toFloat
      carets.get().foreach { caret =>
        ink.setColor(caret.color.argb)
        canvas.drawRect(
          Rect.Companion.makeXYWH(
            caret.xPx * scale,
            caret.yPx * scale,
            caret.widthPx * scale,
            caret.heightPx * scale
          ),
          ink
        )
      }
      ink.close()
      context.direct.flushAndSubmit(made.window, false)
      beforeSwap(sdl)
      val _ = sdl.glSwapWindow(window.handle)
      val _ = presented.incrementAndGet()
      listener.get.foreach(_())
    }

  /** Closes the surfaces, then the context; later frames and presents are refused. Harmless a second time. */
  private def shutDown(): Unit =
    if closed.compareAndSet(false, true) then
      targets.close()
      context.close()

object SkiaGlPresenter:

  private val Closed = "the presenter is closed"

  /** A presenter for `window`, or why Skia would not make a GL context for it (the cue for the raster fallback).
    * Releasing it closes its surfaces and context on the SDL thread. `beforeSwap` runs on the SDL thread just before
    * each swap, which is the last moment the framebuffer holds the frame; it is for a spec to read it.
    */
  def open(window: SdlGlWindow, beforeSwap: Sdl => Unit = _ => ()): Resource[IO, Either[SdlError, SkiaGlPresenter]] =
    Resource.make(
      window.onSdlThread { sdl =>
        SkiaGlContext.open(sdl).map { context =>
          val targets = SkiaGlTargets(context)
          new SkiaGlPresenter(window, context, targets, SkiaGlFrames(context, targets), beforeSwap)
        }
      }
    )(_.fold(_ => IO.unit, presenter => window.onSdlThread(_ => presenter.shutDown())))
