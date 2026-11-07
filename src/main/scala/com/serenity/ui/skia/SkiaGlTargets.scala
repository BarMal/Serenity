package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicReference

import scala.util.Try

import org.jetbrains.skia.{BackendRenderTarget, ImageInfo, Surface, SurfaceColorFormat, SurfaceOrigin}

/** The two surfaces a GL window is drawn on (#2380): an offscreen one that keeps the frame between draws, so the
  * renderer can draw only what changed, and the window's own framebuffer, which a swap leaves undefined and which is
  * therefore only ever drawn over in full.
  *
  * Both are made again when the pixel size or the scale changes, and the new offscreen surface is a different
  * `Surface`, which is how the renderer knows it holds no earlier frame. Use it, and close it, on the thread the GL
  * context is current on, before the context is closed.
  */
final class SkiaGlTargets private (context: SkiaGlContext) extends AutoCloseable:
  import SkiaGlTargets.*

  private val state = new AtomicReference(Option.empty[Made])

  /** The targets for a window of `widthPx` x `heightPx` device pixels at `scale`: the ones already made while those
    * still hold, otherwise new ones, with the old ones closed. Skia's reason when it cannot make a surface that size.
    */
  def ensure(widthPx: Int, heightPx: Int, scale: Double): Either[String, Made] =
    state.get().filter(_.fits(widthPx, heightPx, scale)).toRight(()).left.flatMap { _ =>
      state.getAndSet(None).foreach(_.close())
      build(context, widthPx, heightPx, scale).map { made =>
        state.set(Some(made))
        made
      }
    }

  def close(): Unit = state.getAndSet(None).foreach(_.close())

object SkiaGlTargets:

  private val Rgba8 = 0x8058

  def apply(context: SkiaGlContext): SkiaGlTargets = new SkiaGlTargets(context)

  /** What was made for one size and scale. */
  final class Made private[SkiaGlTargets] (
      val offscreen: Surface,
      val window: Surface,
      backend: BackendRenderTarget,
      widthPx: Int,
      heightPx: Int,
      scale: Double
  ) extends AutoCloseable:

    private[SkiaGlTargets] def fits(width: Int, height: Int, at: Double): Boolean =
      widthPx == width && heightPx == height && scale == at

    def close(): Unit =
      window.close()
      backend.close()
      offscreen.close()

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private def build(context: SkiaGlContext, widthPx: Int, heightPx: Int, scale: Double): Either[String, Made] =
    Try {
      val offscreen = Surface.Companion.makeRenderTarget(
        context.direct,
        true,
        ImageInfo.Companion.makeN32Premul(widthPx, heightPx),
        0,
        SurfaceOrigin.TOP_LEFT,
        null,
        false
      )
      val backend = BackendRenderTarget.Companion.makeGL(widthPx, heightPx, 0, 8, 0, Rgba8)
      val window = Surface.Companion.makeFromBackendRenderTarget(
        context.direct,
        backend,
        SurfaceOrigin.BOTTOM_LEFT,
        SurfaceColorFormat.RGBA_8888,
        null,
        null
      )
      (Option(offscreen), Option(window), backend)
    }.toEither.left
      .map(failure => Option(failure.getMessage).getOrElse(failure.getClass.getName))
      .flatMap {
        case (Some(offscreen), Some(window), backend) =>
          Right(new Made(offscreen, window, backend, widthPx, heightPx, scale))
        case (offscreen, window, backend) =>
          offscreen.foreach(_.close())
          window.foreach(_.close())
          backend.close()
          Left(s"Skia could not make a ${widthPx}x$heightPx GL surface")
      }
