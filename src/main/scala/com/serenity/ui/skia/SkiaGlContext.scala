package com.serenity.ui.skia

import scala.util.Try

import com.serenity.ui.sdl.{Sdl, SdlError}
import org.jetbrains.skia.{DirectContext, DirectContext_jvmKt, GLAssembledInterface}

/** Skia's GPU context over the GL context SDL made for a window (#2380). Skia asks the window, through [[SkiaGlProcs]],
  * for every GL function it uses, so it needs neither GLX nor a shim.
  *
  * Open and use it on the thread the window's context is current on, and close it there before the window goes: Skia
  * keeps GL objects and the lookup's stub until then.
  */
final class SkiaGlContext private (
    val direct: DirectContext,
    interface: GLAssembledInterface,
    procs: SkiaGlProcs
) extends AutoCloseable:

  def close(): Unit =
    direct.close()
    interface.close()
    procs.close()

object SkiaGlContext:

  private val Opening = "SkiaGlContext.open"

  /** A context over the GL context current on this thread for `sdl`, or why Skia would not make one, which is the
    * caller's cue for [[com.serenity.ui.sdl.PresentPath.RasterFallback]].
    */
  def open(sdl: Sdl): Either[SdlError, SkiaGlContext] =
    open(name => sdl.glProcAddress(name).fold(0L)(_.address()))

  private[skia] def open(lookup: String => Long): Either[SdlError, SkiaGlContext] =
    val procs = SkiaGlProcs(lookup)
    val opened = procs.assembled().flatMap { interface =>
      val direct = Try(DirectContext_jvmKt.makeGLWithInterface(DirectContext.Companion, interface)).toEither.left
        .map(failure => Option(failure.getMessage).getOrElse(failure.getClass.getName))
      direct.flatMap(context => Option(context).toRight("Skia gave no GL context")) match
        case Right(context) => Right(new SkiaGlContext(context, interface, procs))
        case Left(reason) =>
          interface.close()
          Left(reason)
    }
    opened.left.map { reason =>
      procs.close()
      SdlError.Call(Opening, reason)
    }
