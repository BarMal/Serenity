package com.serenity.ui.sdl

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

import cats.effect.{Deferred, IO, Ref, Resource}
import cats.syntax.all.*

/** A window whose frames are drawn with GL: SDL makes the window and an EGL context for it, and every SDL and GL call
  * runs on one dedicated thread, the one the context is current on (#2380). Nothing is drawn with a renderer yet; the
  * window can clear to a colour and swap, which is what shows the context works, and it follows its own size, scale and
  * close request from SDL's events.
  *
  * Where SDL cannot make a context (a driver without GL, no EGL on the machine) [[SdlGlWindow.open]] gives SDL's reason
  * as a `Left` and leaves nothing behind, so the caller can choose [[PresentPath.RasterFallback]].
  */
final class SdlGlWindow private (
    sdl: Sdl,
    val handle: SdlWindow,
    calls: GlCalls,
    executor: ExecutionContext,
    state: Ref[IO, GlWindowState],
    closed: Deferred[IO, Unit],
    val vsync: Boolean
):

  /** Runs `use` on the SDL thread, the only thread that may call SDL or GL for this window. */
  def onSdlThread[A](use: Sdl => A): IO[A] = IO(use(sdl)).evalOn(executor)

  /** Completes once the window has been asked to close. */
  def awaitClose: IO[Unit] = closed.get

  def pixelSize: IO[(Int, Int)] = state.get.map(current => (current.pixelWidth, current.pixelHeight))

  /** The scale the display asked for (1.5 stays 1.5). */
  def deviceScale: IO[Double] = state.get.map(_.scale)

  /** The pixel size to lay out for after a resize or a scale change, once, until the next one. */
  def takeResize: IO[Option[(Int, Int)]] = state.modify(_.takeResize)

  /** What GL says it is drawing with: a GPU, or Mesa's llvmpipe. */
  def renderer: IO[String] = onSdlThread(_ => calls.renderer)

  def clear(red: Float, green: Float, blue: Float): IO[Unit] =
    onSdlThread(_ => calls.clear(red, green, blue, 1.0f))

  def swap: IO[Either[SdlError, Unit]] = onSdlThread(_.glSwapWindow(handle))

  def clearAndSwap(red: Float, green: Float, blue: Float): IO[Either[SdlError, Unit]] =
    clear(red, green, blue) *> swap

  /** Folds one of SDL's events into the window's state; `scale` is the display scale read when the event arrived. */
  private[sdl] def applyEvent(event: SdlEvent, scale: Double): IO[Unit] =
    state
      .updateAndGet(_.after(event, scale))
      .flatMap(current => closed.complete(()).void.whenA(current.closeRequested))

object SdlGlWindow:

  final case class Settings(title: String, width: Int, height: Int, videoDriver: Option[String])

  final private case class Native(
      window: SdlWindow,
      context: SdlGlContext,
      calls: GlCalls,
      vsync: Boolean,
      start: GlWindowState
  )

  private val PumpInterval = 4.millis

  /** Opens the window, or says why SDL could not give it a GL context. Releasing destroys the context and the window
    * and shuts SDL down.
    */
  def open(sdl: Sdl, settings: Settings): Resource[IO, Either[SdlError, SdlGlWindow]] =
    for
      executor <- sdlThread
      opened <- Resource.make(openNativeOn(sdl, settings, executor))(
        _.fold(_ => IO.unit, native => IO(close(sdl, native)).evalOn(executor))
      )
      assembled <- Resource.eval(opened.traverse(assemble(sdl, executor, _)))
      _         <- assembled.fold(_ => Resource.unit[IO], pump(sdl, executor, _))
    yield assembled

  private def sdlThread: Resource[IO, ExecutionContext] =
    Resource
      .make(IO(Executors.newSingleThreadExecutor { runnable =>
        // Daemon so a window that was never released cannot keep the JVM alive.
        val thread = new Thread(runnable, "serenity-sdl")
        thread.setDaemon(true)
        thread
      }))(executor => IO(executor.shutdown()))
      .map(ExecutionContext.fromExecutorService)

  private def openNativeOn(sdl: Sdl, settings: Settings, executor: ExecutionContext): IO[Either[SdlError, Native]] =
    IO(openNative(sdl, settings)).evalOn(executor)

  private def requestContext(sdl: Sdl): Either[SdlError, Unit] =
    List(
      SdlLayouts.GlAttribute.RedSize             -> 8,
      SdlLayouts.GlAttribute.GreenSize           -> 8,
      SdlLayouts.GlAttribute.BlueSize            -> 8,
      SdlLayouts.GlAttribute.AlphaSize           -> 8,
      SdlLayouts.GlAttribute.StencilSize         -> 8,
      SdlLayouts.GlAttribute.DoubleBuffer        -> 1,
      SdlLayouts.GlAttribute.ContextProfileMask  -> SdlLayouts.GlProfileEs,
      SdlLayouts.GlAttribute.ContextMajorVersion -> 3,
      SdlLayouts.GlAttribute.ContextMinorVersion -> 0
    ).traverse_((attribute, value) => sdl.glSetAttribute(attribute, value))

  /** Everything up to a current context, on the SDL thread; undoes what it did when a step fails. */
  private def openNative(sdl: Sdl, settings: Settings): Either[SdlError, Native] =
    sdl.init(settings.videoDriver).flatMap { _ =>
      val flags = SdlLayouts.WindowOpenGl | SdlLayouts.WindowResizable | SdlLayouts.WindowHighPixelDensity
      val created = for
        _      <- requestContext(sdl)
        window <- sdl.createWindow(settings.title, settings.width, settings.height, flags)
        context <- sdl.glCreateContext(window).leftMap { failure =>
          sdl.destroyWindow(window)
          failure
        }
        ready <- makeCurrent(sdl, window, context).leftMap { failure =>
          sdl.glDestroyContext(context)
          sdl.destroyWindow(window)
          failure
        }
      yield ready
      created.left.foreach(_ => sdl.quit())
      created
    }

  private def makeCurrent(sdl: Sdl, window: SdlWindow, context: SdlGlContext): Either[SdlError, Native] =
    for
      _      <- sdl.glMakeCurrent(window, context)
      calls  <- GlCalls.load(sdl)
      pixels <- sdl.windowSizeInPixels(window)
      vsync = sdl.glSetSwapInterval(1).isRight
    yield Native(window, context, calls, vsync, GlWindowState(pixels._1, pixels._2, sdl.displayScale(window).toDouble))

  private def close(sdl: Sdl, native: Native): Unit =
    sdl.glDestroyContext(native.context)
    sdl.destroyWindow(native.window)
    sdl.quit()

  private def assemble(sdl: Sdl, executor: ExecutionContext, native: Native): IO[SdlGlWindow] =
    (Ref.of[IO, GlWindowState](native.start), Deferred[IO, Unit]).mapN { (state, closed) =>
      new SdlGlWindow(sdl, native.window, native.calls, executor, state, closed, native.vsync)
    }

  /** Feeds SDL's events to the window's state until the resource is released. */
  private def pump(sdl: Sdl, executor: ExecutionContext, window: SdlGlWindow): Resource[IO, Unit] =
    pumpOnce(sdl, executor, window).productR(IO.sleep(PumpInterval)).foreverM.background.void

  private def pumpOnce(sdl: Sdl, executor: ExecutionContext, window: SdlGlWindow): IO[Unit] =
    IO(drain(sdl, window.handle)).evalOn(executor).flatMap { (events, scale) =>
      events.traverse_(event => window.applyEvent(event, scale))
    }

  private def drain(sdl: Sdl, handle: SdlWindow): (List[SdlEvent], Double) =
    val events = Iterator.continually(sdl.pollEvent()).map(_.toOption.flatten).takeWhile(_.isDefined).flatten.toList
    val rescaled = events.exists {
      case SdlEvent.Window(SdlEvent.WindowKind.DisplayScaleChanged, _, _) => true
      case _                                                              => false
    }
    (events, if rescaled then sdl.displayScale(handle).toDouble else 0.0)
