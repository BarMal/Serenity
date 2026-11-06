package com.serenity.ui.sdl

import java.lang.foreign.{Arena, MemorySegment, ValueLayout}
import java.lang.invoke.MethodHandle
import java.nio.file.Path

/** SDL3 through the Java foreign-function API, without a binding library (#2380). It covers what a window needs: video,
  * the window's software surface, events, text input, cursors and the clipboard.
  *
  * SDL wants every video call on one thread, the one that called [[init]]; nothing here locks. Calls report failure as
  * an [[SdlError]] carrying SDL's own error text.
  */
final class Sdl private (
    val source: SdlLibrary.Source,
    handles: Map[SdlFunction, MethodHandle],
    arena: Arena
):
  import SdlFunction.*

  private val eventBuffer: MemorySegment = arena.allocate(SdlLayouts.EventSize, 8)

  private def handle(function: SdlFunction): MethodHandle = handles(function)

  private def errorText: String =
    SdlEvent.cString(handle(GetError).invokeExact(): MemorySegment)

  private def failed(function: SdlFunction): SdlError = SdlError.Call(function.symbol, errorText)

  /** Runs `call` and turns `false` into SDL's error. */
  private def checked(function: SdlFunction)(call: => Boolean): Either[SdlError, Unit] =
    if call then Right(()) else Left(failed(function))

  /** Runs `use` on a scratch area that is freed when it returns. */
  private def scratch[A](use: Arena => A): A =
    val area = Arena.ofConfined()
    try use(area)
    finally area.close()

  def version: Int = handle(GetVersion).invokeExact(): Int

  /** Starts the video subsystem, on `videoDriver` when given (`wayland`, `dummy`, ...) and on SDL's choice otherwise.
    */
  def init(videoDriver: Option[String]): Either[SdlError, Unit] =
    scratch { area =>
      videoDriver.fold[Either[SdlError, Unit]](Right(())) { name =>
        checked(SetHint)(
          handle(SetHint).invokeExact(area.allocateFrom("SDL_VIDEO_DRIVER"), area.allocateFrom(name)): Boolean
        )
      }
    }.flatMap(_ => checked(Init)(handle(Init).invokeExact(SdlLayouts.InitVideo): Boolean))

  def quit(): Unit = handle(Quit).invokeExact(): Unit

  def currentVideoDriver: Option[String] =
    val name = handle(GetCurrentVideoDriver).invokeExact(): MemorySegment
    Option.when(name.address() != 0L)(SdlEvent.cString(name))

  // ---- windows ----

  def createWindow(title: String, width: Int, height: Int, flags: Long): Either[SdlError, SdlWindow] =
    scratch { area =>
      val window = handle(CreateWindow).invokeExact(area.allocateFrom(title), width, height, flags): MemorySegment
      Either.cond(window.address() != 0L, SdlWindow(window), failed(CreateWindow))
    }

  def destroyWindow(window: SdlWindow): Unit = handle(DestroyWindow).invokeExact(window.handle): Unit

  def showWindow(window: SdlWindow): Either[SdlError, Unit] =
    checked(ShowWindow)(handle(ShowWindow).invokeExact(window.handle): Boolean)

  def raiseWindow(window: SdlWindow): Either[SdlError, Unit] =
    checked(RaiseWindow)(handle(RaiseWindow).invokeExact(window.handle): Boolean)

  def maximizeWindow(window: SdlWindow): Either[SdlError, Unit] =
    checked(MaximizeWindow)(handle(MaximizeWindow).invokeExact(window.handle): Boolean)

  def minimizeWindow(window: SdlWindow): Either[SdlError, Unit] =
    checked(MinimizeWindow)(handle(MinimizeWindow).invokeExact(window.handle): Boolean)

  def restoreWindow(window: SdlWindow): Either[SdlError, Unit] =
    checked(RestoreWindow)(handle(RestoreWindow).invokeExact(window.handle): Boolean)

  def setWindowTitle(window: SdlWindow, title: String): Either[SdlError, Unit] =
    scratch(area =>
      checked(SetWindowTitle)(handle(SetWindowTitle).invokeExact(window.handle, area.allocateFrom(title)): Boolean)
    )

  def setWindowSize(window: SdlWindow, width: Int, height: Int): Either[SdlError, Unit] =
    checked(SetWindowSize)(handle(SetWindowSize).invokeExact(window.handle, width, height): Boolean)

  def setWindowMinimumSize(window: SdlWindow, width: Int, height: Int): Either[SdlError, Unit] =
    checked(SetWindowMinimumSize)(handle(SetWindowMinimumSize).invokeExact(window.handle, width, height): Boolean)

  /** The window's size in logical pixels. */
  def windowSize(window: SdlWindow): Either[SdlError, (Int, Int)] =
    sizeOf(GetWindowSize, window)

  /** The window's size in the pixels of its surface: logical size times the scale the compositor asked for. */
  def windowSizeInPixels(window: SdlWindow): Either[SdlError, (Int, Int)] =
    sizeOf(GetWindowSizeInPixels, window)

  private def sizeOf(function: SdlFunction, window: SdlWindow): Either[SdlError, (Int, Int)] =
    scratch { area =>
      val width  = area.allocate(ValueLayout.JAVA_INT)
      val height = area.allocate(ValueLayout.JAVA_INT)
      checked(function)(handle(function).invokeExact(window.handle, width, height): Boolean)
        .map(_ => (width.get(ValueLayout.JAVA_INT, 0L), height.get(ValueLayout.JAVA_INT, 0L)))
    }

  /** Surface pixels per logical pixel. */
  def pixelDensity(window: SdlWindow): Float = handle(GetWindowPixelDensity).invokeExact(window.handle): Float

  /** The scale the compositor asked for (1.5 stays 1.5). */
  def displayScale(window: SdlWindow): Float = handle(GetWindowDisplayScale).invokeExact(window.handle): Float

  /** The refresh rate of the display the window is on, when SDL knows it. */
  def refreshRate(window: SdlWindow): Option[Float] =
    val display = handle(GetDisplayForWindow).invokeExact(window.handle): Int
    val mode    = handle(GetCurrentDisplayMode).invokeExact(display): MemorySegment
    Option
      .when(mode.address() != 0L)(
        mode
          .reinterpret(SdlLayouts.offset("SDL_DisplayMode.refresh_rate") + 4L)
          .get(ValueLayout.JAVA_FLOAT_UNALIGNED, SdlLayouts.offset("SDL_DisplayMode.refresh_rate"))
      )
      .filter(_ > 1f)

  // ---- the software surface ----

  def surface(window: SdlWindow): Either[SdlError, SdlSurface] =
    val pointer = handle(GetWindowSurface).invokeExact(window.handle): MemorySegment
    if pointer.address() == 0L then Left(failed(GetWindowSurface))
    else
      val struct                  = pointer.reinterpret(SdlLayouts.SurfaceSize)
      def int(field: String): Int = struct.get(ValueLayout.JAVA_INT_UNALIGNED, SdlLayouts.offset(field))
      val pitch                   = int("SDL_Surface.pitch")
      val height                  = int("SDL_Surface.h")
      val pixels                  = struct.get(ValueLayout.ADDRESS_UNALIGNED, SdlLayouts.offset("SDL_Surface.pixels"))
      Right(
        SdlSurface(
          int("SDL_Surface.w"),
          height,
          pitch,
          int("SDL_Surface.format"),
          pixels.reinterpret(pitch.toLong * height)
        )
      )

  def updateSurface(window: SdlWindow): Either[SdlError, Unit] =
    checked(UpdateWindowSurface)(handle(UpdateWindowSurface).invokeExact(window.handle): Boolean)

  /** Presents the given rectangles of the surface. */
  def updateSurfaceRects(window: SdlWindow, rects: List[SdlRect]): Either[SdlError, Unit] =
    scratch { area =>
      val native = area.allocate(SdlLayouts.RectSize * rects.size.max(1), 4)
      rects.zipWithIndex.foreach { (rect, index) =>
        val base = index * SdlLayouts.RectSize
        native.set(ValueLayout.JAVA_INT_UNALIGNED, base + SdlLayouts.offset("SDL_Rect.x"), rect.x)
        native.set(ValueLayout.JAVA_INT_UNALIGNED, base + SdlLayouts.offset("SDL_Rect.y"), rect.y)
        native.set(ValueLayout.JAVA_INT_UNALIGNED, base + SdlLayouts.offset("SDL_Rect.w"), rect.width)
        native.set(ValueLayout.JAVA_INT_UNALIGNED, base + SdlLayouts.offset("SDL_Rect.h"), rect.height)
      }
      checked(UpdateWindowSurfaceRects)(
        handle(UpdateWindowSurfaceRects).invokeExact(window.handle, native, rects.size): Boolean
      )
    }

  /** 0 (`SdlLayouts.SurfaceVsyncDisabled`) to present at once, 1 to wait for the compositor's frame callback. */
  def setSurfaceVsync(window: SdlWindow, vsync: Int): Either[SdlError, Unit] =
    checked(SetWindowSurfaceVSync)(handle(SetWindowSurfaceVSync).invokeExact(window.handle, vsync): Boolean)

  // ---- OpenGL ----

  /** Sets an attribute the next window's context is created with: set them, then create the window. */
  def glSetAttribute(attribute: SdlLayouts.GlAttribute, value: Int): Either[SdlError, Unit] =
    checked(GlSetAttribute)(handle(GlSetAttribute).invokeExact(attribute.id, value): Boolean)

  /** A context for a window made with `SDL_WINDOW_OPENGL`; SDL's own text says why not, e.g. on a driver without GL. */
  def glCreateContext(window: SdlWindow): Either[SdlError, SdlGlContext] =
    val context = handle(GlCreateContext).invokeExact(window.handle): MemorySegment
    Either.cond(context.address() != 0L, SdlGlContext(context), failed(GlCreateContext))

  /** Makes `context` current on the calling thread. */
  def glMakeCurrent(window: SdlWindow, context: SdlGlContext): Either[SdlError, Unit] =
    checked(GlMakeCurrent)(handle(GlMakeCurrent).invokeExact(window.handle, context.handle): Boolean)

  def glDestroyContext(context: SdlGlContext): Unit =
    val _ = handle(GlDestroyContext).invokeExact(context.handle): Boolean

  /** 0 swaps at once, 1 waits for the display's refresh, -1 asks for late-swap tearing where the driver has it. */
  def glSetSwapInterval(interval: Int): Either[SdlError, Unit] =
    checked(GlSetSwapInterval)(handle(GlSetSwapInterval).invokeExact(interval): Boolean)

  /** Shows what was drawn since the last swap; with an interval of 1 it returns when the display is ready for more. */
  def glSwapWindow(window: SdlWindow): Either[SdlError, Unit] =
    checked(GlSwapWindow)(handle(GlSwapWindow).invokeExact(window.handle): Boolean)

  /** The address of a GL function of the current context, by name. An EGL implementation may answer for a name it does
    * not know, so a name that is not a GL function can still come back with an address.
    */
  def glProcAddress(name: String): Option[MemorySegment] =
    scratch { area =>
      val address = handle(GlGetProcAddress).invokeExact(area.allocateFrom(name)): MemorySegment
      Option.when(address.address() != 0L)(address)
    }

  // ---- events ----

  def pollEvent(): Either[SdlError, Option[SdlEvent]] =
    val any = handle(PollEvent).invokeExact(eventBuffer): Boolean
    Right(Option.when(any)(SdlEvent.decode(eventBuffer)))

  /** Waits up to `timeoutMillis` for an event. */
  def waitEvent(timeoutMillis: Int): Either[SdlError, Option[SdlEvent]] =
    val any = handle(WaitEventTimeout).invokeExact(eventBuffer, timeoutMillis): Boolean
    Right(Option.when(any)(SdlEvent.decode(eventBuffer)))

  /** Queues a quit event, as a window manager's close request would. */
  def pushQuit(): Either[SdlError, Unit] =
    scratch { area =>
      val event = area.allocate(SdlLayouts.EventSize, 8)
      event.set(ValueLayout.JAVA_INT, SdlLayouts.offset("SDL_CommonEvent.type"), SdlLayouts.eventType("SDL_EVENT_QUIT"))
      checked(PushEvent)(handle(PushEvent).invokeExact(event): Boolean)
    }

  // ---- text input and cursors ----

  def startTextInput(window: SdlWindow): Either[SdlError, Unit] =
    checked(StartTextInput)(handle(StartTextInput).invokeExact(window.handle): Boolean)

  def stopTextInput(window: SdlWindow): Either[SdlError, Unit] =
    checked(StopTextInput)(handle(StopTextInput).invokeExact(window.handle): Boolean)

  /** Tells the input method where the text being edited is, so it can put its candidate window beside it. */
  def setTextInputArea(window: SdlWindow, area: SdlRect, cursorOffset: Int): Either[SdlError, Unit] =
    scratch { scope =>
      val rect = scope.allocate(SdlLayouts.RectSize, 4)
      rect.set(ValueLayout.JAVA_INT_UNALIGNED, SdlLayouts.offset("SDL_Rect.x"), area.x)
      rect.set(ValueLayout.JAVA_INT_UNALIGNED, SdlLayouts.offset("SDL_Rect.y"), area.y)
      rect.set(ValueLayout.JAVA_INT_UNALIGNED, SdlLayouts.offset("SDL_Rect.w"), area.width)
      rect.set(ValueLayout.JAVA_INT_UNALIGNED, SdlLayouts.offset("SDL_Rect.h"), area.height)
      checked(SetTextInputArea)(handle(SetTextInputArea).invokeExact(window.handle, rect, cursorOffset): Boolean)
    }

  def keyName(key: Int): String = SdlEvent.cString(handle(GetKeyName).invokeExact(key): MemorySegment)

  def createSystemCursor(cursor: SdlLayouts.SystemCursor): Either[SdlError, SdlCursor] =
    val created = handle(CreateSystemCursor).invokeExact(cursor.id): MemorySegment
    Either.cond(created.address() != 0L, SdlCursor(created), failed(CreateSystemCursor))

  def setCursor(cursor: SdlCursor): Either[SdlError, Unit] =
    checked(SetCursor)(handle(SetCursor).invokeExact(cursor.handle): Boolean)

  def destroyCursor(cursor: SdlCursor): Unit = handle(DestroyCursor).invokeExact(cursor.handle): Unit

  // ---- clipboard and primary selection ----

  def setClipboardText(text: String): Either[SdlError, Unit] =
    scratch(area => checked(SetClipboardText)(handle(SetClipboardText).invokeExact(area.allocateFrom(text)): Boolean))

  def clipboardText: Either[SdlError, Option[String]] =
    textOf(HasClipboardText, GetClipboardText)

  def setPrimarySelectionText(text: String): Either[SdlError, Unit] =
    scratch(area =>
      checked(SetPrimarySelectionText)(handle(SetPrimarySelectionText).invokeExact(area.allocateFrom(text)): Boolean)
    )

  def primarySelectionText: Either[SdlError, Option[String]] =
    textOf(HasPrimarySelectionText, GetPrimarySelectionText)

  /** The text SDL holds for `get` when `has` says there is one; the C string SDL allocates is freed here. */
  private def textOf(has: SdlFunction, get: SdlFunction): Either[SdlError, Option[String]] =
    if !(handle(has).invokeExact(): Boolean) then Right(None)
    else
      val text = handle(get).invokeExact(): MemorySegment
      if text.address() == 0L then Left(failed(get))
      else
        try Right(Some(SdlEvent.cString(text)))
        finally handle(Free).invokeExact(text): Unit

  /** Unloads the library; no call on this instance or its windows may follow. */
  def close(): Unit = arena.close()

object Sdl:

  /** Loads libSDL3 (see [[SdlLibrary]]) and binds every function the binding calls. */
  def load(properties: Map[String, String], fileExists: Path => Boolean): Either[SdlError, Sdl] =
    val arena = Arena.ofShared()
    val loaded = SdlLibrary
      .load(SdlLibrary.candidates(properties, fileExists), SdlLibrary.open(_, arena))
      .flatMap((source, lookup) => SdlFunction.bindAll(lookup).map(handles => new Sdl(source, handles, arena)))
    loaded.left.foreach(_ => arena.close())
    loaded
