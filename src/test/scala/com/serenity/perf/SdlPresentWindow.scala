package com.serenity.perf

import java.lang.foreign.{Arena, FunctionDescriptor, Linker, MemorySegment, SymbolLookup, ValueLayout}
import java.lang.invoke.MethodHandle
import java.nio.file.Files
import java.util.Locale

import scala.concurrent.duration.FiniteDuration
import scala.jdk.OptionConverters.*

import cats.effect.IO
import com.serenity.ui.sdl.{SdlGlWindow, SdlLayouts, SdlLibrary, SdlWindow}

/** How the SDL3 present benchmark asks for its window: a logical size, or the whole display. */
enum WindowRequest:
  case Logical(width: Int, height: Int)
  case Fullscreen

object WindowRequest:

  private val Property = "serenity.sdl3.window"
  private val Shape    = raw"(\d+)x(\d+)".r

  val default: Logical = Logical(LaptopFrameBenchmarks.LogicalWidthPx, LaptopFrameBenchmarks.LogicalHeightPx)

  /** `-Dserenity.sdl3.window=fullscreen` or `=WIDTHxHEIGHT` (logical pixels); anything else is the laptop size. */
  def parse(properties: Map[String, String]): WindowRequest =
    properties
      .get(Property)
      .map(_.trim.toLowerCase(Locale.ROOT))
      .collect {
        case "fullscreen"                                           => Fullscreen
        case Shape(width, height) if width.toInt * height.toInt > 0 => Logical(width.toInt, height.toInt)
      }
      .getOrElse(default)

/** A window that was asked to change size has it some time later, when the compositor answers. */
object SizeSettling:

  /** The size the last `steady` readings agree on, once there are that many. Readings of `awayFrom`, the size the
    * window had before it was asked to change, do not count until it has left it: the compositor may not have answered
    * yet.
    */
  def settled(readings: Vector[(Int, Int)], steady: Int, awayFrom: Option[(Int, Int)] = None): Option[(Int, Int)] =
    val answered = awayFrom.fold(readings)(start => readings.dropWhile(_ == start))
    Option
      .when(steady > 0 && answered.size >= steady)(answered.takeRight(steady))
      .filter(_.distinct.size == 1)
      .flatMap(_.headOption)

private[perf] object SdlPresentWindow:

  /** The title `bench/hyprland-float.sh` floats: Hyprland matches a rule's regex against the whole title. */
  val Title = "Serenity render benchmark"

  /** Why the numbers are not for the size that was asked for, when the window has other pixels than it. */
  def sizeNote(requestedLogical: (Int, Int), pixels: (Int, Int), scale: Double, fix: String): Option[String] =
    val wanted = (math.round(requestedLogical._1 * scale).toInt, math.round(requestedLogical._2 * scale).toInt)
    Option.when((pixels._1 - wanted._1).abs > 1 || (pixels._2 - wanted._2).abs > 1)(
      s"size_mismatch: window is ${pixels._1}x${pixels._2}px, ${wanted._1}x${wanted._2}px was asked for; $fix"
    )

  /** What the display the window is on has, in logical pixels and at what pixel density. */
  final case class DisplaySize(logicalWidth: Int, logicalHeight: Int, density: Double)

  /** The SDL3 calls the binding does not have (fullscreen, the display's size), found in the same libSDL3 and made from
    * the test, so the benchmark needs no production change. Call from the SDL thread.
    */
  final class FullscreenCalls private (
      setFullscreen: MethodHandle,
      displayOf: MethodHandle,
      currentMode: MethodHandle
  ):

    def enter(window: SdlWindow): Boolean = setFullscreen.invokeExact(window.handle, true): Boolean

    def displaySize(window: SdlWindow): Option[DisplaySize] =
      val display = displayOf.invokeExact(window.handle): Int
      val mode    = currentMode.invokeExact(display): MemorySegment
      Option.when(mode.address() != 0L) {
        val seen                    = mode.reinterpret(SdlLayouts.offset("sizeof(SDL_DisplayMode)"))
        def int(field: String): Int = seen.get(ValueLayout.JAVA_INT_UNALIGNED, SdlLayouts.offset(field))
        DisplaySize(
          int("SDL_DisplayMode.w"),
          int("SDL_DisplayMode.h"),
          seen.get(ValueLayout.JAVA_FLOAT_UNALIGNED, SdlLayouts.offset("SDL_DisplayMode.pixel_density")).toDouble
        )
      }

  object FullscreenCalls:

    def load(properties: Map[String, String]): Either[String, FullscreenCalls] =
      val arena = Arena.global()
      SdlLibrary
        .load(SdlLibrary.candidates(properties, Files.exists(_)), SdlLibrary.open(_, arena))
        .left
        .map(_.describe)
        .flatMap((_, lookup) => bind(lookup))

    private def bind(lookup: SymbolLookup): Either[String, FullscreenCalls] =
      val linker = Linker.nativeLinker()
      def handle(name: String, descriptor: FunctionDescriptor): Either[String, MethodHandle] =
        lookup.find(name).toScala.map(linker.downcallHandle(_, descriptor)).toRight(s"$name not found in libSDL3")
      for
        fullscreen <- handle(
          "SDL_SetWindowFullscreen",
          FunctionDescriptor.of(ValueLayout.JAVA_BOOLEAN, ValueLayout.ADDRESS, ValueLayout.JAVA_BOOLEAN)
        )
        display <- handle(
          "SDL_GetDisplayForWindow",
          FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.ADDRESS)
        )
        mode <- handle(
          "SDL_GetCurrentDisplayMode",
          FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.JAVA_INT)
        )
      yield new FullscreenCalls(fullscreen, display, mode)

  /** The readings of the window's pixel size, one every `pause`, until `steady` in a row agree or `attempts` run out.
    * With `awayFrom` the size the window started at is not an answer (see [[SizeSettling.settled]]).
    */
  def settledPixels(
    window: SdlGlWindow,
    steady: Int,
    attempts: Int,
    pause: FiniteDuration,
    awayFrom: Option[(Int, Int)] = None
  ): IO[(Int, Int)] =
    def go(readings: Vector[(Int, Int)], left: Int): IO[(Int, Int)] =
      window.pixelSize.flatMap { now =>
        val all = readings :+ now
        SizeSettling.settled(all, steady, awayFrom) match
          case Some(size)        => IO.pure(size)
          case None if left <= 0 => IO.pure(now)
          case None              => IO.sleep(pause) *> go(all, left - 1)
      }
    go(Vector.empty, attempts)
