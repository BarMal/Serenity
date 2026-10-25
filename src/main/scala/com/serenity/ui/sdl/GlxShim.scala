package com.serenity.ui.sdl

import java.lang.foreign.{Arena, FunctionDescriptor, Linker, MemorySegment, SymbolLookup, ValueLayout}
import java.nio.file.Path

import scala.jdk.OptionConverters.*
import scala.util.Try

/** Makes Skiko's native library usable on an EGL context (#2380).
  *
  * Skiko's Skia is built for GLX: `DirectContext.makeGL()` asks `glXGetCurrentContext` whether a context is current and
  * gives up when it says no, which it always does for the EGL context SDL makes on Wayland. The shim is one function
  * that answers yes. Loading it with `RTLD_GLOBAL` before Skiko's library puts it ahead of libGL's in symbol lookup, so
  * Skiko's call lands here; the GL functions themselves still resolve through `glXGetProcAddress`, whose dispatch
  * follows whichever context is current.
  */
object GlxShim:

  val FileName: String = "libserenity-glx-shim.so"

  private val RtldNow    = 0x2
  private val RtldGlobal = 0x100

  /** The shim beside the SDL library the properties point at, when it is there. */
  def candidate(properties: Map[String, String], fileExists: Path => Boolean): Option[Path] =
    properties
      .get(SdlLibrary.PathProperty)
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(Path.of(_))
      .map { configured =>
        val directory =
          if configured.getFileName.toString.contains(".so") then Option(configured.getParent).getOrElse(Path.of("."))
          else configured
        directory.resolve(FileName)
      }
      .filter(fileExists)

  /** Loads the shim into the global symbol scope. It stays loaded for the life of the process. */
  def load(shim: Path): Either[SdlError, Unit] =
    val linker = Linker.nativeLinker()
    // glibc before 2.34 keeps dlopen in libdl
    val lookups =
      linker.defaultLookup() :: Try(SymbolLookup.libraryLookup("libdl.so.2", Arena.global())).toOption.toList
    def find(name: String): Option[MemorySegment] = lookups.iterator.flatMap(_.find(name).toScala).nextOption()

    (find("dlopen"), find("dlerror")) match
      case (Some(dlopen), Some(dlerror)) =>
        val open = linker.downcallHandle(
          dlopen,
          FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.JAVA_INT)
        )
        val error = linker.downcallHandle(dlerror, FunctionDescriptor.of(ValueLayout.ADDRESS))
        val area  = Arena.ofConfined()
        try
          val library = open.invokeExact(area.allocateFrom(shim.toString), RtldNow | RtldGlobal): MemorySegment
          if library.address() != 0L then Right(())
          else Left(SdlError.Call("dlopen", SdlEvent.cString(error.invokeExact(): MemorySegment)))
        finally area.close()
      case _ => Left(SdlError.MissingFunction("dlopen"))
