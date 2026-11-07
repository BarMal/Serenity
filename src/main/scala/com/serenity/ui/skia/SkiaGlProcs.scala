package com.serenity.ui.skia

import java.lang.foreign.{Arena, FunctionDescriptor, Linker, MemorySegment, ValueLayout}
import java.lang.invoke.{MethodHandles, MethodType}

import scala.annotation.unused
import scala.util.Try

import org.jetbrains.skia.GLAssembledInterface
import org.jetbrains.skiko.Library

/** How Skia finds GL functions on a context it did not make (#2380). Skiko's own lookup goes through GLX and gives up
  * on an EGL context; instead Skia is handed this stub, which has the signature of `GrGLGetProc` (`void* (*)(void* ctx,
  * const char* name)`) and answers every name from `lookup`, the window's `SDL_GL_GetProcAddress`.
  *
  * A name `lookup` has no address for, or a `lookup` that fails, is answered with no address, which Skia reads as "this
  * function is missing"; an exception must not cross back into native code. Skia may ask from any thread, so `lookup`
  * has to be callable from whichever thread creates the `DirectContext`, which for a window is the one its context is
  * current on.
  *
  * Close it only after the `DirectContext` built over it is gone: Skia keeps the stub's address.
  */
final class SkiaGlProcs private (lookup: String => Long) extends AutoCloseable:

  private val arena = Arena.ofShared()

  // Bound by name into the upcall stub, so the compiler sees no caller.
  private[skia] def answer(@unused context: MemorySegment, name: MemorySegment): MemorySegment =
    MemorySegment.ofAddress(Try(lookup(name.reinterpret(Long.MaxValue).getString(0))).getOrElse(0L))

  private val stub: MemorySegment =
    val target = MethodHandles
      .lookup()
      .bind(
        this,
        "answer",
        MethodType.methodType(classOf[MemorySegment], classOf[MemorySegment], classOf[MemorySegment])
      )
    Linker
      .nativeLinker()
      .upcallStub(
        target,
        FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS),
        arena
      )

  /** The stub's address, as Skia takes it. */
  def address: Long = stub.address()

  /** The interface `DirectContext.makeGLWithInterface` takes, which the caller closes with the context; or Skia's
    * reason for refusing, which is what a lookup that cannot find the functions Skia requires gets.
    */
  def assembled(): Either[String, GLAssembledInterface] =
    Try {
      Library.INSTANCE.load()
      GLAssembledInterface.Companion.createFromNativePointers(0L, address)
    }.toEither.left.map(failure => Option(failure.getMessage).getOrElse(failure.getClass.getName))

  def close(): Unit = arena.close()

object SkiaGlProcs:

  def apply(lookup: String => Long): SkiaGlProcs = new SkiaGlProcs(lookup)
