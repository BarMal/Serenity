package com.serenity.ui.sdl

import java.lang.foreign.{Arena, FunctionDescriptor, Linker, MemorySegment, ValueLayout}
import java.lang.invoke.MethodHandle

/** The few GL calls the GL specs make to see what a context holds, reached through `SDL_GL_GetProcAddress` like any GL
  * function.
  */
final class GlProbe(sdl: Sdl):

  private def call(name: String, descriptor: FunctionDescriptor): MethodHandle =
    val address = sdl.glProcAddress(name).getOrElse(throw new IllegalStateException(s"no $name"))
    Linker.nativeLinker().downcallHandle(address, descriptor)

  private val getString = call("glGetString", FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.JAVA_INT))

  private val readPixels = call(
    "glReadPixels",
    FunctionDescriptor.ofVoid(
      ValueLayout.JAVA_INT,
      ValueLayout.JAVA_INT,
      ValueLayout.JAVA_INT,
      ValueLayout.JAVA_INT,
      ValueLayout.JAVA_INT,
      ValueLayout.JAVA_INT,
      ValueLayout.ADDRESS
    )
  )

  private val GlRenderer = 0x1f01
  private val GlRgba     = 0x1908
  private val GlUnsigned = 0x1401

  def renderer: String =
    (getString.invokeExact(GlRenderer): MemorySegment).reinterpret(256).getString(0)

  /** The RGBA bytes of the pixel at (`x`, `y`) in the framebuffer being drawn to. */
  def pixel(x: Int, y: Int): Vector[Int] =
    val area = Arena.ofConfined()
    try
      val out = area.allocate(4L)
      readPixels.invokeExact(x, y, 1, 1, GlRgba, GlUnsigned, out): Unit
      Vector.tabulate(4)(index => out.get(ValueLayout.JAVA_BYTE, index.toLong) & 0xff)
    finally area.close()
