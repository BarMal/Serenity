package com.serenity.ui.sdl

import java.lang.foreign.{FunctionDescriptor, Linker, MemorySegment, ValueLayout}
import java.lang.invoke.MethodHandle

/** The handful of GL functions the window host calls itself, found on the current context through SDL. The renderer
  * that comes later brings its own GL (Skia); this is only what a window needs to clear and to say what it is drawing
  * with. Call from the thread the context is current on.
  */
final private[sdl] class GlCalls private (
    clearColor: MethodHandle,
    clearBuffers: MethodHandle,
    getString: MethodHandle
):

  def clear(red: Float, green: Float, blue: Float, alpha: Float): Unit =
    clearColor.invokeExact(red, green, blue, alpha): Unit
    clearBuffers.invokeExact(GlCalls.ColorBufferBit): Unit

  def renderer: String =
    val text = getString.invokeExact(GlCalls.Renderer): MemorySegment
    if text.address() == 0L then "" else SdlEvent.cString(text)

private[sdl] object GlCalls:

  private val ColorBufferBit = 0x4000
  private val Renderer       = 0x1f01

  def load(sdl: Sdl): Either[SdlError, GlCalls] =
    val linker = Linker.nativeLinker()
    def bind(name: String, descriptor: FunctionDescriptor): Either[SdlError, MethodHandle] =
      sdl.glProcAddress(name).map(linker.downcallHandle(_, descriptor)).toRight(SdlError.MissingFunction(name))
    for
      color <- bind(
        "glClearColor",
        FunctionDescriptor.ofVoid(
          ValueLayout.JAVA_FLOAT,
          ValueLayout.JAVA_FLOAT,
          ValueLayout.JAVA_FLOAT,
          ValueLayout.JAVA_FLOAT
        )
      )
      clear  <- bind("glClear", FunctionDescriptor.ofVoid(ValueLayout.JAVA_INT))
      string <- bind("glGetString", FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.JAVA_INT))
    yield new GlCalls(color, clear, string)
