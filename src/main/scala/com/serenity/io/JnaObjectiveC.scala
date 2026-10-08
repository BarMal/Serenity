package com.serenity.io

import java.nio.charset.StandardCharsets

import com.sun.jna.{Memory, NativeLibrary, Pointer}

/** The Objective-C runtime through JNA, the FFI the Windows Common Item Dialog already uses. It cannot run off macOS,
  * so it holds nothing but the calls: what to send is decided in [[OpenPanelScript]], which a recording double checks.
  *
  * `objc_msgSend` is declared variadic in the headers, but the runtime reads its arguments from registers and jumps to
  * the method, so it is called here as the fixed-arity function the method actually is. A variadic call would put the
  * arguments in different places on arm64.
  */
private[io] object JnaObjectiveC extends ObjectiveC:

  // AppKit is linked into every AWT process on macOS; loading it by name guarantees NSOpenPanel is registered.
  private lazy val appKit      = NativeLibrary.getInstance("AppKit")
  private lazy val runtime     = NativeLibrary.getInstance("objc")
  private lazy val getClass    = runtime.getFunction("objc_getClass")
  private lazy val selector    = runtime.getFunction("sel_registerName")
  private lazy val messageSend = runtime.getFunction("objc_msgSend")

  def classNamed(name: String): Long =
    val _ = appKit
    getClass.invokeLong(Array[AnyRef](name))

  def send(receiver: Long, selectorName: String, arguments: Long*): Long =
    val sel  = selector.invokeLong(Array[AnyRef](selectorName))
    val args = Array[AnyRef](Long.box(receiver), Long.box(sel)) ++ arguments.map(Long.box)
    messageSend.invokeLong(args)

  def withUtf8[A](text: String)(use: Long => A): A =
    val bytes  = text.getBytes(StandardCharsets.UTF_8)
    val memory = new Memory(bytes.length.toLong + 1L)
    try
      memory.clear()
      memory.write(0L, bytes, 0, bytes.length)
      use(Pointer.nativeValue(memory))
    finally memory.close()

  def readUtf8(cString: Long): Option[String] =
    Option.when(cString != 0L)(new Pointer(cString).getString(0L, "UTF-8"))
