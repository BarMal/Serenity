package com.serenity.io

import com.sun.jna.{Callback, CallbackReference, Memory, NativeLibrary, Pointer}

/** Queues work on the AppKit main thread's run loop through a CoreFoundation run loop source, which is what
  * [[MainThreadHandoff]] needs and the JVM does not offer on its own.
  *
  * The source is added to the modes the JDK itself performs main-thread work in (`ThreadUtilities.m`'s `javaModes`):
  * the default, modal panel and event tracking modes, and `AWTRunLoopMode`, the mode AWT runs the main thread in. The
  * modal mode is the one that matters while a panel is up: AWT calls from the event thread into AppKit wait for the
  * main thread, and they are served from inside the panel's modal loop only because that mode is among them.
  *
  * It cannot run off macOS. The work it queues is the only code that runs on the main thread, and it installs no
  * delegate on the panel, so AppKit never calls back into Java while the panel is up.
  */
private[io] object CoreFoundationMainLoop:

  private val Modes =
    List("kCFRunLoopDefaultMode", "NSModalPanelRunLoopMode", "NSEventTrackingRunLoopMode", "AWTRunLoopMode")

  private val Utf8Encoding  = 0x08000100
  private val PerformOffset = 72L
  private val ContextSize   = 80L

  trait Perform extends Callback:
    def callback(info: Pointer): Unit

  private lazy val coreFoundation = NativeLibrary.getInstance("CoreFoundation")

  def isLoadable: Boolean =
    val _ = coreFoundation.getFunction("CFRunLoopGetMain")
    true

  private def call(name: String, arguments: AnyRef*): Pointer =
    coreFoundation.getFunction(name).invokePointer(arguments.toArray)

  private def callVoid(name: String, arguments: AnyRef*): Unit =
    coreFoundation.getFunction(name).invokeVoid(arguments.toArray)

  def schedule(work: Runnable): () => Unit =
    val perform: Perform = new Perform:
      def callback(info: Pointer): Unit = work.run()
    val context = new Memory(ContextSize)
    context.clear()
    context.setPointer(PerformOffset, CallbackReference.getFunctionPointer(perform))
    val source   = call("CFRunLoopSourceCreate", Pointer.NULL, Long.box(0L), context)
    val mainLoop = call("CFRunLoopGetMain")
    val modes    = Modes.map(mode => call("CFStringCreateWithCString", Pointer.NULL, mode, Int.box(Utf8Encoding)))
    modes.foreach(mode => callVoid("CFRunLoopAddSource", mainLoop, source, mode))
    callVoid("CFRunLoopSourceSignal", source)
    callVoid("CFRunLoopWakeUp", mainLoop)
    () =>
      callVoid("CFRunLoopSourceInvalidate", source)
      callVoid("CFRelease", source)
      modes.foreach(callVoid("CFRelease", _))
      // The source holds only the function pointer, so the callback and its context must outlive the source.
      java.lang.ref.Reference.reachabilityFence(perform)
      java.lang.ref.Reference.reachabilityFence(context)
