package com.serenity.ui.sdl

import java.lang.foreign.{FunctionDescriptor, Linker, MemorySegment, ValueLayout}

import com.serenity.testkit.SdlNativeTest
import org.scalatest.DoNotDiscover
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The built shim, found next to libSDL3 (`-Dserenity.sdl3.library.path`), becomes visible to every library loaded
  * afterwards, which is how Skiko's native library finds `glXGetCurrentContext` before libGL's.
  */
@DoNotDiscover
class GlxShimNativeSpec extends AnyFlatSpec with Matchers:

  "GlxShim.load" should "put glXGetCurrentContext in the global symbol scope" taggedAs SdlNativeTest in {
    val shim = GlxShim.candidate(sys.props.toMap, java.nio.file.Files.exists(_))
    shim should not be empty
    GlxShim.load(shim.get) shouldBe Right(())
    inGlobalScope("glXGetCurrentContext") shouldBe true
    inGlobalScope("glXNoSuchFunction") shouldBe false
  }

  /** What the loader finds for a library loaded after the shim: `dlsym` over the global scope (`RTLD_DEFAULT`). */
  private def inGlobalScope(symbol: String): Boolean =
    val linker = Linker.nativeLinker()
    val dlsym  = linker.defaultLookup().find("dlsym").orElseThrow()
    val find =
      linker.downcallHandle(dlsym, FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS))
    val area = java.lang.foreign.Arena.ofConfined()
    try (find.invokeExact(MemorySegment.NULL, area.allocateFrom(symbol)): MemorySegment).address() != 0L
    finally area.close()
