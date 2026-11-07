package com.serenity.ui.skia

import java.lang.foreign.{Arena, FunctionDescriptor, Linker, MemorySegment, ValueLayout}
import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Skia finds GL functions by calling a stub with a name; the stub asks whoever owns the context. These specs call the
  * stub the way Skia does, through its address, so no GL context and no window are needed.
  */
class SkiaGlProcsSpec extends AnyFlatSpec with Matchers:

  private val getProc = FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS)

  private def callThrough(procs: SkiaGlProcs, name: String): Long =
    val call = Linker.nativeLinker().downcallHandle(MemorySegment.ofAddress(procs.address), getProc)
    Using.resource(Arena.ofConfined()) { arena =>
      val answer = call.invokeExact(MemorySegment.NULL, arena.allocateFrom(name)): MemorySegment
      answer.address()
    }

  "The GL procedure stub" should "answer a name with the address the lookup gives for it" in
    Using.resource(SkiaGlProcs(Map("glClear" -> 0x1234L).getOrElse(_, 0L))) { procs =>
      callThrough(procs, "glClear") shouldBe 0x1234L
    }

  it should "pass the lookup the name Skia asked for, exactly" in {
    val asked = new ConcurrentLinkedQueue[String]()
    Using.resource(SkiaGlProcs { name =>
      val _ = asked.add(name)
      1L
    }) { procs =>
      callThrough(procs, "glGetIntegerv")
      callThrough(procs, "glBindFramebuffer")
    }
    asked.asScala.toList shouldBe List("glGetIntegerv", "glBindFramebuffer")
  }

  it should "answer no address for a name the lookup does not know" in
    Using.resource(SkiaGlProcs(Map("glClear" -> 0x1234L).getOrElse(_, 0L))) { procs =>
      callThrough(procs, "glNoSuchFunction") shouldBe 0L
    }

  it should "answer no address, and not take the process down, when the lookup throws" in
    Using.resource(SkiaGlProcs(_ => throw new IllegalStateException("context lost"))) { procs =>
      callThrough(procs, "glClear") shouldBe 0L
    }

  "An assembled interface" should "be refused, with Skia's reason, when the lookup knows no GL function" in
    Using.resource(SkiaGlProcs(_ => 0L))(procs => procs.assembled().left.map(_.nonEmpty) shouldBe Left(true))
