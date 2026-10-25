package com.serenity.ui.sdl

import java.nio.file.Path

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The shim ships beside libSDL3, so it is found where libSDL3 is. */
class GlxShimSpec extends AnyFlatSpec with Matchers:

  "GlxShim.candidate" should "be the shim file next to the configured SDL library directory" in {
    val properties = Map(SdlLibrary.PathProperty -> "/opt/serenity/natives")
    GlxShim.candidate(properties, _ => true) shouldBe Some(Path.of("/opt/serenity/natives", "libserenity-glx-shim.so"))
  }

  it should "be the shim file next to the configured SDL library file" in {
    val properties = Map(SdlLibrary.PathProperty -> "/opt/serenity/natives/libSDL3.so.0")
    GlxShim.candidate(properties, _ => true) shouldBe Some(Path.of("/opt/serenity/natives", "libserenity-glx-shim.so"))
  }

  it should "be absent when no SDL library path is configured or the file is not there" in {
    GlxShim.candidate(Map.empty, _ => true) shouldBe None
    GlxShim.candidate(Map(SdlLibrary.PathProperty -> "/opt/serenity/natives"), _ => false) shouldBe None
  }

  "GlxShim.load" should "refuse, without trying, anywhere but Linux" in {
    GlxShim.load(Path.of("libserenity-glx-shim.so"), osName = "Windows 11") shouldBe
      Left(SdlError.Call("GlxShim.load", "the GLX shim is for Linux, not Windows 11"))
    GlxShim.load(Path.of("libserenity-glx-shim.so"), osName = "Mac OS X").isLeft shouldBe true
  }

  it should "say what dlopen said when the file is not a library" in {
    assume(sys.props.get("os.name").exists(_.toLowerCase.contains("linux")), "the shim loads on Linux only")
    GlxShim.load(Path.of("/nonexistent/libserenity-glx-shim.so")) match
      case Left(SdlError.Call("dlopen", message)) => message should include("libserenity-glx-shim.so")
      case other                                  => fail(s"expected a dlopen failure, got $other")
  }
