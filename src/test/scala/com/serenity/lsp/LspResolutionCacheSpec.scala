package com.serenity.lsp

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.lsp.config.{LanguageId, LspServerBinary, LspServerConfig}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LspResolutionCacheSpec extends AnyFlatSpec with Matchers:

  private val config = LspServerConfig(LanguageId.Scala, LspServerBinary.Metals)

  "LspResolutionCache" should "compute the resolution once per (languageId, fileUri) and reuse it after" in {
    val program = for
      cache     <- LspResolutionCache.empty
      callCount <- IO.ref(0)
      compute = callCount.update(_ + 1).as(Some(config -> "file:///workspace"))
      first  <- cache.resolve(LanguageId.Scala, "file:///workspace/Foo.scala")(compute)
      second <- cache.resolve(LanguageId.Scala, "file:///workspace/Foo.scala")(compute)
      calls  <- callCount.get
    yield (first, second, calls)

    val (first, second, calls) = program.unsafeRunSync()
    first shouldBe Some(config -> "file:///workspace")
    second shouldBe first
    calls shouldBe 1
  }

  it should "compute independently for different documents" in {
    val program = for
      cache     <- LspResolutionCache.empty
      callCount <- IO.ref(0)
      compute = callCount.update(_ + 1).as(Some(config -> "file:///workspace"))
      _     <- cache.resolve(LanguageId.Scala, "file:///workspace/Foo.scala")(compute)
      _     <- cache.resolve(LanguageId.Scala, "file:///workspace/Bar.scala")(compute)
      calls <- callCount.get
    yield calls

    program.unsafeRunSync() shouldBe 2
  }

  it should "compute independently for the same document under a different language" in {
    val program = for
      cache     <- LspResolutionCache.empty
      callCount <- IO.ref(0)
      compute = callCount.update(_ + 1).as(Some(config -> "file:///workspace"))
      _     <- cache.resolve(LanguageId.Scala, "file:///workspace/Foo.scala")(compute)
      _     <- cache.resolve(LanguageId.Python, "file:///workspace/Foo.scala")(compute)
      calls <- callCount.get
    yield calls

    program.unsafeRunSync() shouldBe 2
  }

  it should "cache a None resolution and not recompute it" in {
    val program = for
      cache     <- LspResolutionCache.empty
      callCount <- IO.ref(0)
      compute = callCount.update(_ + 1).as(None)
      first  <- cache.resolve(LanguageId.Scala, "file:///workspace/Foo.scala")(compute)
      second <- cache.resolve(LanguageId.Scala, "file:///workspace/Foo.scala")(compute)
      calls  <- callCount.get
    yield (first, second, calls)

    val (first, second, calls) = program.unsafeRunSync()
    first shouldBe None
    second shouldBe None
    calls shouldBe 1
  }
