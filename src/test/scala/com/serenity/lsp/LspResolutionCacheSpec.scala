package com.serenity.lsp

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.lsp.client.{DocumentUri, WorkspaceRootUri}
import com.serenity.lsp.config.{LanguageId, LspServerBinary, LspServerConfig}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LspResolutionCacheSpec extends AnyFlatSpec with Matchers:

  private val config  = LspServerConfig(LanguageId.Scala, LspServerBinary.Metals)
  private val rootUri = WorkspaceRootUri("file:///workspace")

  "LspResolutionCache" should "compute the resolution once per (languageId, fileUri) and reuse it after" in {
    val program = for
      cache     <- LspResolutionCache.empty
      callCount <- IO.ref(0)
      compute = callCount.update(_ + 1).as(Some(config -> rootUri))
      first  <- cache.resolve(LanguageId.Scala, DocumentUri("file:///workspace/Foo.scala"))(compute)
      second <- cache.resolve(LanguageId.Scala, DocumentUri("file:///workspace/Foo.scala"))(compute)
      calls  <- callCount.get
    yield (first, second, calls)

    val (first, second, calls) = program.unsafeRunSync()
    first shouldBe Some(config -> rootUri)
    second shouldBe first
    calls shouldBe 1
  }

  it should "compute independently for different documents" in {
    val program = for
      cache     <- LspResolutionCache.empty
      callCount <- IO.ref(0)
      compute = callCount.update(_ + 1).as(Some(config -> rootUri))
      _     <- cache.resolve(LanguageId.Scala, DocumentUri("file:///workspace/Foo.scala"))(compute)
      _     <- cache.resolve(LanguageId.Scala, DocumentUri("file:///workspace/Bar.scala"))(compute)
      calls <- callCount.get
    yield calls

    program.unsafeRunSync() shouldBe 2
  }

  it should "compute independently for the same document under a different language" in {
    val program = for
      cache     <- LspResolutionCache.empty
      callCount <- IO.ref(0)
      compute = callCount.update(_ + 1).as(Some(config -> rootUri))
      _     <- cache.resolve(LanguageId.Scala, DocumentUri("file:///workspace/Foo.scala"))(compute)
      _     <- cache.resolve(LanguageId.Python, DocumentUri("file:///workspace/Foo.scala"))(compute)
      calls <- callCount.get
    yield calls

    program.unsafeRunSync() shouldBe 2
  }

  it should "cache a None resolution and not recompute it" in {
    val program = for
      cache     <- LspResolutionCache.empty
      callCount <- IO.ref(0)
      compute = callCount.update(_ + 1).as(None)
      first  <- cache.resolve(LanguageId.Scala, DocumentUri("file:///workspace/Foo.scala"))(compute)
      second <- cache.resolve(LanguageId.Scala, DocumentUri("file:///workspace/Foo.scala"))(compute)
      calls  <- callCount.get
    yield (first, second, calls)

    val (first, second, calls) = program.unsafeRunSync()
    first shouldBe None
    second shouldBe None
    calls shouldBe 1
  }

  it should "recompute after the entry for a (languageId, fileUri) is evicted" in {
    val program = for
      cache     <- LspResolutionCache.empty
      callCount <- IO.ref(0)
      compute = callCount.update(_ + 1).as(Some(config -> rootUri))
      _      <- cache.resolve(LanguageId.Scala, DocumentUri("file:///workspace/Foo.scala"))(compute)
      _      <- cache.evict(LanguageId.Scala, DocumentUri("file:///workspace/Foo.scala"))
      second <- cache.resolve(LanguageId.Scala, DocumentUri("file:///workspace/Foo.scala"))(compute)
      calls  <- callCount.get
    yield (second, calls)

    val (second, calls) = program.unsafeRunSync()
    second shouldBe Some(config -> rootUri)
    calls shouldBe 2
  }

  it should "leave other documents' cached entries untouched when evicting one" in {
    val program = for
      cache     <- LspResolutionCache.empty
      callCount <- IO.ref(0)
      compute = callCount.update(_ + 1).as(Some(config -> rootUri))
      _     <- cache.resolve(LanguageId.Scala, DocumentUri("file:///workspace/Foo.scala"))(compute)
      _     <- cache.resolve(LanguageId.Scala, DocumentUri("file:///workspace/Bar.scala"))(compute)
      _     <- cache.evict(LanguageId.Scala, DocumentUri("file:///workspace/Foo.scala"))
      _     <- cache.resolve(LanguageId.Scala, DocumentUri("file:///workspace/Bar.scala"))(compute)
      calls <- callCount.get
    yield calls

    program.unsafeRunSync() shouldBe 2
  }

  it should "no-op when evicting a (languageId, fileUri) with no cached entry" in {
    val program = for
      cache <- LspResolutionCache.empty
      _     <- cache.evict(LanguageId.Scala, DocumentUri("file:///workspace/Never.scala"))
    yield ()

    noException should be thrownBy program.unsafeRunSync()
  }
