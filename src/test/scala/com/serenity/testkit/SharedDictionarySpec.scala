package com.serenity.testkit

import java.util.concurrent.{CountDownLatch, TimeUnit}

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.spellcheck.DictionaryCache
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SharedDictionarySpec extends AnyFlatSpec with Matchers:

  private val defaults = AppConfig.default.languageToolsConfig.spellCheck

  private val french =
    val tools = AppConfig.default.languageToolsConfig
    AppConfig.default.copy(languageToolsConfig = tools.copy(spellCheck = defaults.copy(languages = List("fr"))))

  /** A dictionary whose warm-up runs until `release` is counted down. */
  private def heldUntil(release: CountDownLatch): SharedDictionary =
    SharedDictionary(defaults, (_: SpellCheckConfig, _: DictionaryCache) => release.await())

  private def released[A](test: CountDownLatch => A): A =
    val release = CountDownLatch(1)
    try test(release)
    finally release.countDown()

  "The shared dictionary" should "hand every manager checking against it the same cache" in {
    val shared = SharedDictionary(defaults, (_, _) => ())

    shared.cacheFor(AppConfig.default) should be theSameInstanceAs shared.cacheFor(AppConfig.default)
  }

  it should "give a manager checking against another dictionary a cache of its own, without waiting for the warm-up" in
    released { release =>
      val shared = heldUntil(release)

      shared.cacheFor(french) should not be theSameInstanceAs(shared.cacheFor(french))
    }

  it should "hand out its cache only once the warm-up has finished" in released { release =>
    val shared  = heldUntil(release)
    val handout = IO.blocking(shared.cacheFor(AppConfig.default)).unsafeToFuture()

    IO.sleep(100.millis).unsafeRunSync()
    handout.isCompleted shouldBe false
    release.countDown()
    IO.fromFuture(IO.pure(handout)).timeout(10.seconds).unsafeRunSync() should be theSameInstanceAs
      shared.cacheFor(AppConfig.default)
  }

  // Every compute thread is made to wait; a design that loses them would never run another fiber, so the test thread
  // only ever waits on the runtime with a deadline, and releases the warm-up whatever happens.
  it should "not stall the IO runtime while compute threads wait for the warm-up" in released { release =>
    val shared  = heldUntil(release)
    val workers = Runtime.getRuntime.availableProcessors
    val entered = CountDownLatch(workers)
    val waiting = List
      .fill(workers)(IO(entered.countDown()) >> IO(shared.cacheFor(AppConfig.default)))
      .parSequence
      .unsafeToFuture()

    entered.await(10, TimeUnit.SECONDS) shouldBe true
    IO(42).unsafeRunTimed(10.seconds) shouldBe Some(42)
    release.countDown()
    IO.fromFuture(IO.pure(waiting)).timeout(10.seconds).unsafeRunSync().distinct.size shouldBe 1
  }
