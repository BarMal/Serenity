package com.serenity.spellcheck

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.parallel.*
import com.serenity.config.SpellCheckConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Several analysis services over one injected [[DictionaryCache]] (as the benchmark harness builds them) parse and
  * merge the dictionary once between them, not once each.
  */
class DictionaryCacheSharingSpec extends AnyFlatSpec with Matchers:

  private val inputs = DictionaryMergeInputs(SpellCheckConfig(), Nil, List("en-GB"))

  "A dictionary cache asked for the same merge from many threads at once" should "build it once" in {
    val cache    = DictionaryCache()
    val builds   = AtomicInteger(0)
    val snapshot = DictionarySnapshot(DictionaryContext(Set.empty, Map.empty, Nil), Nil)

    val results = List
      .fill(16)(
        IO.blocking(
          cache.mergedFor(
            inputs,
            () =>
              builds.incrementAndGet()
              Thread.sleep(50)
              snapshot
          )
        )
      )
      .parSequence
      .timeout(30.seconds)
      .unsafeRunSync()

    builds.get shouldBe 1
    all(results) should be theSameInstanceAs snapshot
  }

  "Two configurations that differ only in custom words" should "share the one parsed dictionary" in {
    val cache = DictionaryCache()
    val first = DictionaryLoader.loadSnapshot(SpellCheckConfig(), cache, Nil)
    val second =
      DictionaryLoader.loadSnapshot(SpellCheckConfig(additionalWords = List("serenity")), cache, Nil)

    first.context.stems.nonEmpty shouldBe true
    second.context.stems.zip(first.context.stems).foreach((a, b) => a should be theSameInstanceAs b)
  }
