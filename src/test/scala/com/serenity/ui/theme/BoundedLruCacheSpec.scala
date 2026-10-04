package com.serenity.ui.theme

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoundedLruCacheSpec extends AnyFlatSpec with Matchers:

  /** A key whose `equals` calls are counted, so a lookup's cost is observable without timing it. */
  final private class CountingKey(val id: Int, comparisons: AtomicInteger):
    override def hashCode: Int = id

    override def equals(other: Any): Boolean =
      comparisons.incrementAndGet()
      other match
        case that: CountingKey => that.id == id
        case _                 => false

  "BoundedLruCache.getOrCompute" should "compute a key once and serve the cached value on later hits" in {
    val cache        = BoundedLruCache[String, String](maxEntries = 4)
    val computations = new AtomicInteger(0)
    def lookup(key: String): String =
      cache.getOrCompute(key) {
        computations.incrementAndGet()
        key.toUpperCase
      }

    lookup("a") shouldBe "A"
    lookup("a") shouldBe "A"
    lookup("b") shouldBe "B"
    computations.get() shouldBe 2
  }

  it should "hold at most maxEntries, evicting the least recently used entry" in {
    val cache        = BoundedLruCache[String, String](maxEntries = 2)
    val computations = new AtomicInteger(0)
    def lookup(key: String): String =
      cache.getOrCompute(key) {
        computations.incrementAndGet()
        key.toUpperCase
      }

    lookup("a")
    lookup("b")
    lookup("a")
    lookup("c")
    cache.entryCount shouldBe 2
    computations.get() shouldBe 3

    lookup("a")
    lookup("c")
    computations.get() shouldBe 3

    lookup("b")
    computations.get() shouldBe 4
    cache.entryCount shouldBe 2
  }

  it should "compare the same number of keys per lookup however full the cache is" in {
    def comparisonsPerLookup(filledTo: Int): (Int, Int) =
      val comparisons = new AtomicInteger(0)
      val cache       = BoundedLruCache[CountingKey, Int](maxEntries = filledTo)
      (0 until filledTo).foreach(id => cache.getOrCompute(CountingKey(id, comparisons))(id))
      cache.entryCount shouldBe filledTo

      comparisons.set(0)
      cache.getOrCompute(CountingKey(filledTo / 2, comparisons))(-1) shouldBe filledTo / 2
      val hitComparisons = comparisons.get()

      comparisons.set(0)
      cache.getOrCompute(CountingKey(filledTo + 1, comparisons))(-1) shouldBe -1
      (hitComparisons, comparisons.get())

    val small = comparisonsPerLookup(16)
    val large = comparisonsPerLookup(4096)

    large shouldBe small
    large._1 should be <= 1
  }
