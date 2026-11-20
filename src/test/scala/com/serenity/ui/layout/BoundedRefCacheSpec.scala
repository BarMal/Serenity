package com.serenity.ui.layout

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoundedRefCacheSpec extends AnyFlatSpec with Matchers:

  private val comparisons = new AtomicLong(0L)

  /** A key whose equality costs, as an AWT font's does, with a hash of its own so only a real match is compared. */
  final private class Key(val id: Int):
    override def hashCode: Int = id

    override def equals(other: Any): Boolean =
      val _ = comparisons.incrementAndGet()
      other match
        case that: Key => that.id == id
        case _         => false

  private def cache(capacity: Int) = new BoundedRefCache[Key, Int](new AtomicInteger(capacity))

  "A bounded cache" should "keep the most recently written entries and drop the oldest" in {
    val held = cache(3)
    (1 to 5).foreach(id => held.put(Key(id), id))

    held.snapshot.keySet.map(_.id) shouldBe Set(3, 4, 5)
    held.get(Key(1)) shouldBe None
    held.get(Key(5)) shouldBe Some(5)
  }

  it should "count writing an entry again as the newest write" in {
    val held = cache(3)
    (1 to 3).foreach(id => held.put(Key(id), id))
    held.put(Key(1), 10)
    held.put(Key(4), 4)

    held.snapshot.keySet.map(_.id) shouldBe Set(1, 3, 4)
    held.get(Key(1)) shouldBe Some(10)
  }

  it should "forget a removed entry, and keep the order of the others" in {
    val held = cache(3)
    (1 to 3).foreach(id => held.put(Key(id), id))
    held.remove(Key(2))
    held.put(Key(4), 4)
    held.put(Key(5), 5)

    held.snapshot.keySet.map(_.id) shouldBe Set(3, 4, 5)
  }

  it should "keep the order of the entries it rewrites in bulk, and give new ones the newest places" in {
    val held = cache(3)
    (1 to 3).foreach(id => held.put(Key(id), id))
    held.replaceAll(entries => entries.map((key, value) => key -> (value * 10)) + (Key(4) -> 4))

    held.snapshot.keySet.map(_.id) shouldBe Set(2, 3, 4)
    held.get(Key(2)) shouldBe Some(20)
  }

  it should "compare a key with only a few others on a write, however many it holds" in {
    val capacity = 512
    val held     = cache(capacity)
    (0 until capacity).foreach(id => held.put(Key(id), id))
    comparisons.set(0L)

    (capacity until capacity * 2).foreach(id => held.put(Key(id), id))

    comparisons.get should be < (capacity * 4).toLong
  }
