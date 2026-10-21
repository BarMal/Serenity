package com.serenity.ui.renderer

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BackendCachesSpec extends AnyFlatSpec with Matchers:

  final private class Marker
  final private class Other

  private val markerKey = BackendCacheKey(classOf[Marker])
  private val otherKey  = BackendCacheKey(classOf[Other])

  "BackendCaches" should "hold nothing until a backend asks for its caches" in {
    val owner = BackendCaches()

    owner.holds(markerKey) shouldBe false
  }

  it should "create a backend's caches the first time they are asked for and hand out that one afterwards" in {
    val owner   = BackendCaches()
    val created = AtomicInteger(0)

    val first  = owner.getOrCreate(markerKey) { created.incrementAndGet(); Marker() }
    val second = owner.getOrCreate(markerKey) { created.incrementAndGet(); Marker() }

    second should be theSameInstanceAs first
    created.get shouldBe 1
    owner.holds(markerKey) shouldBe true
  }

  it should "keep the caches of different backends apart" in {
    val owner = BackendCaches()

    val marker = owner.getOrCreate(markerKey)(Marker())
    val other  = owner.getOrCreate(otherKey)(Other())

    (marker: AnyRef) should not be theSameInstanceAs(other)
    owner.holds(otherKey) shouldBe true
  }

  it should "give two owners caches of their own" in {
    val one = BackendCaches()
    val two = BackendCaches()

    one.getOrCreate(markerKey)(Marker()) should not be theSameInstanceAs(two.getOrCreate(markerKey)(Marker()))
  }

  it should "create the caches once however many threads ask for them at the same moment" in {
    val owner   = BackendCaches()
    val created = AtomicInteger(0)
    val threads = 8
    val start   = CountDownLatch(1)
    val pool    = Executors.newFixedThreadPool(threads)
    try
      val asked = (1 to threads).map { _ =>
        pool.submit(() =>
          start.await()
          owner.getOrCreate(markerKey) { created.incrementAndGet(); Marker() }
        )
      }
      start.countDown()
      val handedOut = asked.map(_.get(10, TimeUnit.SECONDS))

      created.get shouldBe 1
      handedOut.distinct should have size 1
    finally
      val _ = pool.shutdownNow()
  }
