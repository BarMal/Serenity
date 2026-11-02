package com.serenity.state.manager

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}

import scala.jdk.CollectionConverters.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The threads that measure the rows a frame left unmeasured (#2483): how many there are, and that they run at once. */
class ScrollCountExecutorSpec extends AnyFlatSpec with Matchers:

  "The default number of count threads" should "be the cores left after the reserved ones, at least one and at most four" in {
    val reserved = ScrollCountExecutor.ReservedCores
    ScrollCountExecutor.defaultThreads(cores = 1) shouldBe 1
    ScrollCountExecutor.defaultThreads(cores = 64) shouldBe ScrollCountExecutor.MaxThreads
    ScrollCountExecutor.defaultThreads(cores = 3) shouldBe (3 - reserved).max(1).min(ScrollCountExecutor.MaxThreads)
    ScrollCountExecutor.defaultThreads(cores = 6) shouldBe (6 - reserved).max(1).min(ScrollCountExecutor.MaxThreads)
  }

  "A configured number of count threads" should "win over the default, within one and the cap" in {
    ScrollCountExecutor.threadsFor(cores = 8, configured = Some("2")) shouldBe 2
    ScrollCountExecutor.threadsFor(cores = 2, configured = Some("4")) shouldBe 4
    ScrollCountExecutor.threadsFor(cores = 8, configured = Some("0")) shouldBe 1
    ScrollCountExecutor.threadsFor(cores = 8, configured = Some("99")) shouldBe ScrollCountExecutor.MaxThreads
  }

  it should "be ignored when it is not a number" in {
    ScrollCountExecutor.threadsFor(cores = 8, configured = Some("many")) shouldBe ScrollCountExecutor.defaultThreads(8)
    ScrollCountExecutor.threadsFor(cores = 8, configured = Some("")) shouldBe ScrollCountExecutor.defaultThreads(8)
    ScrollCountExecutor.threadsFor(cores = 8, configured = None) shouldBe ScrollCountExecutor.defaultThreads(8)
  }

  "A count pool" should "run as many counts at once as it has threads" in {
    val pool    = ScrollCountExecutor.pool(threads = 3)
    val started = new CountDownLatch(3)
    val release = new CountDownLatch(1)
    (1 to 3).foreach(_ =>
      pool.execute { () =>
        started.countDown()
        val _ = release.await(30, TimeUnit.SECONDS)
      }
    )
    val allAtOnce = started.await(30, TimeUnit.SECONDS)
    release.countDown()
    allAtOnce shouldBe true
  }

  it should "run no more counts at once than it has threads" in {
    val pool     = ScrollCountExecutor.pool(threads = 2)
    val running  = new java.util.concurrent.atomic.AtomicInteger(0)
    val highest  = new java.util.concurrent.atomic.AtomicInteger(0)
    val firstTwo = new CountDownLatch(2)
    val aThird   = new CountDownLatch(3)
    val release  = new CountDownLatch(1)
    val done     = new CountDownLatch(8)
    (1 to 8).foreach(_ =>
      pool.execute { () =>
        highest.accumulateAndGet(running.incrementAndGet(), math.max): Unit
        firstTwo.countDown()
        aThird.countDown()
        val _ = release.await(30, TimeUnit.SECONDS)
        running.decrementAndGet(): Unit
        done.countDown()
      }
    )
    firstTwo.await(30, TimeUnit.SECONDS) shouldBe true
    val thirdStartedWhileTwoHeld = aThird.await(300, TimeUnit.MILLISECONDS)
    release.countDown()
    done.await(30, TimeUnit.SECONDS) shouldBe true
    thirdStartedWhileTwoHeld shouldBe false
    highest.get should be <= 2
  }

  it should "start queued counts in the order they were handed in" in {
    val pool    = ScrollCountExecutor.pool(threads = 1)
    val started = new ConcurrentLinkedQueue[Int]()
    val done    = new CountDownLatch(6)
    (1 to 6).foreach(n =>
      pool.execute { () =>
        started.add(n): Unit
        done.countDown()
      }
    )
    done.await(30, TimeUnit.SECONDS) shouldBe true
    started.asScala.toList shouldBe (1 to 6).toList
  }

  it should "use daemon threads named for the count, so none keeps the process alive" in {
    val pool = ScrollCountExecutor.pool(threads = 2)
    val seen = new ConcurrentLinkedQueue[(String, Boolean)]()
    val done = new CountDownLatch(1)
    pool.execute { () =>
      seen.add((Thread.currentThread.getName, Thread.currentThread.isDaemon)): Unit
      done.countDown()
    }
    done.await(30, TimeUnit.SECONDS) shouldBe true
    val (name, daemon) = seen.asScala.head
    name should startWith("serenity-scroll-count")
    daemon shouldBe true
  }
