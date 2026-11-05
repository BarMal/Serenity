package com.serenity.ui.skia

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import scala.annotation.tailrec
import scala.collection.mutable

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** How a hosted layer is asked to draw, without a layer or a display: the requests are counted and the event thread is
  * a queue the spec runs by hand.
  */
class LayerRedrawSpec extends AnyFlatSpec with Matchers:

  final private class Requests:
    val repaints = new AtomicInteger(0)
    val renders  = new AtomicInteger(0)
    val queued   = mutable.Queue.empty[() => Unit]
    val now      = new AtomicLong(0L)

    val redraw: LayerRedraw = LayerRedraw.over(
      repaint = () =>
        val _ = repaints.incrementAndGet()
      ,
      render = () =>
        val _ = renders.incrementAndGet()
      ,
      onEventThread = task => queued.enqueue(task),
      nanoTime = () => now.get()
    )

    /** The layer begins a draw, which answers everything asked for until then. */
    def draw(): Unit = redraw.drawStarted()

    def advanceMs(ms: Long): Unit = now.addAndGet(ms * 1_000_000L): Unit

    @tailrec
    final def runEventThread(): Unit =
      if queued.nonEmpty then
        queued.dequeue()()
        runEventThread()

  "A layer's redraw" should "repaint once for each published frame, and never render directly" in:
    val requests = new Requests
    requests.redraw.afterPublish()
    (requests.repaints.get, requests.renders.get, requests.queued.size) shouldBe (1, 0, 0)

  it should "ask for nothing until it is told of a publish or a change of carets" in:
    val requests = new Requests
    (requests.repaints.get, requests.renders.get, requests.queued.size) shouldBe (0, 0, 0)

  it should "render for a change of carets only on the event thread" in:
    val requests = new Requests
    requests.redraw.afterCaretsChanged()
    requests.renders.get shouldBe 0
    requests.runEventThread()
    requests.renders.get shouldBe 1

  it should "not repaint for a change of carets, which goes through the repaint queue and a paint pass" in:
    val requests = new Requests
    requests.redraw.afterCaretsChanged()
    requests.runEventThread()
    requests.repaints.get shouldBe 0

  it should "queue one render for a burst of caret changes, and queue again once it has run" in:
    val requests = new Requests
    (1 to 5).foreach(_ => requests.redraw.afterCaretsChanged())
    requests.queued.size shouldBe 1
    requests.runEventThread()
    requests.renders.get shouldBe 1
    requests.draw()
    requests.redraw.afterCaretsChanged()
    requests.queued.size shouldBe 1

  it should "ask once for a published frame whose carets follow before the layer has drawn" in:
    val requests = new Requests
    requests.redraw.afterPublish()
    requests.redraw.afterCaretsChanged()
    requests.runEventThread()
    (requests.repaints.get, requests.renders.get) shouldBe (1, 0)

  it should "ask once for carets that change and a frame that is published before the layer has drawn" in:
    val requests = new Requests
    requests.redraw.afterCaretsChanged()
    requests.redraw.afterPublish()
    requests.runEventThread()
    (requests.repaints.get, requests.renders.get) shouldBe (0, 1)

  it should "ask once for several published frames before the layer has drawn" in:
    val requests = new Requests
    (1 to 4).foreach(_ => requests.redraw.afterPublish())
    requests.repaints.get shouldBe 1

  it should "ask again for whatever changes once a draw has started" in:
    val requests = new Requests
    requests.redraw.afterPublish()
    requests.draw()
    requests.redraw.afterCaretsChanged()
    requests.runEventThread()
    (requests.repaints.get, requests.renders.get) shouldBe (1, 1)
    requests.draw()
    requests.redraw.afterPublish()
    requests.repaints.get shouldBe 2

  it should "ask once per draw for carets that blink" in:
    val requests = new Requests
    (1 to 3).foreach { _ =>
      requests.redraw.afterCaretsChanged()
      requests.runEventThread()
      requests.draw()
    }
    requests.renders.get shouldBe 3

  it should "treat a draw that starts with nothing asked as harmless" in:
    val requests = new Requests
    requests.draw()
    requests.draw()
    requests.redraw.afterPublish()
    requests.repaints.get shouldBe 1

  it should "ask again after a quarter of a second without a draw, so a request that was lost cannot silence the rest" in:
    val requests = new Requests
    requests.redraw.afterPublish()
    requests.advanceMs(249)
    requests.redraw.afterPublish()
    requests.repaints.get shouldBe 1
    requests.advanceMs(2)
    requests.redraw.afterPublish()
    requests.repaints.get shouldBe 2
    requests.advanceMs(100)
    requests.redraw.afterCaretsChanged()
    requests.queued.size shouldBe 0
