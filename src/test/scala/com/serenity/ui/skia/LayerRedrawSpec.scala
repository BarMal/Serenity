package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicInteger

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

    val redraw: LayerRedraw = LayerRedraw.over(
      repaint = () =>
        val _ = repaints.incrementAndGet()
      ,
      render = () =>
        val _ = renders.incrementAndGet()
      ,
      onEventThread = task => queued.enqueue(task)
    )

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
    requests.redraw.afterCaretsChanged()
    requests.queued.size shouldBe 1
