package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** How a hosted layer is asked to draw, without a layer or a display: the requests are counted, not drawn. */
class LayerRedrawSpec extends AnyFlatSpec with Matchers:

  private def counted(): (LayerRedraw, AtomicInteger) =
    val repaints = new AtomicInteger(0)
    (
      LayerRedraw.over(() =>
        val _ = repaints.incrementAndGet()
      ),
      repaints
    )

  "A layer's redraw" should "repaint once for each published frame" in:
    val (redraw, repaints) = counted()
    redraw.afterPublish()
    repaints.get shouldBe 1

  it should "repaint once for each change of carets" in:
    val (redraw, repaints) = counted()
    redraw.afterCaretsChanged()
    repaints.get shouldBe 1

  it should "ask for nothing until it is told of a publish or a change of carets" in:
    val (_, repaints) = counted()
    repaints.get shouldBe 0
