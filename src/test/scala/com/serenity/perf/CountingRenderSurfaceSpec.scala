package com.serenity.perf

import cats.effect.unsafe.implicits.global
import com.serenity.MockRenderSurface
import com.serenity.rope.Balance
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.FrameSurfaceFactory
import com.serenity.ui.skia.SkiaRasterSurfaceFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CountingRenderSurfaceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val widthPx  = 900
  private val heightPx = 600
  private val metrics  = PresentBenchmarks.Fonts.default.metrics
  private val grid     = ViewportSize(widthPx / metrics.charWidth, heightPx / metrics.lineHeight)

  private def typingPair() =
    val (prose, _) = LaptopFrameBenchmarks.proseStateManager(grid = grid)
    val states     = PresentBenchmarks.ProseStates(prose)
    Vector(states.beforeTyping, states.afterTyping)

  private def typingFrames(factory: FrameSurfaceFactory, frames: Int): Vector[Array[Int]] =
    val cycle = PresentBenchmarks.Cycling(
      PresentBenchmarks.Window(factory, widthPx, heightPx, 1.0, grid),
      typingPair()
    )
    Vector.fill(frames) {
      val image = cycle.next()
      image.getRGB(0, 0, image.getWidth, image.getHeight, null, 0, image.getWidth)
    }

  "A counting surface" should "count a scripted sequence and forward every call" in {
    val mock     = new MockRenderSurface(20, 5)
    val counts   = DrawCounts()
    val counting = CountingRenderSurface(mock, counts)
    counting.putString(0, 0, "ab")
    counting.putString(1, 1, "cd")
    counting.fillRect(0, 0, 3, 2, '#')
    counting.panelOutlines.foreach(_.withRectClip(1, 1, 4, 2)(counting.putString(2, 2, "in")))

    val seen = counts.snapshot
    seen(DrawKind.PutString) shouldBe 3L
    seen(DrawKind.FillRect) shouldBe 1L
    seen(DrawKind.ClipPush) shouldBe 1L
    seen(DrawKind.ClipPop) shouldBe 1L
    counts.total shouldBe 6L
    mock.putStringCalls.map(_.s) shouldBe List("ab", "cd", "in")
    mock.fillRectCalls should have size 1
    mock.rectClipCalls should have size 1
  }

  it should "count the clip pop when the body throws" in {
    val mock     = new MockRenderSurface(20, 5)
    val counts   = DrawCounts()
    val counting = CountingRenderSurface(mock, counts)
    val outcome =
      scala.util.Try(counting.panelOutlines.foreach(_.withRectClip(0, 0, 2, 2)(throw IllegalStateException("body"))))
    outcome.isFailure shouldBe true
    counts.snapshot(DrawKind.ClipPush) shouldBe 1L
    counts.snapshot(DrawKind.ClipPop) shouldBe 1L
  }

  it should "hand back itself for the capabilities the inner surface returns itself for" in {
    val counting = CountingRenderSurface(new MockRenderSurface(20, 5), DrawCounts())
    (counting.text eq counting) shouldBe true
    (counting.pixels eq counting) shouldBe true
    counting.effects.exists(_ eq counting) shouldBe true
    counting.panelOutlines.exists(_ eq counting) shouldBe true
  }

  it should "publish pixel-identical images to the surface it wraps" in {
    val plain   = typingFrames(SkiaRasterSurfaceFactory, 6)
    val counted = typingFrames(CountingFactory(SkiaRasterSurfaceFactory, DrawCounts()), 6)
    plain.zip(counted).foreach((expected, actual) => java.util.Arrays.equals(expected, actual) shouldBe true)
  }

  it should "count the draws of a typing frame until reset" in {
    val counts = DrawCounts()
    val cycle = PresentBenchmarks.Cycling(
      PresentBenchmarks.Window(CountingFactory(SkiaRasterSurfaceFactory, counts), widthPx, heightPx, 1.0, grid),
      typingPair()
    )
    counts.reset()
    val _ = cycle.next()
    counts.total should be > 0L
    counts.snapshot.values.sum shouldBe counts.total
    counts.reset()
    counts.total shouldBe 0L
    counts.snapshot.values.forall(_ == 0L) shouldBe true
  }

  it should "count a batch of rects once and hand it on as a batch" in {
    final class BatchRecordingMock extends MockRenderSurface(20, 5):
      private val received           = new java.util.concurrent.atomic.AtomicReference(List.empty[IArray[Int]])
      def batches: List[IArray[Int]] = received.get()
      override def fillPixelRects(rects: IArray[Int], color: com.serenity.ui.color.RenderColor): Unit =
        val _ = received.updateAndGet(_ :+ rects)
        super.fillPixelRects(rects, color)

    val mock     = new BatchRecordingMock
    val counts   = DrawCounts()
    val counting = CountingRenderSurface(mock, counts)
    val rects    = IArray(0, 0, 2, 1, 2, 1, 2, 1, 4, 0, 2, 1)
    counting.pixels.fillPixelRects(rects, com.serenity.ui.color.RenderColor.White)
    counts.snapshot(DrawKind.FillPixelRects) shouldBe 1L
    counts.snapshot(DrawKind.FillPixelRect) shouldBe 0L
    mock.batches.map(_.toList) shouldBe List(rects.toList)
  }
