package com.serenity.perf

import java.awt.Font
import java.util.concurrent.Executor

import scala.annotation.tailrec
import scala.collection.mutable

import cats.effect.unsafe.implicits.global
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.benchmark.BenchmarkCaches
import com.serenity.ui.skia.SkiaTextCaches
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The render benchmark builds its scenes as the app does (#2465, M1): a pane's whole-document row count is measured
  * after the frame, and the font sets are made once, not again for every cold frame.
  */
class BenchmarkCachesSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  final private class ByHand extends Executor:
    private val queued                         = mutable.Queue.empty[Runnable]
    override def execute(task: Runnable): Unit = queued.enqueue(task)

    @tailrec final def runAll(): Unit =
      if queued.nonEmpty then
        queued.dequeue().run()
        runAll()

  private def longDocument: AppState =
    val (manager, _) =
      LaptopFrameBenchmarks.stateManagerHolding(
        BenchmarkFixtures.loremIpsumProse(40),
        baseConfig.withScrollbar(true),
        grid = viewport
      )
    manager.getCurrentState.unsafeRunSync()

  private def hasBar(caches: RenderCaches, state: AppState): Boolean =
    caches.authoritativeScene.forState(state, viewport, codeFont, textFont).paneScrolls.nonEmpty

  "The shared caches" should "leave a long document's scrollbar out of the first scene, as the app does" in {
    val state = longDocument
    val bench = new BenchmarkCaches(new ByHand)

    hasBar(bench.shared(), state) shouldBe false
  }

  it should "show the bar once the rows have been counted" in {
    val state    = longDocument
    val executor = new ByHand
    val caches   = new BenchmarkCaches(executor).shared()
    hasBar(caches, state) shouldBe false

    executor.runAll()

    hasBar(caches, state) shouldBe true
  }

  "A cold frame's caches" should "count rows late too" in {
    val state = longDocument
    val bench = new BenchmarkCaches(new ByHand)

    hasBar(bench.cold(bench.shared()), state) shouldBe false
  }

  it should "let a newer cold frame's count replace an older one, as a newer scene does in the app" in {
    val state    = longDocument
    val executor = new ByHand
    val bench    = new BenchmarkCaches(executor)
    val shared   = bench.shared()
    val first    = bench.cold(shared)
    val second   = bench.cold(shared)
    hasBar(first, state) shouldBe false
    hasBar(second, state) shouldBe false

    executor.runAll()

    hasBar(second, state) shouldBe true
    hasBar(first, state) shouldBe false
  }

  it should "keep the Java2D font cache and start everything else empty" in {
    val bench  = new BenchmarkCaches(new ByHand)
    val shared = bench.shared()
    val cold   = bench.cold(shared)

    cold.styledFonts should be theSameInstanceAs shared.styledFonts
    cold.wrappedLines should not be theSameInstanceAs(shared.wrappedLines)
    cold.shapedText should not be theSameInstanceAs(shared.shapedText)
    cold.labelSprites should not be theSameInstanceAs(shared.labelSprites)
    cold.authoritativeScene should not be theSameInstanceAs(shared.authoritativeScene)
  }

  it should "keep Skia's fonts and start its shaped text and images empty" in {
    val bench      = new BenchmarkCaches(new ByHand)
    val shared     = bench.shared()
    val sharedSkia = SkiaTextCaches.of(shared.backendCaches)
    val coldSkia   = SkiaTextCaches.of(bench.cold(shared).backendCaches)

    coldSkia.fonts should be theSameInstanceAs sharedSkia.fonts
    coldSkia.shapedText should not be theSameInstanceAs(sharedSkia.shapedText)
    coldSkia.images should not be theSameInstanceAs(sharedSkia.images)
  }

  it should "shape text again, with the faces it already has" in {
    val bench      = new BenchmarkCaches(new ByHand)
    val shared     = bench.shared()
    val sharedSkia = SkiaTextCaches.of(shared.backendCaches)
    val face       = sharedSkia.fonts.faceFor(new Font(Font.SERIF, Font.PLAIN, 14)).font
    val _          = sharedSkia.shapedText.stretchOf("a cold frame shapes this", face)
    sharedSkia.shapedText.shapings shouldBe 1L

    val coldSkia = SkiaTextCaches.of(bench.cold(shared).backendCaches)
    coldSkia.shapedText.shapings shouldBe 0L
    val _ = coldSkia.shapedText.stretchOf("a cold frame shapes this", face)

    coldSkia.shapedText.shapings shouldBe 1L
  }

  it should "report no faces built, since they were made before the cold frame" in {
    val bench      = new BenchmarkCaches(new ByHand)
    val shared     = bench.shared()
    val sharedSkia = SkiaTextCaches.of(shared.backendCaches)
    val _          = sharedSkia.fonts.faceFor(new Font(Font.SERIF, Font.PLAIN, 14))
    SkiaTextCaches.warmupOf(shared.backendCaches).exists(_.faces >= 1) shouldBe true

    val cold = bench.cold(shared)

    SkiaTextCaches.warmupOf(cold.backendCaches).map(_.faces) shouldBe Some(0)
  }

  it should "carry no Skia caches from a set that never drew with Skia" in {
    val bench = new BenchmarkCaches(new ByHand)

    SkiaTextCaches.heldBy(bench.cold(bench.shared()).backendCaches) shouldBe false
  }

  "Both kinds of caches" should "count what each frame drew, so a scenario can report its shapings" in {
    val bench  = new BenchmarkCaches(new ByHand)
    val shared = bench.shared()

    shared.frameCounters.isEnabled shouldBe true
    bench.cold(shared).frameCounters.isEnabled shouldBe true
  }
