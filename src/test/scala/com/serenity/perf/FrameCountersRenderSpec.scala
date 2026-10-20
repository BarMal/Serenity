package com.serenity.perf

import java.awt.Font
import java.util.concurrent.atomic.AtomicReference

import cats.effect.unsafe.implicits.global
import com.serenity.diagnostics.{FrameCounter, FrameCounts}
import com.serenity.keystroke.events.{Event, InsertChar, ScrollDown}
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, Java2DRenderSurface, RendererFramePlanner, ScreenIdentity}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the frames `laptop.render.typing_frame_2x` and `scroll_frame_2x` time actually draw, counted: a damaged frame
  * over a persisting image redraws the rows its damage names and keeps the rest, and says so in its frame counters.
  */
class FrameCountersRenderSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont    = Font(Font.SERIF, Font.PLAIN, 14)
  private val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val uiMetrics   = CellMetrics.fromFont(uiFont)
  private val viewport    = ViewportSize(1500 / cellMetrics.charWidth, 1000 / cellMetrics.lineHeight)
  private val wholeGridPx = viewport.width.toLong * cellMetrics.charWidth * viewport.height * cellMetrics.lineHeight

  final private class CountedFrames:
    private val image  = LaptopFrameBenchmarks.frameImage()
    private val caches = RenderCaches.create()
    private val screen = ScreenIdentity(this)
    caches.frameCounters.setEnabled(true)

    def render(state: AppState, damage: Damage): FrameCounts =
      val surface = new Java2DRenderSurface(
        image,
        cellMetrics,
        codeFont,
        _ => (),
        logicalWidthPx = LaptopFrameBenchmarks.LogicalWidthPx,
        logicalHeightPx = LaptopFrameBenchmarks.LogicalHeightPx,
        deviceScaleX = LaptopFrameBenchmarks.DeviceScale,
        deviceScaleY = LaptopFrameBenchmarks.DeviceScale,
        contentPersists = true,
        layerCacheOwnerOverride = Some(screen),
        textCaches = caches.surfaceText
      )
      val _ = RendererFramePlanner.renderFrame(
        state,
        cursorVisible = false,
        surface,
        viewport,
        caches.authoritativeScene.forState(state, viewport, codeFont, textFont),
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics,
        None,
        Some(FrameOutput(screen, new AtomicReference(None))),
        damage,
        caches = caches
      )
      caches.frameCounters.lastFrame

  private def transition(event: Event): (AppState, AppState) =
    val (stateManager, _) = LaptopFrameBenchmarks.proseStateManager()
    val before            = stateManager.getCurrentState.unsafeRunSync()
    stateManager.applyEvent(event).unsafeRunSync()
    (before, stateManager.getCurrentState.unsafeRunSync())

  private def rows(counts: FrameCounts): Long = counts(FrameCounter.RowsDrawn) + counts(FrameCounter.RowsPreserved)

  private def describe(counts: FrameCounts): String =
    FrameCounter.values.map(counter => s"${counter.label}=${counts(counter)}").mkString(" ")

  "A first frame" should "draw every row, keep none, and present the whole window" in {
    val (before, _) = transition(InsertChar('x'))

    val counts = CountedFrames().render(before, Damage.Everything)

    withClue(describe(counts) + ": ") {
      counts(FrameCounter.RowsDrawn) should be > 20L
      counts(FrameCounter.RowsPreserved) shouldBe 0L
      counts(FrameCounter.ChromePainted) shouldBe 1L
      counts(FrameCounter.PresentAreaPx) shouldBe wholeGridPx
      counts(FrameCounter.RunsDrawn) should be >= counts(FrameCounter.RowsDrawn)
      counts(FrameCounter.TuiBytes) shouldBe 0L
    }
  }

  "A typing frame" should "draw the typed row and its neighbours and keep the rest" in {
    val (before, after) = transition(InsertChar('x'))
    val frames          = CountedFrames()
    val first           = frames.render(before, Damage.Everything)

    val typed = frames.render(after, DamageProducer.forTransition(before, after))

    withClue(s"first ${describe(first)}; typed ${describe(typed)}: ") {
      typed(FrameCounter.RowsDrawn) should be >= 1L
      typed(FrameCounter.RowsDrawn) should be <= LaptopFrameBenchmarks.MaxTypingRowsDrawn
      rows(typed) shouldBe rows(first)
      typed(FrameCounter.RowsPreserved) should be > (rows(first) / 2)
      typed(FrameCounter.RunsDrawn) should be > 0L
      typed(FrameCounter.RunsDrawn) should be < first(FrameCounter.RunsDrawn) / 4
      typed(FrameCounter.PresentAreaPx) should be > 0L
      typed(FrameCounter.PresentAreaPx) should be <= wholeGridPx
    }
  }

  "A wheel-scroll frame" should "draw only the rows scrolled into view and keep the rest" in {
    val (before, after) = transition(ScrollDown(3))
    val frames          = CountedFrames()
    val first           = frames.render(before, Damage.Everything)

    val scrolled = frames.render(after, DamageProducer.forTransition(before, after))

    withClue(s"first ${describe(first)}; scrolled ${describe(scrolled)}: ") {
      scrolled(FrameCounter.RowsDrawn) should be >= 1L
      scrolled(FrameCounter.RowsDrawn) should be <= LaptopFrameBenchmarks.MaxScrollRowsDrawn
      rows(scrolled) shouldBe rows(first)
      scrolled(FrameCounter.RowsPreserved) should be > scrolled(FrameCounter.RowsDrawn)
    }
  }

  "A frame nothing damaged" should "draw no row, no run and present nothing" in {
    val (before, _) = transition(InsertChar('x'))
    val frames      = CountedFrames()
    val first       = frames.render(before, Damage.Everything)

    val idle = frames.render(before, Damage.Nothing)

    withClue(s"first ${describe(first)}; idle ${describe(idle)}: ") {
      idle(FrameCounter.RowsDrawn) shouldBe 0L
      idle(FrameCounter.RowsPreserved) shouldBe rows(first)
      idle(FrameCounter.RunsDrawn) shouldBe 0L
      idle(FrameCounter.PresentAreaPx) shouldBe 0L
      idle(FrameCounter.ChromePainted) shouldBe 0L
    }
  }

  "Frame counters left off" should "record nothing while frames are drawn" in {
    val (before, _) = transition(InsertChar('x'))
    val caches      = RenderCaches.create()
    val surface = new Java2DRenderSurface(
      LaptopFrameBenchmarks.frameImage(),
      cellMetrics,
      codeFont,
      _ => (),
      logicalWidthPx = LaptopFrameBenchmarks.LogicalWidthPx,
      logicalHeightPx = LaptopFrameBenchmarks.LogicalHeightPx,
      deviceScaleX = LaptopFrameBenchmarks.DeviceScale,
      deviceScaleY = LaptopFrameBenchmarks.DeviceScale,
      textCaches = caches.surfaceText
    )

    val _ = RendererFramePlanner.renderFrame(
      before,
      cursorVisible = false,
      surface,
      viewport,
      caches.authoritativeScene.forState(before, viewport, codeFont, textFont),
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      None,
      None,
      Damage.Everything,
      caches = caches
    )

    caches.frameCounters.lastFrame.values.values.sum shouldBe 0L
  }
