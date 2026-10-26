package com.serenity.perf

import java.awt.Font
import java.util.concurrent.atomic.AtomicReference

import cats.effect.unsafe.implicits.global
import com.serenity.SkiaFrames
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, PixelRect, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, RendererFramePlanner, ScreenIdentity}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a typing frame presents to the window, through the planner's own repaint region -- the rects `SwingWindow`
  * publishes -- for the keystroke `laptop.render.typing_frame_2x` times (#1839).
  */
class TypingFramePresentSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont    = Font(Font.SERIF, Font.PLAIN, 14)
  private val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val uiMetrics   = CellMetrics.fromFont(uiFont)
  private val viewport    = ViewportSize(1500 / cellMetrics.charWidth, 1000 / cellMetrics.lineHeight)
  private val windowArea  = viewport.width.toLong * cellMetrics.charWidth * viewport.height * cellMetrics.lineHeight

  final private class ScreenFrames:
    private val frame  = SkiaFrames.newFrame(LaptopColdFrames.DeviceWidthPx, LaptopColdFrames.DeviceHeightPx)
    private val caches = RenderCaches.create()
    private val screen = ScreenIdentity(this)

    /** The rects the frame asks the window to present, `None` for the whole window. */
    def present(state: AppState, damage: Damage): Option[List[PixelRect]] =
      val region = new AtomicReference[Option[List[PixelRect]]](None)
      val surface = SkiaFrames.surfaceOver(
        frame,
        cellMetrics,
        codeFont,
        LaptopFrameBenchmarks.LogicalWidthPx,
        LaptopFrameBenchmarks.LogicalHeightPx,
        LaptopFrameBenchmarks.DeviceScale,
        persists = true,
        caches,
        Some(screen)
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
        Some(FrameOutput(screen, region)),
        damage,
        caches = caches
      )
      region.get()

  private def area(rects: List[PixelRect]): Long =
    rects.foldLeft(0L)((sum, rect) => sum + rect.widthPx.toLong * rect.heightPx)

  "The keystroke the typing benchmark times" should "damage buffer rows and chrome only, as a keystroke in a modified buffer does" in {
    val (before, after) = TypingTransition.of(LaptopFrameBenchmarks.proseStateManager()._1)

    Damage.isBufferRowsOrChromeOnly(DamageProducer.forTransition(before, after)) shouldBe true
  }

  it should "present the damaged rows and the status row, not the whole window" in {
    val (before, after) = TypingTransition.of(LaptopFrameBenchmarks.proseStateManager()._1)
    val frames          = ScreenFrames()
    val _               = frames.present(before, Damage.Everything)

    val presented = frames.present(after, DamageProducer.forTransition(before, after))

    withClue(s"presented $presented of a ${windowArea}px window: ") {
      presented.exists(_.nonEmpty) shouldBe true
      presented.map(area).getOrElse(windowArea) should be < (windowArea / 4)
    }
  }
