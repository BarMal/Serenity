package com.serenity.perf

import java.awt.GraphicsEnvironment

import cats.effect.unsafe.implicits.global
import com.serenity.SkiaPixels
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{Event, InsertChar, PageDown, PageUp, ScrollDown, ScrollUp}
import com.serenity.rope.Balance
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.{AppState, Damage}
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.skia.SkiaRasterSurfaceFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A window that scrolls, types and pages through the real renderer shows, after every frame, exactly what the Skia
  * surface holds. The image it shows follows the surface by moving rows instead of copying them from Skia, so a pixel
  * the frame painted and the record of its paint missed would be left stale; this is where that is caught. Two pooled
  * images alternate, as the window's pool hands them out.
  */
class SkiaScrollAwareFramesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val codeFont = LaptopFrameBenchmarks.codeFont
  private val metrics  = CellMetrics.fromFont(codeFont)
  private val width    = 900
  private val height   = 600
  private val grid     = ViewportSize(width / metrics.charWidth, height / metrics.lineHeight)

  private val script: List[(String, Event)] =
    List.fill(6)("type" -> InsertChar('q')) ++
      List.fill(8)("scroll down" -> ScrollDown(3)) ++
      List.fill(5)("scroll up" -> ScrollUp(2)) ++
      List("page down" -> PageDown, "page down" -> PageDown, "page up" -> PageUp) ++
      List.fill(3)("type" -> InsertChar('z')) ++
      List.fill(6)("scroll down" -> ScrollDown(1)) ++
      List.fill(4)("scroll down" -> ScrollDown(7))

  private val plain = AppConfig.conventional
  private val chrome =
    AppConfig.conventional.withScrollbar(true).withLineNumbers(true).withPaneHeaders(true)

  for
    scale           <- List(1.0, 2.0)
    (label, config) <- List("with the default chrome" -> plain, "with a scrollbar, line numbers and a header" -> chrome)
  do
    s"A Skia window at ${scale}x $label" should "show the surface exactly after typing, scrolling and paging" in {
      assume(scale == 1.0 || !GraphicsEnvironment.isHeadless, "A fractional canvas scale needs a display")
      val (stateManager, _) = LaptopFrameBenchmarks.proseStateManager(paragraphs = 400, config = config, grid = grid)
      val window            = PresentBenchmarks.Window(SkiaRasterSurfaceFactory, width, height, scale, grid)
      def state: AppState   = stateManager.getCurrentState.unsafeRunSync()
      val first             = window.render(state, Damage.Everything)
      pixelsOf(window, first) shouldBe surfacePixels(window)
      script.zipWithIndex.foreach {
        case ((label, event), step) =>
          val before = state
          stateManager.applyEvent(event).unsafeRunSync()
          val after = state
          val shown = window.render(after, DamageProducer.forTransition(before, after))
          withClue(s"step ${step + 1} ($label): ")(pixelsOf(window, shown) shouldBe surfacePixels(window))
      }
    }

  private def surfacePixels(window: PresentBenchmarks.Window): Array[Int] =
    val surface = SkiaRasterSurfaceFactory.retainedSurface(window.backendCaches).getOrElse(fail("no surface"))
    SkiaPixels.read(surface).map(_ & 0xffffff)

  private def pixelsOf(window: PresentBenchmarks.Window, image: java.awt.image.BufferedImage): Array[Int] =
    image.getRGB(0, 0, image.getWidth, image.getHeight, null, 0, image.getWidth).map(_ & 0xffffff)
