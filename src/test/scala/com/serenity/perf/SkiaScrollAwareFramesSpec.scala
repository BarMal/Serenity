package com.serenity.perf

import java.awt.{Font, GraphicsEnvironment}

import cats.effect.unsafe.implicits.global
import com.serenity.SkiaPixels
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{Event, InsertChar, PageDown, PageUp, ScrollDown, ScrollUp}
import com.serenity.rope.Balance
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.{AppState, Damage}
import com.serenity.ui.layout.ViewportSize
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

  private val width = 900

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

  /** Fonts and window heights that leave a partial line at the bottom, where paint lands outside the reported area. */
  private val codeSizes = List(10, 12, 15)
  private val heights   = List(600, 613)

  for
    scale           <- List(1.0, 2.0)
    (label, config) <- List("with the default chrome" -> plain, "with a scrollbar, line numbers and a header" -> chrome)
    codeSize        <- codeSizes
    height          <- heights
  do
    s"A Skia window at ${scale}x $label in a $codeSize pt code font, ${height}px high" should "show the surface exactly after typing, scrolling and paging" in {
      assume(scale == 1.0 || !GraphicsEnvironment.isHeadless, "A fractional canvas scale needs a display")
      val fonts =
        PresentBenchmarks.Fonts(Font(Font.MONOSPACED, Font.PLAIN, codeSize), PresentBenchmarks.Fonts.default.text)
      val grid              = ViewportSize(width / fonts.metrics.charWidth, height / fonts.metrics.lineHeight)
      val (stateManager, _) = LaptopFrameBenchmarks.proseStateManager(paragraphs = 400, config = config, grid = grid)
      val window            = PresentBenchmarks.Window(SkiaRasterSurfaceFactory, width, height, scale, grid, fonts)
      def state: AppState   = stateManager.getCurrentState.unsafeRunSync()
      val first             = window.render(state, Damage.Everything)
      sameAs(pixelsOf(window, first), surfacePixels(window), first.getWidth)
      script.zipWithIndex.foreach {
        case ((label, event), step) =>
          val before = state
          stateManager.applyEvent(event).unsafeRunSync()
          val after = state
          val shown = window.render(after, DamageProducer.forTransition(before, after))
          withClue(s"step ${step + 1} ($label): ")(
            sameAs(pixelsOf(window, shown), surfacePixels(window), shown.getWidth)
          )
      }
    }

  private def surfacePixels(window: PresentBenchmarks.Window): Array[Int] =
    val surface = SkiaRasterSurfaceFactory.retainedSurface(window.backendCaches).getOrElse(fail("no surface"))
    SkiaPixels.read(surface).map(_ & 0xffffff)

  private def pixelsOf(window: PresentBenchmarks.Window, image: java.awt.image.BufferedImage): Array[Int] =
    image.getRGB(0, 0, image.getWidth, image.getHeight, null, 0, image.getWidth).map(_ & 0xffffff)

  /** Equal, or a failure that says where the two differ rather than printing both images. */
  private def sameAs(shown: Array[Int], surface: Array[Int], widthPx: Int): Unit =
    val differing = shown.indices.filter(i => shown(i) != surface(i))
    if differing.nonEmpty then
      val xs = differing.map(_ % widthPx)
      val ys = differing.map(_ / widthPx)
      val first = differing
        .take(6)
        .map(i => s"(${i % widthPx},${i / widthPx}) shown ${shown(i).toHexString} surface ${surface(i).toHexString}")
      fail(
        s"${differing.size} pixels differ, x ${xs.min}-${xs.max}, y ${ys.min}-${ys.max}; first: ${first.mkString("; ")}"
      )
