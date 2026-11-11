package com.serenity.ui.skia

import java.awt.Font
import java.awt.image.BufferedImage
import javax.swing.JPanel

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.FrameSurfaceRequest
import com.serenity.ui.terminal.{SwingCanvasGeometry, SwingPresentationSite}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The stub layer the present benchmark draws image frames through: no GL, and no image to read back. */
class DiscardingImageLayerSpec extends AnyFlatSpec with Matchers:

  private def frame(canvas: JPanel, caches: RenderCaches, paint: Boolean, persists: Boolean = true): Unit =
    val request = FrameSurfaceRequest(
      CellMetrics(8, 16, 12),
      new Font(Font.MONOSPACED, Font.PLAIN, 12),
      SwingCanvasGeometry.of(canvas),
      SwingPresentationSite(canvas),
      _ => (),
      (w, h, t) => new BufferedImage(w, h, t),
      caches.surfaceText,
      caches.backendCaches,
      () => None,
      contentPersists = persists
    )
    val surface = DiscardingImageLayer.factory.create(request)
    if paint then surface.pixels.fillPixelRect(0, 0, 40, 40, RenderColor.fromRgba(200, 30, 30))
    surface.flush()

  private def canvas(): JPanel =
    val panel = new JPanel
    panel.setSize(100, 60)
    panel

  "DiscardingImageLayer" should "keep the latest frame's image, and say whether it holds ink" in {
    val window = canvas()
    val caches = RenderCaches.create()
    frame(window, caches, paint = true)
    DiscardingImageLayer.latestHasInk shouldBe true
  }

  it should "say a frame that painted nothing holds none" in {
    val window = canvas()
    frame(window, RenderCaches.create(), paint = false, persists = false)
    DiscardingImageLayer.latestHasInk shouldBe false
  }

  it should "start every frame from the pixels the last one left, as the layer's image frames do" in {
    val window = canvas()
    val caches = RenderCaches.create()
    frame(window, caches, paint = true)
    frame(window, caches, paint = false)
    DiscardingImageLayer.latestHasInk shouldBe true
  }
