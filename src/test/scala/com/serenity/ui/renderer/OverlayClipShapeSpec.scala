package com.serenity.ui.renderer

import java.awt.Font

import com.serenity.MockRenderSurface
import com.serenity.config.{AppConfig, DockedCornerStyle}
import com.serenity.state.models.UiSurface
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, LayoutRect, OverlayRow, RoundedCorners, TextOverlayView}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389: Java2D paints every cell inside a shape clip about thirty times slower than inside a rectangle, and the
  * status line is painted in every frame, so it takes its rounded corners from its fill and is clipped to a rectangle.
  * A floating panel is painted rarely, and keeps the exact shape clip.
  */
class OverlayClipShapeSpec extends AnyFlatSpec with Matchers:

  final private class CountingSurface extends MockRenderSurface(40, 6):
    var shapeClips: Int   = 0
    var roundedFills: Int = 0

    override def withRoundRectClip(x: Int, y: Int, width: Int, height: Int, corners: RoundedCorners, radiusPx: Int)(
      render: => Unit
    ): Unit =
      shapeClips += 1
      render

    override def fillRoundRect(
      x: Int,
      y: Int,
      width: Int,
      height: Int,
      corners: RoundedCorners,
      radiusPx: Int,
      color: RenderColor
    ): Boolean =
      roundedFills += 1
      true

  private val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val metrics = CellMetrics.fromFont(font)
  private val config  = AppConfig.conventional.withUiCornerRadiusPx(12).withDockedCornerStyle(DockedCornerStyle.Floating)
  private val shaped  = MeasuredTextAdvances(MeasuredTextAdvances.DefaultCapacity, ShapedTextCache())

  private def paint(surfaceId: Option[com.serenity.state.models.SurfaceId]): CountingSurface =
    val surface = new CountingSurface
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 30, 1),
      rows = List(OverlayRow(plainText = "ready")),
      surfaceId = surfaceId
    )
    TextOverlayRenderer.render(surface, overlay, Theme.light, config, false, font, metrics, shaped)
    surface

  "A status line with rounded corners" should "be filled with its corners and clipped to no shape" in {
    val surface = paint(Some(UiSurface.StatusLineSurfaceId))

    surface.shapeClips shouldBe 0
    surface.roundedFills should be >= 1
  }

  "A floating panel with rounded corners" should "keep the exact shape clip" in {
    val surface = paint(None)

    surface.shapeClips shouldBe 1
  }
