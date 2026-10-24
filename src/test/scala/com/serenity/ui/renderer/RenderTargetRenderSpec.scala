package com.serenity.ui.renderer

import java.awt.image.BufferedImage

import com.serenity.PersistedFrameFixture
import com.serenity.state.models.{AppState, CursorPosition}
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.Java2DRenderSurface.DeviceScale
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The renderer paints into a [[RenderTarget]], not into a window: a target with no Swing behind it receives the frames
  * a window would, at the size and scale it reports.
  */
class RenderTargetRenderSpec extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  private def targetAt(scale: Double): FakeRenderTarget =
    new FakeRenderTarget(cellMetrics, viewport, logicalW, logicalH, DeviceScale(scale, scale))

  private val state: AppState = stateOf("hello\nworld", CursorPosition(0, 2), dirty = false)

  private def renderFull(target: RenderTarget, cursorVisible: Boolean): Unit =
    RendererEntryPoints.render(
      state,
      cursorVisible,
      target,
      FontSpec.fromAwt(codeFont),
      FontSpec.fromAwt(textFont),
      FontSpec.fromAwt(uiFont),
      CellMetrics.fromFont(uiFont),
      None,
      repaintOnFlush = true
    )

  "RendererEntryPoints.render" should "publish one opaque frame at the target's device size" in {
    val target = targetAt(2.0)
    renderFull(target, cursorVisible = false)

    target.published should have size 1
    val image = target.published.head.image
    (image.getWidth, image.getHeight) shouldBe (deviceSize(logicalW, 2.0), deviceSize(logicalH, 2.0))
    image.getType shouldBe BufferedImage.TYPE_INT_RGB
  }

  it should "size the frame for a fractional scale by rounding up" in {
    val target = targetAt(1.5)
    renderFull(target, cursorVisible = false)

    val image = target.published.head.image
    (image.getWidth, image.getHeight) shouldBe (deviceSize(logicalW, 1.5), deviceSize(logicalH, 1.5))
  }

  it should "draw into the image the target handed out" in {
    val target = targetAt(1.0)
    renderFull(target, cursorVisible = false)

    target.acquisitions should have size 1
    target.published.head.image should be theSameInstanceAs target.acquisitions.head.image
    target.acquisitions.head.imageType shouldBe BufferedImage.TYPE_INT_RGB
  }

  "RendererCursorOverlay.renderWithCursorOverlay" should "publish the base frame and then the carets over it" in {
    val target = targetAt(1.0)
    val rendered = RendererCursorOverlay.renderWithCursorOverlay(
      state,
      target,
      FontSpec.fromAwt(codeFont),
      FontSpec.fromAwt(textFont),
      FontSpec.fromAwt(uiFont),
      CellMetrics.fromFont(uiFont),
      None
    )

    rendered shouldBe true
    target.baseOnly should have size 1
    target.overlayCarets should have size 1
    target.overlayCarets.head should not be empty
  }

  "RendererCursorOverlay.renderCursorOnly" should "decline until the target holds a base frame" in {
    val target = targetAt(1.0)
    val rendered = RendererCursorOverlay.renderCursorOnly(
      state,
      cursorVisible = true,
      target,
      FontSpec.fromAwt(codeFont),
      FontSpec.fromAwt(textFont),
      FontSpec.fromAwt(uiFont),
      CellMetrics.fromFont(uiFont),
      None
    )

    rendered shouldBe false
    target.overlayCarets shouldBe empty
  }
