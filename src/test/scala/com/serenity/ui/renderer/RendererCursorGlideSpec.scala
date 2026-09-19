package com.serenity.ui.renderer

import java.awt.image.BufferedImage

import com.serenity.animation.{EasingCurve, Tween}
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, LayoutRect, PixelPoint, TextLayoutSnapshot}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Caret-glide (issue #1085 phase 2): [[RendererCursorGlyphs.glidePixelPosition]] is the pure rule deciding whether a
  * cursor paints at its glide's tweened pane-relative offset or at its plain logical position -- GUI-canvas-only (TUI
  * mode always reads `None`, per `RendererCursorOverlay.presentHardwareCursor`'s existing GUI/TUI split, since a
  * terminal cursor can't glide sub-cell), and `None` once the glide completes (`Cursor.glide` is cleared by
  * `StateManagerEditorCapability.advanceCursorGlides` at that point, but a defensive check here costs nothing and
  * documents the contract).
  */
class RendererCursorGlideSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val font        = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
  private val cellMetrics = CellMetrics.fromFont(font)

  private val inFlightGlide =
    Tween(start = PixelPoint(0, 0), end = PixelPoint(20, 0), curve = EasingCurve.Linear, steps = 4)

  "glidePixelPosition" should "return the tweened position for a GUI cursor with an in-flight glide" in {
    val cursor = Cursor(CursorPosition(0, 0), glide = Some(inFlightGlide))

    RendererCursorGlyphs.glidePixelPosition(cursor, isTuiMode = false) shouldBe Some(PixelPoint(0, 0))
  }

  it should "return None for a cursor with no glide" in {
    val cursor = Cursor(CursorPosition(0, 0))

    RendererCursorGlyphs.glidePixelPosition(cursor, isTuiMode = false) shouldBe None
  }

  it should "return None once the glide has completed" in {
    val cursor = Cursor(CursorPosition(0, 0), glide = Some(inFlightGlide.copy(currentFrame = 4)))

    RendererCursorGlyphs.glidePixelPosition(cursor, isTuiMode = false) shouldBe None
  }

  it should "return None in TUI mode even with an in-flight glide -- the caret always snaps instantly there" in {
    val cursor = Cursor(CursorPosition(0, 0), glide = Some(inFlightGlide))

    RendererCursorGlyphs.glidePixelPosition(cursor, isTuiMode = true) shouldBe None
  }

  "renderCursors" should "paint the caret at its glide's tweened offset from the pane origin while in flight" in {
    val buffer = Buffer
      .fromString(BufferId(1), "hello world")
      .copy(
        viewport = Viewport(visibleLines = 5, visibleColumns = 40),
        editing = EditingState.fromCursors(List(Cursor(CursorPosition(0, 0), glide = Some(inFlightGlide))))
      )
    val rect     = LayoutRect(0, 0, 40, 5)
    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 400, font)
    val context  = contextWith(cellMetrics)

    val painted = RendererCursorGlyphs.renderCursors(
      buffer,
      rect,
      Theme.dark,
      AppConfig.default,
      context,
      snapshot,
      isTuiMode = false
    )

    painted.headOption.map(_.xPx) shouldBe Some(cellMetrics.toPixelX(rect.x) + inFlightGlide.currentValue.xPx)
  }

  it should "paint the caret at its logical position when isTuiMode is true, ignoring any glide" in {
    val buffer = Buffer
      .fromString(BufferId(1), "hello world")
      .copy(
        viewport = Viewport(visibleLines = 5, visibleColumns = 40),
        editing = EditingState.fromCursors(List(Cursor(CursorPosition(0, 0), glide = Some(inFlightGlide))))
      )
    val rect     = LayoutRect(0, 0, 40, 5)
    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 400, font)
    val context  = contextWith(cellMetrics)

    val painted = RendererCursorGlyphs.renderCursors(
      buffer,
      rect,
      Theme.dark,
      AppConfig.default,
      context,
      snapshot,
      isTuiMode = true
    )

    painted.headOption.map(_.xPx) shouldBe Some(cellMetrics.toPixelX(rect.x))
  }

  private def contextWith(cellMetrics: CellMetrics): RenderContext =
    val image = new BufferedImage(400, 400, BufferedImage.TYPE_INT_ARGB)
    RenderContext(
      surface = new Java2DRenderSurface(image, cellMetrics, font, _ => ()),
      layout = com.serenity.ui.layout.CalculatedLayout(
        editorPanelRect = LayoutRect(0, 0, 80, 24),
        leftSpacerRect = LayoutRect(0, 0, 0, 0),
        rightSpacerRect = LayoutRect(0, 0, 0, 0)
      ),
      codeFont = font,
      textFont = font,
      uiFont = font,
      cellMetrics = cellMetrics,
      uiMetrics = cellMetrics
    )
