package com.serenity

import java.awt.font.{TextAttribute, TextHitInfo, TextLayout}
import java.awt.image.BufferedImage
import java.text.AttributedString

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, Viewport}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}
import com.serenity.ui.renderer.Java2DRenderSurface

/** Verifies that the FontRenderContext used by Java2DRenderSurface for drawing text matches the one used by
  * TextLayoutSnapshot for measuring caret positions. A mismatch causes the cursor to drift leftward as characters are
  * typed on a proportional-font line.
  */
class CursorPixelAlignmentSpec extends AnyFlatSpec with Matchers:

  given Balance    = Balance.default
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val proportionalFont =
    FontLoader
      .loadTextFont(FontConfig(textFontFamily = "SansSerif", fontSize = 14.0f))
      .unsafeRunSync()

  "Java2DRenderSurface" should "use fractional metrics so text advances match TextLayoutSnapshot measurements" in {
    val cellMetrics = CellMetrics.fromFont(proportionalFont)
    val image       = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB)
    val surface     = new Java2DRenderSurface(image, cellMetrics, proportionalFont, _ => ())

    surface.fontRenderContext.getOrElse(fail("missing FontRenderContext")).usesFractionalMetrics() shouldBe true
  }

  "TextLayoutSnapshot caret stops" should
    "agree with Java2DRenderSurface layout advances for a proportional narrow-character line" in {
      val font        = proportionalFont
      val cellMetrics = CellMetrics.fromFont(font)

      // 16 narrow 'i' characters: at column 14 even a 0.1 px/char FRC discrepancy accumulates to
      // ~1.4 px, well above the 0.5 px tolerance that catches real drift.
      val text         = "iiiiiiiiiiiiiiii"
      val cursorColumn = 14

      // Get the FRC that Java2DRenderSurface actually uses at runtime.
      val testImage   = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB)
      val surface     = new Java2DRenderSurface(testImage, cellMetrics, font, _ => ())
      val rendererFRC = surface.fontRenderContext.getOrElse(fail("missing FontRenderContext"))

      // Measure column 14 using the renderer's FRC.
      val attributed = new AttributedString(text)
      attributed.addAttribute(TextAttribute.FONT, font)
      val layout    = new TextLayout(attributed.getIterator, rendererFRC)
      val rendererX = layout.getCaretInfo(TextHitInfo.leading(cursorColumn))(0)

      // Measure column 14 using TextLayoutSnapshot (the source of truth for cursor placement).
      val buffer = Buffer
        .fromString(BufferId(99), text)
        .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 30, visibleLines = 2))
      val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 400, font)
      val snapshotX = snapshot.visualLines.head
        .xForColumn(cursorColumn)
        .getOrElse(fail(s"no caret stop at column $cursorColumn"))

      snapshotX shouldBe rendererX +- 0.5f
    }

  it should "measure the default bundled code font with the renderer FontRenderContext when precise layout is required" in {
    val font         = FontLoader.loadCodeFont(FontConfig()).unsafeRunSync()
    val cellMetrics  = CellMetrics.fromFont(font)
    val text         = "iiiiiiiiiiiiiiii"
    val cursorColumn = 14

    val testImage   = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB)
    val surface     = new Java2DRenderSurface(testImage, cellMetrics, font, _ => ())
    val rendererFRC = surface.fontRenderContext.getOrElse(fail("missing FontRenderContext"))
    val attributed  = new AttributedString(text)
    attributed.addAttribute(TextAttribute.FONT, font)
    val layout    = new TextLayout(attributed.getIterator, rendererFRC)
    val rendererX = layout.getCaretInfo(TextHitInfo.leading(cursorColumn))(0)
    val buffer = Buffer
      .fromString(BufferId(100), text)
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 30, visibleLines = 2))
    val snapshot     = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 400, font, rendererFRC)
    val snapshotLine = snapshot.visualLines.head
    val snapshotX = snapshotLine
      .xForColumn(cursorColumn)
      .getOrElse(fail(s"no caret stop at column $cursorColumn"))

    snapshot.usesMeasuredLayout shouldBe true
    snapshotX shouldBe rendererX +- 0.5f
  }
