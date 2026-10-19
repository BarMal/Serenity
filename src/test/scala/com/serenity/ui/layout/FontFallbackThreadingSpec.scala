package com.serenity.ui.layout

import java.awt.{Font, GraphicsEnvironment}
import java.nio.file.Paths

import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.ui.fonts.FontFallback
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1866: the configured fallback families reach every layout entry point, so rows are counted, wrapped and cached for
  * the fonts that will draw them.
  */
class FontFallbackThreadingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val frc    = TextLayoutSnapshot.defaultFontRenderContext()
  private val sizePx = 14.0f

  private def registered(resource: String): Font =
    val font = Font.createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource(resource).toURI).toFile)
    val _    = GraphicsEnvironment.getLocalGraphicsEnvironment.registerFont(font)
    font

  private val fallback = FontFallback(Vector(registered("/fonts/SerenityFallbackTest.ttf").getFamily))
  private val font     = registered("/fonts/SerenityProseTest.ttf").deriveFont(Font.PLAIN, sizePx)
  private val buffer   = Buffer.fromString(BufferId(1), "中中中中中中中中")

  // Four fixture 中 (one em each) fit a row of 4 * 14 = 56 px, so eight take two rows.
  private val panelPx = 56

  "Counting rows with a fallback chain" should "wrap on the fallback font's advances" in {
    val rows = VisualRowCounts.forBuffer(
      buffer,
      panelPx,
      font,
      None,
      false,
      WrappedLineCache.bounded(),
      fallback = fallback
    )
    rows.rowsIn(0) shouldBe 2
  }

  "A snapshot built with a fallback chain" should "lay the fallback glyphs out in the fallback font" in {
    val snapshot = TextLayoutSnapshot.fromBuffer(
      buffer,
      panelPx,
      font,
      frc,
      wordWrapEnabled = true,
      wrapCache = WrappedLineCache.bounded(),
      fallback = fallback
    )
    snapshot.visualLines.map(_.text) shouldBe Vector("中中中中", "中中中中")
    snapshot.visualLines.foreach(line => line.widthPx shouldBe (4 * sizePx) +- 0.01f)
  }

  "The row-count key" should "differ when the fallback chain does" in {
    def key(chain: FontFallback) =
      VisualRowKey(BufferId(1), panelPx, font, frc, None, forceCellLayout = false, dropCapsEnabled = true, chain)
    key(fallback) should not be key(FontFallback.Default)
    key(fallback) shouldBe key(FontFallback(fallback.families))
  }
