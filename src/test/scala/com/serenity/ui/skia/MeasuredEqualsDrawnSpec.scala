package com.serenity.ui.skia

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

import com.serenity.ui.fonts.LogicalFontFamilies
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Layout measures with AWT and Skia draws, so the face a logical family resolves to has to advance the same in both:
  * that is what makes measured equal drawn. Compared against fontconfig itself, so it only runs where there is one.
  */
class MeasuredEqualsDrawnSpec extends AnyFlatSpec with Matchers:

  private val probe   = "Buffer 1 - unsaved Hamburgefonstiv"
  private val context = FontRenderContext(AffineTransform(), true, true)

  private def onLinuxWithFontconfig(): Unit =
    assume(System.getProperty("os.name", "").toLowerCase.contains("linux"), "the physical face comes from fontconfig")
    assume(
      scala.util.Try(new ProcessBuilder("fc-match", "--version").start().waitFor() == 0).getOrElse(false),
      "fc-match is not available"
    )

  List(Font.SANS_SERIF, Font.SERIF, Font.MONOSPACED, Font.DIALOG, Font.DIALOG_INPUT).foreach { logical =>
    s"The face $logical resolves to" should "advance the same in AWT and in Skia" in {
      onLinuxWithFontconfig()
      val resolved = LogicalFontFamilies.system.resolve(logical)
      withClue(s"$logical resolved to '$resolved'; ")(resolved should not be logical)
      val awt     = new Font(resolved, Font.PLAIN, 12)
      val awtSize = awt.getStringBounds(probe, context).getWidth
      val skia    = new SkiaFontCache().faceFor(awt).font.measureTextWidth(probe).toDouble
      withClue(s"$logical -> '$resolved': AWT advance $awtSize, Skia advance $skia; ")(
        math.abs(awtSize - skia) / awtSize should be <= 0.001
      )
    }
  }
