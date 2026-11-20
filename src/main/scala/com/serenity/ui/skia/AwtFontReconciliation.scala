package com.serenity.ui.skia

import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

import com.serenity.ui.renderer.{FontFirstUse, FontSpec, StyledFontCache}

/** What Skia still asks AWT about a font: the geometry of a strikethrough rule. It goes when the text stack leaves AWT
  * fonts, so it lives here, where the Skia measurement path itself names no AWT.
  */
private[skia] object AwtFontReconciliation:

  private val MeasureContext = FontRenderContext(AffineTransform(), true, true)

  /** An AWT font's first use is where AWT disagreed with itself across threads, so a face built for a spec read from
    * one settles that font before anything reads it, as every path that measures or shapes text has to.
    */
  def settle(spec: FontSpec): Unit =
    spec.awtOrigin.foreach(awt => FontFirstUse.settle(awt, MeasureContext))

  def strikethroughOf(spec: FontSpec): Option[StyledFontCache.Underline] =
    Option.when(spec.strikethrough) {
      val metrics = spec.toAwt.getLineMetrics("", MeasureContext)
      StyledFontCache.Underline(metrics.getStrikethroughOffset, metrics.getStrikethroughThickness)
    }
