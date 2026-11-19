package com.serenity.ui.skia

import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

import com.serenity.ui.renderer.{FontSpec, StyledFontCache}

/** What Skia still asks AWT about a font: the geometry of a strikethrough rule. It goes when the text stack leaves AWT
  * fonts, so it lives here, where the Skia measurement path itself names no AWT.
  */
private[skia] object AwtFontReconciliation:

  private val MeasureContext = FontRenderContext(AffineTransform(), true, true)

  def strikethroughOf(spec: FontSpec): Option[StyledFontCache.Underline] =
    Option.when(spec.strikethrough) {
      val metrics = spec.toAwt.getLineMetrics("", MeasureContext)
      StyledFontCache.Underline(metrics.getStrikethroughOffset, metrics.getStrikethroughThickness)
    }
