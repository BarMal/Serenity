package com.serenity.ui.skia

import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

import com.serenity.ui.renderer.{FontFirstUse, FontSpec, StyledFontCache}

/** What Skia still asks AWT about a font: the advance AWT measures a probe string at, which picks among the weights of
  * a family the one layout measures with, and the geometry of a strikethrough rule. Both go when layout measures with
  * Skia, so they live here, where the Skia measurement path itself names no AWT.
  *
  * A spec made from data has no AWT font to ask: it picks its face by style alone.
  */
private[skia] object AwtFontReconciliation:

  val MeasureProbe           = "Buffer 1 - unsaved Hamburgefonstiv"
  private val MeasureContext = FontRenderContext(AffineTransform(), true, true)

  def referenceAdvance(spec: FontSpec): Option[Double] =
    spec.awtOrigin.map { awt =>
      FontFirstUse.settle(awt, MeasureContext)
      awt.getStringBounds(MeasureProbe, MeasureContext).getWidth
    }

  def strikethroughOf(spec: FontSpec): Option[StyledFontCache.Underline] =
    Option.when(spec.strikethrough) {
      val metrics = spec.toAwt.getLineMetrics("", MeasureContext)
      StyledFontCache.Underline(metrics.getStrikethroughOffset, metrics.getStrikethroughThickness)
    }
