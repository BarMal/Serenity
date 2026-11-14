package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.TextAttribute

import scala.jdk.CollectionConverters.*

/** The font a [[RenderSurface]] draws text with and the renderer entry points are handed. Owned by the render seam so
  * frontends and renderers name a font without naming `java.awt` (#1812).
  *
  * A spec is the data that picks a face: family, the face's own name, size, style and the two text attributes layout
  * and drawing read. Two specs with the same data are equal, so caches key on what chooses the face and not on which
  * AWT instance happened to describe it. `faceName` is there because AWT calls a bundled bold file plain: only the face
  * name tells it from the regular file of the same family.
  *
  * Layout, Swing chrome and PDF export still measure and draw through AWT, so a spec read from an AWT font keeps that
  * font and [[toAwt]] returns it; one made from data builds an equivalent. The kept font is not part of equality.
  */
final case class FontSpec(
    family: String,
    faceName: String,
    sizePt: Float,
    bold: Boolean,
    italic: Boolean,
    ligatures: Boolean,
    strikethrough: Boolean
)(origin: Option[Font] = None):

  def isBold: Boolean = bold

  def isItalic: Boolean = italic

  /** The AWT font this spec was read from, when it was read from one. */
  def awtOrigin: Option[Font] = origin

  def toAwt: Font = origin.getOrElse(FontSpec.awtFontFor(this))

object FontSpec:

  def fromAwt(font: Font): FontSpec =
    val attributes = font.getAttributes
    FontSpec(
      font.getFamily,
      font.getFontName,
      font.getSize2D,
      font.isBold,
      font.isItalic,
      attributes.get(TextAttribute.LIGATURES) == TextAttribute.LIGATURES_ON,
      attributes.get(TextAttribute.STRIKETHROUGH) == TextAttribute.STRIKETHROUGH_ON
    )(Some(font))

  private def awtFontFor(spec: FontSpec): Font =
    val style = (if spec.bold then Font.BOLD else Font.PLAIN) | (if spec.italic then Font.ITALIC else Font.PLAIN)
    val base  = new Font(spec.family, style, math.round(spec.sizePt)).deriveFont(spec.sizePt)
    val attributes: Map[TextAttribute, AnyRef] =
      Map.from(
        Option.when(spec.ligatures)(TextAttribute.LIGATURES -> TextAttribute.LIGATURES_ON) ++
          Option.when(spec.strikethrough)(TextAttribute.STRIKETHROUGH -> TextAttribute.STRIKETHROUGH_ON)
      )
    if attributes.isEmpty then base else base.deriveFont(attributes.asJava)
