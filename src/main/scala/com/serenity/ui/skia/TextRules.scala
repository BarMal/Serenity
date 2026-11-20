package com.serenity.ui.skia

import java.lang.invoke.{MethodHandle, MethodHandles, MethodType}

import scala.util.Try

import org.jetbrains.skia.{Font, FontMetrics}

/** A rule drawn with text: `offsetPx` from the baseline to the top of its stroke (down is positive), and its thickness.
  */
final case class TextRule(offsetPx: Float, thicknessPx: Float)

/** Where an underline and a strikethrough go, from the font's own Skia metrics. A font that gives none (an older font
  * has no such table) gets rules in proportion to its size: a stroke of a fourteenth of it, an underline a tenth of it
  * below the baseline, and a strikethrough through the middle of its x-height.
  */
private[skia] object TextRules:

  private val FallbackThickness = 1.0f / 14.0f
  private val FallbackUnderline = 0.1f
  private val FallbackXHeight   = 0.5f

  private def thicknessOf(supplied: Option[Float], sizePx: Float): Float =
    supplied.filter(_ > 0f).getOrElse(math.max(1.0f, sizePx * FallbackThickness))

  def underline(position: Option[Float], thickness: Option[Float], sizePx: Float): TextRule =
    TextRule(position.getOrElse(sizePx * FallbackUnderline), thicknessOf(thickness, sizePx))

  /** Skia gives the strikeout's position as the bottom of its stroke; the rule is placed by the top. */
  def strikethrough(
    position: Option[Float],
    thickness: Option[Float],
    xHeightPx: Option[Float],
    sizePx: Float
  ): TextRule =
    val stroke = thicknessOf(thickness, sizePx)
    val bottom =
      position.getOrElse(-xHeightPx.filter(_ > 0f).getOrElse(sizePx * FallbackXHeight) / 2.0f + stroke / 2.0f)
    TextRule(bottom - stroke, stroke)

  private val XHeightIndex = 9

  /** Skiko's optional values come out of the metrics array through static accessors under names Scala cannot call. */
  private def optional(name: String): Option[MethodHandle] =
    Try(
      MethodHandles
        .publicLookup()
        .findStatic(classOf[FontMetrics], name, MethodType.methodType(classOf[java.lang.Float], classOf[Array[Float]]))
    ).toOption

  private val UnderlineThickness = optional("getUnderlineThickness-impl")
  private val UnderlinePosition  = optional("getUnderlinePosition-impl")
  private val StrikeoutThickness = optional("getStrikeoutThickness-impl")
  private val StrikeoutPosition  = optional("getStrikeoutPosition-impl")

  private def value(accessor: Option[MethodHandle], raw: Array[Float]): Option[Float] =
    accessor.flatMap { handle =>
      val answer: Object = handle.invoke(raw)
      Option(answer).collect { case number: java.lang.Float => number.floatValue }
    }

  def underlineOf(font: Font): TextRule =
    val raw = SkiaVerticalMetrics.raw(font)
    underline(
      raw.flatMap(value(UnderlinePosition, _)),
      raw.flatMap(value(UnderlineThickness, _)),
      font.getSize
    )

  def strikethroughOf(font: Font): TextRule =
    val raw = SkiaVerticalMetrics.raw(font)
    strikethrough(
      raw.flatMap(value(StrikeoutPosition, _)),
      raw.flatMap(value(StrikeoutThickness, _)),
      raw.map(_(XHeightIndex)),
      font.getSize
    )
