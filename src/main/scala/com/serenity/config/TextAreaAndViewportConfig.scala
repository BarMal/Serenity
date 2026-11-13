package com.serenity.config

final case class TextAreaInsets(
    left: Gutter = Gutter.Zero,
    right: Gutter = Gutter.Zero,
    top: Gutter = Gutter.Zero,
    bottom: Gutter = Gutter.Zero
):

  /** Each share capped at [[TextAreaInsets.MaxInset]], and the two shares of a pair at
    * [[TextAreaInsets.MaxCombinedInset]] together. A count, or `auto`, is left as it is: it is not a share of anything.
    */
  def normalized: TextAreaInsets =
    val (normalizedLeft, normalizedRight) = TextAreaInsets.capped(left, right)
    val (normalizedTop, normalizedBottom) = TextAreaInsets.capped(top, bottom)
    TextAreaInsets(normalizedLeft, normalizedRight, normalizedTop, normalizedBottom)

object TextAreaInsets:
  val DefaultInset: Double     = 0.0
  val MaxInset: Double         = 0.45
  val MaxCombinedInset: Double = 0.8
  val MinTextAreaWidth: Double = 1.0 - MaxCombinedInset

  /** Insets that are all shares of the width, as every setting here was before gutters had units. */
  def percent(
    left: Double = DefaultInset,
    right: Double = DefaultInset,
    top: Double = DefaultInset,
    bottom: Double = DefaultInset
  ): TextAreaInsets =
    TextAreaInsets(Gutter.Percent(left), Gutter.Percent(right), Gutter.Percent(top), Gutter.Percent(bottom))

  def fromPercent(
    left: Double,
    right: Double,
    top: Double = DefaultInset,
    bottom: Double = DefaultInset
  ): TextAreaInsets =
    percent(left / 100.0, right / 100.0, top / 100.0, bottom / 100.0).normalized

  def clamp(value: Double): Double =
    value.max(0.0).min(MaxInset)

  /** A pair of opposite sides with each share clamped, and, when both are shares, scaled to fit the combined cap. */
  private[config] def capped(first: Gutter, second: Gutter): (Gutter, Gutter) =
    (first, second) match
      case (Gutter.Percent(a), Gutter.Percent(b)) =>
        val (clampedA, clampedB) = (clamp(a), clamp(b))
        val total                = clampedA + clampedB
        val scale                = if total <= MaxCombinedInset then 1.0 else MaxCombinedInset / total
        (Gutter.Percent(clampedA * scale), Gutter.Percent(clampedB * scale))
      case _ => (clampedShare(first), clampedShare(second))

  private def clampedShare(gutter: Gutter): Gutter =
    gutter match
      case Gutter.Percent(share) => Gutter.Percent(clamp(share))
      case other                 => other
