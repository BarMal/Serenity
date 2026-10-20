package com.serenity.ui.theme

object HeadingScale:

  val DefaultDampingThresholdPt: Float = 24.0f

  /** The size multiple a heading is drawn at over a body of `logicalBodyPt`. Above `threshold` the part of the multiple
    * that exceeds 1 shrinks by sqrt(threshold / body), so big bodies do not produce enormous headings. The damping is
    * strictly increasing in `multiple`, so the H1 > H2 > H3 ordering survives it, and no heading falls below the body.
    */
  def effective(multiple: Float, logicalBodyPt: Float, threshold: Float, dampingOn: Boolean): Float =
    val damped =
      if dampingOn && logicalBodyPt > threshold then
        1.0f + (multiple - 1.0f) * math.sqrt(threshold.toDouble / logicalBodyPt.toDouble).toFloat
      else multiple
    damped.max(1.0f)

  /** The body size before any device scaling, so a hi-DPI display never tightens the headings. */
  def logicalBodyPt(proseSizePt: Float, manualMultiplier: Float): Float =
    proseSizePt * manualMultiplier
