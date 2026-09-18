package com.serenity.animation

/** A tick-driven scalar progress value in `[0.0, 1.0]`, advanced one discrete step at a time by the same render-loop
  * tick cadence every other animation in this package uses (see `AppRuntime.AnimationTickCadence`).
  *
  * The colour-interpolation primitives above ([[ColorTimeline]]/[[AnimatedCell]]) don't generalise to a plain number --
  * they are built entirely around [[RgbInterpolator]] -- so this is the minimal scalar counterpart a feature that
  * animates a position (rather than a colour) needs, such as the column-to-column sweep in
  * `com.serenity.ui.renderer.RendererColumnTransition`.
  */
final case class ScalarTimeline(steps: Int, currentFrame: Int = 0):

  def progress: Double =
    if steps <= 0 then 1.0 else math.min(1.0, currentFrame.toDouble / steps.toDouble)

  def advance: ScalarTimeline =
    if isComplete then this else copy(currentFrame = currentFrame + 1)

  def isComplete: Boolean =
    steps <= 0 || currentFrame >= steps
