package com.serenity.ui.layout

import com.serenity.config.InterfaceDensity

/** The spacing vocabulary UI chrome is laid out with. Steps are named rather than numeric so a surface asks for a
  * *relationship* -- "a small gap", "a roomy inset" -- and the scale decides what that costs in pixels for the current
  * typography and density. Two surfaces asking for the same step are guaranteed to agree; two asking for adjacent steps
  * are guaranteed to differ visibly.
  */
enum SpacingStep(val em: Double):
  case None extends SpacingStep(0.0)
  case Xs   extends SpacingStep(0.25)
  case Sm   extends SpacingStep(0.5)
  case Md   extends SpacingStep(0.75)
  case Lg   extends SpacingStep(1.0)
  case Xl   extends SpacingStep(1.5)

/** Resolves [[SpacingStep]]s against one typographic base.
  *
  * Chrome spacing is expressed relative to the UI font's em rather than in fixed pixels, so it stays proportionate as
  * the UI font size and device scale change -- the same reason `AppConfig.scaledUiCornerRadiusPx` scales corner radius
  * (issue #1542). Fixed pixel spacing under a doubled font size reads as cramped; fixed *cell* spacing, which is what
  * the surface layer used before this type, could only ever be a whole character wide and was therefore both
  * unadjustable and asymmetric (a cell is much taller than it is wide).
  */
final case class SpacingScale(basePx: Double):

  /** The step's width in whole pixels. Any non-[[SpacingStep.None]] step resolves to at least one pixel: a step the
    * caller asked for should separate things, and rounding it away to nothing would silently reintroduce exactly the
    * flush-against-the-edge look the scale exists to remove.
    */
  def px(step: SpacingStep): Int =
    if step == SpacingStep.None then 0
    else math.round(basePx * step.em).toInt.max(1)

object SpacingScale:

  /** Density's effect on chrome spacing. Density already chooses row counts and heights (`InterfaceDensityMetrics`);
    * these multipliers extend the same one control to the space *inside* and *between* surfaces, which is what makes
    * the setting read as a density setting rather than a row-count setting.
    *
    * Public (not just [[forUi]]'s private concern) so a caller that still has to reason in whole grid cells --
    * `AppState.effectiveUiElementGap`/`effectiveLineNumberMarginLeft`/`effectiveLineNumberPadding`, whose consumers
    * (`LayoutEngine`, `PinnedPanelLayoutEngine`, `EditorLayoutContract`) lay out panes in a cell grid shared with the
    * TUI and have no pixel/font-metric input to resolve a [[SpacingStep]] against -- can still apply the same
    * density curve to its cell count, rather than every unset default staying flat regardless of density (issue
    * #1542 re-scope) while every pixel-resolved piece of chrome already varies with it.
    */
  def densityMultiplier(density: InterfaceDensity): Double =
    density match
      case InterfaceDensity.Compact     => 0.75
      case InterfaceDensity.Comfortable => 1.0
      case InterfaceDensity.Spacious    => 1.35

  /** @param uiFontSizePx
    *   the UI font size actually being rendered with -- `FontConfig.scaledUiFontSize`, which already carries the text
    *   scale and device scale, rather than the configured nominal size.
    */
  def forUi(uiFontSizePx: Double, density: InterfaceDensity): SpacingScale =
    val safeFontSize = if uiFontSizePx.isFinite && uiFontSizePx > 0.0 then uiFontSizePx else 0.0
    SpacingScale(safeFontSize * densityMultiplier(density))

  /** A terminal cell is the smallest thing a TUI can move by and cannot be subdivided, so every step that means "some
    * space" resolves to exactly one cell there. The distinctions the GUI draws between `Xs` and `Xl` are real but
    * unrepresentable in a character grid; collapsing them keeps every surface on one spacing vocabulary instead of
    * branching on render mode at each call site.
    *
    * A zero base expresses that directly rather than by special case: `px`'s one-pixel floor -- there so a step the
    * caller asked for always separates something -- is already "one unit of whatever this scale measures in", and a
    * terminal's unit is the cell.
    */
  val terminal: SpacingScale = SpacingScale(0.0)
