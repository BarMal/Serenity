package com.serenity.ui.layout

import com.serenity.config.{DockedCornerStyle, InterfaceDensityMetrics}
import com.serenity.state.models.AppState

/** Where the window's chrome sits inside the viewport: the tab strip, the docked panels' area and the status bar.
  *
  * With docked corner style `floating` on a pixel surface the element gap is also left round the whole chrome -- at the
  * window's left, right and top, between the tab strip and the panels, above the status bar and below it -- so every
  * element's corners can round. `flush`, a zero gap or a cell grid leave `inset` at 0, and every rectangle is then
  * today's (#2389).
  */
final case class ChromeFrame(inset: Int, tabBarHeight: Int, gutterHeight: Int, viewport: ViewportSize):

  private val tabGap    = if tabBarHeight > 0 then inset else 0
  private val gutterGap = if gutterHeight > 0 then inset else 0
  private val top       = inset + tabBarHeight + tabGap
  private val bottom    = gutterHeight + gutterGap + inset

  val panelBounds: LayoutRect =
    LayoutRect(
      inset,
      top,
      math.max(1, viewport.width - 2 * inset),
      math.max(1, viewport.height - top - bottom)
    )

  val tabBarRect: Option[LayoutRect] =
    Option.when(tabBarHeight > 0)(LayoutRect(inset, inset, panelBounds.width, tabBarHeight))

  val gutterRect: Option[LayoutRect] =
    Option.when(gutterHeight > 0)(
      LayoutRect(inset, viewport.height - inset - gutterHeight, panelBounds.width, gutterHeight)
    )

  /** The extent a docked panel's share of the window is divided from: the viewport less what `inset` takes. */
  val panelViewport: ViewportSize =
    ViewportSize(panelBounds.width, viewport.height - (2 * inset + tabGap + gutterGap))

  /** The rows above `panelBounds` that a panel's top edge has no part in: the margin and the gap below the tab strip.
    */
  val topMargin: Int = inset + tabGap

object ChromeFrame:

  def of(state: AppState, viewport: ViewportSize): ChromeFrame =
    val gutterHeight =
      if LayoutEngine.usesBottomGutter(state) then
        InterfaceDensityMetrics.forDensity(state.persisted.config.interfaceDensity).gutterHeight
      else 0
    val tabBarHeight = if LayoutEngine.showsTabBar(state) then LayoutEngine.TabBarHeight else 0
    ChromeFrame(inset(state), tabBarHeight, gutterHeight, viewport)

  /** The margin the chrome keeps round itself, in cells: 0 unless the docked corner style is floating on a pixel
    * surface.
    */
  def inset(state: AppState): Int =
    val floating = state.persisted.config.dockedCornerStyle == DockedCornerStyle.Floating
    if floating && !state.runtime.capabilities.isCellGrid then
      math.ceil(math.max(0.0, state.effectiveUiElementGap)).toInt
    else 0
