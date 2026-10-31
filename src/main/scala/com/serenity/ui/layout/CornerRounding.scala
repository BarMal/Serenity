package com.serenity.ui.layout

import com.serenity.config.{DockedCornerStyle, PaneHeaderCorners}

/** Which of a rectangle's four corners are rounded. */
final case class RoundedCorners(topLeft: Boolean, topRight: Boolean, bottomRight: Boolean, bottomLeft: Boolean):
  def any: Boolean = topLeft || topRight || bottomRight || bottomLeft

object RoundedCorners:
  val All: RoundedCorners  = RoundedCorners(true, true, true, true)
  val None: RoundedCorners = RoundedCorners(false, false, false, false)
  val Top: RoundedCorners  = RoundedCorners(topLeft = true, topRight = true, bottomRight = false, bottomLeft = false)

/** The pieces of GUI chrome whose corners round (#2389). */
enum ChromeElement:
  case FloatingSurface
  case DockedPanel
  case StatusBar
  case TabStrip
  case PaneHeader

/** Which corners of each piece of chrome round, as a pure function of where it sits in the window. */
object CornerRounding:

  /** For a caller that does not know the window's size: no right or bottom edge is ever reached. */
  val UnboundedWindow: ViewportSize = ViewportSize(Int.MaxValue, Int.MaxValue)

  def corners(
    element: ChromeElement,
    rect: LayoutRect,
    viewport: ViewportSize,
    style: DockedCornerStyle,
    header: PaneHeaderCorners
  ): RoundedCorners =
    element match
      case ChromeElement.FloatingSurface => RoundedCorners.All
      case ChromeElement.PaneHeader =>
        header match
          case PaneHeaderCorners.Rounded => RoundedCorners.Top
          case PaneHeaderCorners.Square  => RoundedCorners.None
      case ChromeElement.DockedPanel | ChromeElement.StatusBar | ChromeElement.TabStrip =>
        style match
          case DockedCornerStyle.Floating => RoundedCorners.All
          case DockedCornerStyle.Flush    => offTheWindowEdge(rect, viewport)

  /** A corner on one of the window's edges stays square; every other corner faces something else and rounds. */
  private def offTheWindowEdge(rect: LayoutRect, viewport: ViewportSize): RoundedCorners =
    val left   = rect.x <= 0
    val right  = rect.right >= viewport.width
    val top    = rect.y <= 0
    val bottom = rect.bottom >= viewport.height
    RoundedCorners(
      topLeft = !left && !top,
      topRight = !right && !top,
      bottomRight = !right && !bottom,
      bottomLeft = !left && !bottom
    )

  /** A radius no larger than half the smaller side, so a small surface cannot invert its arc. */
  def effectiveRadiusPx(radiusPx: Int, widthPx: Int, heightPx: Int): Int =
    radiusPx.min(widthPx.min(heightPx) / 2).max(0)
