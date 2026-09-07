package com.serenity.ui.terminal

import java.awt.*
import java.util.concurrent.atomic.AtomicReference
import javax.swing.JComponent

import com.serenity.config.WindowChromeMode
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.theme.Theme

/** Pure geometry, palette, and control-layout logic for the custom window chrome (title bar, control buttons,
  * rounded corners). Mixed into [[SwingWindow]]'s companion object so this stays `SwingWindow.XyzMetrics` etc. to
  * every caller, while living in its own file to keep `SwingWindow.scala` within the architecture ratchet's line
  * target.
  */
private[terminal] trait SwingWindowChromeSupport:

  final case class ChromeMetrics(
      titleBarHeight: Int,
      buttonWidth: Int,
      margin: Int,
      cornerArc: Int,
      minWidth: Int,
      minHeight: Int,
      titleFontSize: Int
  )

  object ChromeMetrics:
    private val BaseTitleBarHeight = 32
    private val BaseButtonWidth    = 46
    private val BaseMargin         = 6
    private val BaseCornerArc      = 12
    private val BaseTitleFontSize  = 13

    def fromCellMetrics(metrics: CellMetrics): ChromeMetrics =
      val scale = (metrics.lineHeight.toDouble / SwingWindow.DefaultMetrics.lineHeight.toDouble).max(1.0)
      ChromeMetrics(
        titleBarHeight = scaledInt(BaseTitleBarHeight, scale),
        buttonWidth = scaledInt(BaseButtonWidth, scale),
        margin = scaledInt(BaseMargin, scale),
        cornerArc = scaledInt(BaseCornerArc, scale),
        minWidth = scaledInt(SwingWindow.BaseMinWidth, scale),
        minHeight = scaledInt(SwingWindow.BaseMinHeight, scale),
        titleFontSize = scaledInt(BaseTitleFontSize, scale)
      )

    private def scaledInt(value: Int, scale: Double): Int =
      math.round(value.toDouble * scale).toInt.max(1)

  private[serenity] def shouldUseCustomChrome(
    chromeMode: WindowChromeMode,
    osName: String = System.getProperty("os.name", "")
  ): Boolean =
    chromeMode == WindowChromeMode.Custom ||
      (chromeMode == WindowChromeMode.Auto && osName.toLowerCase(java.util.Locale.ROOT).contains("linux"))

  private[serenity] def shouldUsePerPixelRoundedCorners(
    usesCustomChrome: Boolean,
    maximized: Boolean,
    perPixelTranslucencySupported: Boolean
  ): Boolean =
    usesCustomChrome && !maximized && perPixelTranslucencySupported

  private[serenity] def roundedCornerMask(
    usesCustomChrome: Boolean,
    maximized: Boolean,
    perPixelTranslucencySupported: Boolean,
    cornerArc: Int
  ): Option[Int] =
    Option.when(shouldUsePerPixelRoundedCorners(usesCustomChrome, maximized, perPixelTranslucencySupported))(
      cornerArc.max(0)
    )

  private[serenity] def shouldRefreshRoundedCornerMask(previous: Option[Int], current: Option[Int]): Boolean =
    previous != current

  /** A theme with an alpha-0 background (e.g. the built-in "Transparent" theme) should only make `canvas` paint its own
    * background as genuinely see-through when the window can actually composite that against the desktop: custom chrome
    * (the frame itself is undecorated with a transparent `Color` background, see `frame`'s construction) and per-pixel
    * translucency support from the platform. Anywhere else -- native OS chrome, or a platform/window manager without
    * translucency -- painting nothing would composite garbage (stale backbuffer pixels, or an opaque-but-undefined
    * native surface) rather than the desktop, so that combination must fall back to an ordinary opaque paint instead.
    */
  private[serenity] def shouldPaintTransparentContent(
    usesCustomChrome: Boolean,
    perPixelTranslucencySupported: Boolean,
    backgroundAlpha: Int
  ): Boolean =
    usesCustomChrome && perPixelTranslucencySupported && backgroundAlpha == 0

  /** Paint `canvas`'s own background into `g` (sized `width` x `height`): genuinely transparent pixels when
    * `transparent`, replacing whatever the backing buffer already held (`AlphaComposite.Src`, not the default
    * `SrcOver`, so this actually clears stale opaque pixels rather than leaving a zero-alpha fill's no-op) -- otherwise
    * an ordinary opaque black fill, the graceful fallback for when [[shouldPaintTransparentContent]] is false. `g`
    * should be a scratch `Graphics2D` the caller disposes (`Graphics.create()`), since this permanently changes its
    * composite.
    */
  private[serenity] def paintCanvasBackground(g: Graphics2D, width: Int, height: Int, transparent: Boolean): Unit =
    if transparent then
      g.setComposite(AlphaComposite.Src)
      g.setColor(SwingWindow.Transparent)
      g.fillRect(0, 0, width, height)
    else
      g.setColor(Color.BLACK)
      g.fillRect(0, 0, width, height)

  private[serenity] def setAccessibleNameIfAvailable(component: JComponent, name: String): Unit =
    Option(component.getAccessibleContext).foreach(_.setAccessibleName(name))

  enum ChromeControlKind(val accessibleName: String):
    case Minimize extends ChromeControlKind("Minimize")
    case Maximize extends ChromeControlKind("Maximize")
    case Restore  extends ChromeControlKind("Restore")
    case Close    extends ChromeControlKind("Close")

  enum ChromeControlPlacement:
    case Left
    case Right

  final case class ChromeControlLayout(placement: ChromeControlPlacement, controls: scala.List[ChromeControlKind]):

    def flowAlignment: Int =
      placement match
        case ChromeControlPlacement.Left  => FlowLayout.LEFT
        case ChromeControlPlacement.Right => FlowLayout.RIGHT

  object ChromeControlLayout:
    val WindowsOrder: scala.List[ChromeControlKind] =
      scala.List(ChromeControlKind.Minimize, ChromeControlKind.Maximize, ChromeControlKind.Close)
    val MacOrder: scala.List[ChromeControlKind] =
      scala.List(ChromeControlKind.Close, ChromeControlKind.Minimize, ChromeControlKind.Maximize)

    def current: ChromeControlLayout =
      forOs(System.getProperty("os.name", ""))

    def forOs(osName: String): ChromeControlLayout =
      if osName.toLowerCase(java.util.Locale.ROOT).contains("mac") then
        ChromeControlLayout(ChromeControlPlacement.Left, MacOrder)
      else ChromeControlLayout(ChromeControlPlacement.Right, WindowsOrder)

  final case class ChromeIconLine(x1: Int, y1: Int, x2: Int, y2: Int)

  final case class ChromeControlState(
      hovered: Boolean = false,
      pressed: Boolean = false,
      focused: Boolean = false
  )

  object ChromeControlPaint:

    def background(kind: ChromeControlKind, palette: ChromePalette, state: ChromeControlState): Color =
      if state.pressed && kind == ChromeControlKind.Close then palette.closePressedBackground
      else if state.pressed then palette.buttonPressedBackground
      else if state.hovered && kind == ChromeControlKind.Close then palette.closeHoverBackground
      else if state.hovered then palette.buttonHoverBackground
      else palette.titleBackground

    def foreground(kind: ChromeControlKind, palette: ChromePalette, state: ChromeControlState): Color =
      if (state.hovered || state.pressed) && kind == ChromeControlKind.Close then palette.closeHoverForeground
      else palette.titleForeground

    def focusBorder(palette: ChromePalette, state: ChromeControlState): Option[Color] =
      Option.when(state.focused)(palette.focusBorder)

  object ChromeIconGeometry:

    def lines(kind: ChromeControlKind, width: Int, height: Int): scala.List[ChromeIconLine] =
      val box     = iconBox(width, height)
      val left    = box.x
      val right   = box.x + box.width
      val top     = box.y
      val bottom  = box.y + box.height
      val middleY = box.y + box.height / 2
      kind match
        case ChromeControlKind.Minimize =>
          scala.List(ChromeIconLine(left, middleY + box.height / 3, right, middleY + box.height / 3))
        case ChromeControlKind.Maximize =>
          scala.List(
            ChromeIconLine(left, top, right, top),
            ChromeIconLine(right, top, right, bottom),
            ChromeIconLine(right, bottom, left, bottom),
            ChromeIconLine(left, bottom, left, top)
          )
        case ChromeControlKind.Restore =>
          val offset = math.max(2, box.width / 4)
          scala.List(
            ChromeIconLine(left + offset, top, right, top),
            ChromeIconLine(right, top, right, bottom - offset),
            ChromeIconLine(left, top + offset, right - offset, top + offset),
            ChromeIconLine(right - offset, top + offset, right - offset, bottom),
            ChromeIconLine(right - offset, bottom, left, bottom),
            ChromeIconLine(left, bottom, left, top + offset)
          )
        case ChromeControlKind.Close =>
          scala.List(
            ChromeIconLine(left, top, right, bottom),
            ChromeIconLine(right, top, left, bottom)
          )

    def strokeWidth(height: Int): Int =
      math.max(1, math.round(height.toDouble / 16.0).toInt)

    private def iconBox(width: Int, height: Int): Rectangle =
      val size = math.max(8, math.min(width, height) / 3)
      new Rectangle((width - size) / 2, (height - size) / 2, size, size)

  final case class ChromePalette(
      titleBackground: Color,
      titleForeground: Color,
      border: Color,
      buttonHoverBackground: Color,
      buttonPressedBackground: Color,
      closeHoverBackground: Color,
      closePressedBackground: Color,
      closeHoverForeground: Color,
      focusBorder: Color
  )

  object ChromePalette:

    def fromTheme(theme: Theme): ChromePalette =
      ChromePalette(
        titleBackground = theme.panel.background,
        titleForeground = theme.panel.foreground,
        border = theme.panelBorder,
        buttonHoverBackground = blend(theme.highlighted.background, theme.panel.background, 0.24),
        buttonPressedBackground = blend(theme.highlighted.background, theme.panel.background, 0.38),
        closeHoverBackground = theme.error.foreground,
        closePressedBackground = blend(theme.error.foreground, theme.background, 0.82),
        closeHoverForeground = theme.background,
        focusBorder = theme.highlighted.foreground
      )

    private def blend(foreground: Color, background: Color, foregroundWeight: Double): Color =
      val clampedWeight    = foregroundWeight.max(0.0).min(1.0)
      val backgroundWeight = 1.0 - clampedWeight
      def channel(value: Color => Int): Int =
        math.round(value(foreground) * clampedWeight + value(background) * backgroundWeight).toInt
      new Color(channel(_.getRed), channel(_.getGreen), channel(_.getBlue))

  /** Avoids redundant native DWM updates while preserving applications for palette changes. */
  final private[serenity] class NativeChromeThemeCache:
    private val paletteRef = new AtomicReference[Option[ChromePalette]](None)

    def recordIfChanged(palette: ChromePalette, supported: Boolean): Boolean =
      supported && paletteRef.getAndSet(Some(palette)) != Some(palette)

  final case class TitleBarDragDecision(restoreFirst: Boolean, moveDelta: Option[(Int, Int)])

  private[serenity] def titleBarDragDecision(
    maximized: Boolean,
    anchorX: Int,
    anchorY: Int,
    pointerX: Int,
    pointerY: Int
  ): TitleBarDragDecision =
    if maximized then TitleBarDragDecision(restoreFirst = true, moveDelta = None)
    else TitleBarDragDecision(restoreFirst = false, moveDelta = Some((pointerX - anchorX, pointerY - anchorY)))
