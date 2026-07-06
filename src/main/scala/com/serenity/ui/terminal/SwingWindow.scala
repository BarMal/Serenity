package com.serenity.ui.terminal

import java.awt.*
import java.awt.event.*
import java.awt.geom.RoundRectangle2D
import java.awt.image.BufferedImage
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import javax.swing.*

import cats.effect.{IO, Resource}
import com.serenity.config.{PreferredWindowSize, WindowChromeMode}
import com.serenity.ui.display.DisplayScale
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.theme.Theme

class SwingWindow(
    initialPixelSize: Dimension,
    initialMetrics: CellMetrics,
    chromeMode: WindowChromeMode = WindowChromeMode.Native,
    initialChromeMetrics: CellMetrics
):

  private val initialChromeLayoutMetrics = SwingWindow.ChromeMetrics.fromCellMetrics(initialChromeMetrics)
  private val initialCanvasPixelSize =
    SwingWindow.canvasFallbackSize(initialPixelSize, chromeMode, initialChromeLayoutMetrics)
  private val pixelSize           = new AtomicReference(initialCanvasPixelSize)
  private val metricsRef          = new AtomicReference(initialMetrics)
  private val chromeMetricsRef    = new AtomicReference(initialChromeLayoutMetrics)
  private val chromePaletteRef    = new AtomicReference(SwingWindow.ChromePalette.fromTheme(Theme.default))
  private val pendingResize       = new AtomicReference[Option[ViewportSize]](None)
  private val closeLatch          = new CountDownLatch(1)
  private val baseImageRef        = new AtomicReference[Option[BufferedImage]](None)
  private val renderedImageRef    = new AtomicReference[Option[BufferedImage]](None)
  private val savedBoundsRef      = new AtomicReference[Option[Rectangle]](None)
  private val maximizedRef        = new AtomicBoolean(false)
  private val maxBtnRef           = new AtomicReference[Option[ChromeControlButton]](None)
  private val controlButtonsRef   = new AtomicReference[scala.List[ChromeControlButton]](Nil)
  private val controlPanelRef     = new AtomicReference[Option[JPanel]](None)
  private val titleBarRef         = new AtomicReference[Option[JPanel]](None)
  private val titleLabelRef       = new AtomicReference[Option[JLabel]](None)
  private val titleSpacerRef      = new AtomicReference[Option[JPanel]](None)
  private val onResizeCallbackRef = new AtomicReference[Option[() => Unit]](None)
  private val usesCustomChrome    = chromeMode == WindowChromeMode.Custom

  def setOnResize(cb: () => Unit): Unit = onResizeCallbackRef.set(Some(cb))

  val canvas: JPanel = new JPanel:
    setBackground(Color.BLACK)
    setPreferredSize(initialCanvasPixelSize)
    setFocusable(true)
    setFocusTraversalKeysEnabled(false)
    addComponentListener(
      new ComponentAdapter:
        override def componentResized(e: ComponentEvent): Unit =
          publishCanvasResize(getSize())
    )
    override def paintComponent(g: java.awt.Graphics): Unit =
      g.setColor(Color.BLACK)
      g.fillRect(0, 0, getWidth, getHeight)
      renderedImageRef.get().foreach(img => g.drawImage(img, 0, 0, getWidth, getHeight, null))

  def onImageReady(image: BufferedImage): Unit =
    baseImageRef.set(Some(image))
    renderedImageRef.set(Some(image))
    SwingUtilities.invokeLater(() => canvas.repaint())

  def onCursorOverlayReady(drawOverlay: BufferedImage => Unit): Boolean =
    baseImageRef.get() match
      case Some(baseImage) =>
        val overlayImage = SwingWindow.copyImage(baseImage)
        drawOverlay(overlayImage)
        renderedImageRef.set(Some(overlayImage))
        SwingUtilities.invokeLater(() => canvas.repaint())
        true
      case None =>
        false

  private def updateShape(): Unit =
    if usesCustomChrome && !maximizedRef.get() then
      val d      = frame.getSize
      val chrome = chromeMetricsRef.get()
      frame.setShape(new RoundRectangle2D.Double(0, 0, d.width, d.height, chrome.cornerArc, chrome.cornerArc))
    else if usesCustomChrome then frame.setShape(null)

  private def toggleMaximize(): Unit =
    if maximizedRef.get() then
      frame.setExtendedState(Frame.NORMAL)
      savedBoundsRef.get().foreach(frame.setBounds)
    else
      savedBoundsRef.set(Some(frame.getBounds))
      frame.setExtendedState(Frame.MAXIMIZED_BOTH)

  private class ChromeControlButton(initialKind: SwingWindow.ChromeControlKind) extends JComponent:
    private val kindRef    = new AtomicReference(initialKind)
    private val hoverRef   = new AtomicBoolean(false)
    private val pressedRef = new AtomicBoolean(false)

    setOpaque(true)
    setFocusable(true)
    setPreferredSize(chromeButtonSize)
    SwingWindow.setAccessibleNameIfAvailable(this, initialKind.accessibleName)

    def setKind(kind: SwingWindow.ChromeControlKind): Unit =
      kindRef.set(kind)
      SwingWindow.setAccessibleNameIfAvailable(this, kind.accessibleName)
      repaint()

    override def paintComponent(g: Graphics): Unit =
      val palette = chromePaletteRef.get()
      val state = SwingWindow.ChromeControlState(
        hovered = hoverRef.get(),
        pressed = pressedRef.get(),
        focused = hasFocus
      )
      val kind       = kindRef.get()
      val background = SwingWindow.ChromeControlPaint.background(kind, palette, state)
      val foreground = SwingWindow.ChromeControlPaint.foreground(kind, palette, state)
      val g2         = g.create().asInstanceOf[Graphics2D]
      try
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        g2.setColor(background)
        g2.fillRect(0, 0, getWidth, getHeight)
        g2.setColor(foreground)
        g2.setStroke(new BasicStroke(SwingWindow.ChromeIconGeometry.strokeWidth(getHeight).toFloat))
        SwingWindow.ChromeIconGeometry.lines(kind, getWidth, getHeight).foreach { line =>
          g2.drawLine(line.x1, line.y1, line.x2, line.y2)
        }
        SwingWindow.ChromeControlPaint.focusBorder(palette, state).foreach { border =>
          g2.setColor(border)
          g2.drawRect(1, 1, getWidth - 3, getHeight - 3)
        }
      finally g2.dispose()

    addMouseListener(new MouseAdapter:
      override def mouseEntered(e: MouseEvent): Unit =
        hoverRef.set(true)
        repaint()
      override def mouseExited(e: MouseEvent): Unit =
        hoverRef.set(false)
        repaint()
      override def mousePressed(e: MouseEvent): Unit =
        pressedRef.set(true)
        repaint()
      override def mouseReleased(e: MouseEvent): Unit =
        val wasPressed = pressedRef.getAndSet(false)
        repaint()
        if wasPressed && e.getX >= 0 && e.getX < getWidth && e.getY >= 0 && e.getY < getHeight then activate())

    addKeyListener(
      new KeyAdapter:
        override def keyPressed(e: KeyEvent): Unit =
          if e.getKeyCode == KeyEvent.VK_ENTER || e.getKeyCode == KeyEvent.VK_SPACE then
            pressedRef.set(true)
            repaint()
        override def keyReleased(e: KeyEvent): Unit =
          if e.getKeyCode == KeyEvent.VK_ENTER || e.getKeyCode == KeyEvent.VK_SPACE then
            val wasPressed = pressedRef.getAndSet(false)
            repaint()
            if wasPressed then activate()
    )

    private def activate(): Unit =
      kindRef.get() match
        case SwingWindow.ChromeControlKind.Minimize =>
          frame.setExtendedState(Frame.ICONIFIED)
        case SwingWindow.ChromeControlKind.Maximize | SwingWindow.ChromeControlKind.Restore =>
          toggleMaximize()
        case SwingWindow.ChromeControlKind.Close =>
          closeLatch.countDown()

  private def makeCtrlBtn(kind: SwingWindow.ChromeControlKind): ChromeControlButton =
    new ChromeControlButton(kind)

  private val titleBar: JPanel =
    val minBtn = makeCtrlBtn(SwingWindow.ChromeControlKind.Minimize)
    val maxBtn = makeCtrlBtn(SwingWindow.ChromeControlKind.Maximize)
    maxBtnRef.set(Some(maxBtn))
    val closeBtn = makeCtrlBtn(SwingWindow.ChromeControlKind.Close)
    controlButtonsRef.set(scala.List(minBtn, maxBtn, closeBtn))

    val btnPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 0, 0)):
      setBackground(chromePaletteRef.get().titleBackground)
    controlPanelRef.set(Some(btnPanel))
    btnPanel.add(minBtn)
    btnPanel.add(maxBtn)
    btnPanel.add(closeBtn)

    val spacer = new JPanel:
      setBackground(chromePaletteRef.get().titleBackground)
      setPreferredSize(chromeSpacerSize)
    titleSpacerRef.set(Some(spacer))

    val titleLabel = new JLabel("Serenity", SwingConstants.CENTER):
      setForeground(chromePaletteRef.get().titleForeground)
      setFont(chromeControlFont)
    titleLabelRef.set(Some(titleLabel))

    val dragAdapter = new MouseAdapter:
      private case class DragAnchor(x: Int, y: Int)
      private val anchorRef = new AtomicReference(DragAnchor(0, 0))

      override def mousePressed(e: MouseEvent): Unit =
        anchorRef.set(DragAnchor(e.getXOnScreen, e.getYOnScreen))

      override def mouseDragged(e: MouseEvent): Unit =
        if !maximizedRef.get() then
          val anchor = anchorRef.get()
          val dx     = e.getXOnScreen - anchor.x
          val dy     = e.getYOnScreen - anchor.y
          val loc    = frame.getLocation
          frame.setLocation(loc.x + dx, loc.y + dy)
          anchorRef.set(DragAnchor(e.getXOnScreen, e.getYOnScreen))

      override def mouseClicked(e: MouseEvent): Unit =
        if e.getClickCount == 2 then toggleMaximize()

    val bar = new JPanel(new BorderLayout):
      setBackground(chromePaletteRef.get().titleBackground)
      setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, chromePaletteRef.get().border))
      setPreferredSize(chromeTitleBarSize)
    titleBarRef.set(Some(bar))
    bar.add(spacer, BorderLayout.WEST)
    bar.add(titleLabel, BorderLayout.CENTER)
    bar.add(btnPanel, BorderLayout.EAST)
    bar.addMouseListener(dragAdapter)
    bar.addMouseMotionListener(dragAdapter)
    titleLabel.addMouseListener(dragAdapter)
    titleLabel.addMouseMotionListener(dragAdapter)
    bar

  private class ResizeGlassPane extends JComponent:
    setOpaque(false)
    setFocusable(false)

    private case class ResizeState(
        resizing: Boolean = false,
        resizeDir: Int = 0,
        pressX: Int = 0,
        pressY: Int = 0,
        pressBounds: Rectangle = new Rectangle()
    )

    private case class BoundsBox(x: Int, y: Int, width: Int, height: Int)
    private val resizeStateRef = new AtomicReference(ResizeState())

    override def contains(x: Int, y: Int): Boolean =
      val margin = chromeMetricsRef.get().margin
      x < margin || x > getWidth - margin || y < margin || y > getHeight - margin

    private def edgeDir(e: MouseEvent): Int =
      val x      = e.getX; val y   = e.getY
      val w      = getWidth; val h = getHeight
      val margin = chromeMetricsRef.get().margin
      scala
        .List(
          Option.when(y < margin)(1),
          Option.when(y > h - margin)(2),
          Option.when(x < margin)(4),
          Option.when(x > w - margin)(8)
        )
        .flatten
        .foldLeft(0)(_ | _)

    private def dirCursor(d: Int): Cursor = d match
      case 1  => Cursor.getPredefinedCursor(Cursor.N_RESIZE_CURSOR)
      case 2  => Cursor.getPredefinedCursor(Cursor.S_RESIZE_CURSOR)
      case 4  => Cursor.getPredefinedCursor(Cursor.W_RESIZE_CURSOR)
      case 8  => Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR)
      case 5  => Cursor.getPredefinedCursor(Cursor.NW_RESIZE_CURSOR)
      case 9  => Cursor.getPredefinedCursor(Cursor.NE_RESIZE_CURSOR)
      case 6  => Cursor.getPredefinedCursor(Cursor.SW_RESIZE_CURSOR)
      case 10 => Cursor.getPredefinedCursor(Cursor.SE_RESIZE_CURSOR)
      case _  => Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)

    private val adapter = new MouseAdapter:
      override def mousePressed(e: MouseEvent): Unit =
        val d = edgeDir(e)
        if d != 0 && !maximizedRef.get() then
          resizeStateRef.set(
            ResizeState(
              resizing = true,
              resizeDir = d,
              pressX = e.getXOnScreen,
              pressY = e.getYOnScreen,
              pressBounds = frame.getBounds
            )
          )

      override def mouseReleased(e: MouseEvent): Unit =
        val _ = resizeStateRef.updateAndGet(_.copy(resizing = false, resizeDir = 0))

      override def mouseMoved(e: MouseEvent): Unit =
        setCursor(dirCursor(edgeDir(e)))

      override def mouseExited(e: MouseEvent): Unit =
        setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR))

      override def mouseDragged(e: MouseEvent): Unit =
        val state = resizeStateRef.get()
        if state.resizing then
          val dx = e.getXOnScreen - state.pressX
          val dy = e.getYOnScreen - state.pressY
          val base = BoundsBox(
            state.pressBounds.x,
            state.pressBounds.y,
            state.pressBounds.width,
            state.pressBounds.height
          )
          val afterNorth =
            if (state.resizeDir & 1) != 0 then
              base.copy(y = state.pressBounds.y + dy, height = state.pressBounds.height - dy)
            else base
          val afterSouth =
            if (state.resizeDir & 2) != 0 then afterNorth.copy(height = state.pressBounds.height + dy)
            else afterNorth
          val afterWest =
            if (state.resizeDir & 4) != 0 then
              afterSouth.copy(x = state.pressBounds.x + dx, width = state.pressBounds.width - dx)
            else afterSouth
          val finalBounds =
            if (state.resizeDir & 8) != 0 then afterWest.copy(width = state.pressBounds.width + dx)
            else afterWest
          val chrome = chromeMetricsRef.get()
          if finalBounds.width >= chrome.minWidth && finalBounds.height >= chrome.minHeight then
            frame.setBounds(finalBounds.x, finalBounds.y, finalBounds.width, finalBounds.height)

    addMouseListener(adapter)
    addMouseMotionListener(adapter)

  private val frame: JFrame =
    val f = new JFrame("Serenity")
    f.setUndecorated(usesCustomChrome)
    f.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    f.addWindowListener(
      new WindowAdapter:
        override def windowClosing(e: WindowEvent): Unit = closeLatch.countDown()
    )
    f.addWindowStateListener((e: WindowEvent) =>
      val isMax = (e.getNewState & Frame.MAXIMIZED_BOTH) == Frame.MAXIMIZED_BOTH
      maximizedRef.set(isMax)
      SwingUtilities.invokeLater { () =>
        if usesCustomChrome then
          maxBtnRef
            .get()
            .foreach(_.setKind {
              if isMax then SwingWindow.ChromeControlKind.Restore else SwingWindow.ChromeControlKind.Maximize
            })
          updateShape()
      }
    )
    f.addComponentListener(
      new ComponentAdapter:
        override def componentResized(e: ComponentEvent): Unit =
          if usesCustomChrome then SwingUtilities.invokeLater(() => updateShape())
    )
    val content = new JPanel(new BorderLayout):
      setBackground(Color.BLACK)
    if usesCustomChrome then content.add(titleBar, BorderLayout.NORTH)
    content.add(canvas, BorderLayout.CENTER)
    f.setContentPane(content)
    if usesCustomChrome then
      val glassPane = new ResizeGlassPane
      f.setGlassPane(glassPane)
      glassPane.setVisible(true)
    f.pack()
    if usesCustomChrome then
      val chrome = chromeMetricsRef.get()
      f.setMinimumSize(new Dimension(chrome.minWidth, chrome.minHeight))
    else f.setMinimumSize(new Dimension(SwingWindow.BaseMinWidth, SwingWindow.BaseMinHeight))
    f.setLocationRelativeTo(null)
    f

  def awaitClose: IO[Unit] = IO.blocking(closeLatch.await())

  def start(): Unit =
    val showWindow: Runnable = () =>
      frame.setVisible(true)
      if usesCustomChrome then updateShape()
      publishCanvasResize(canvas.getSize())
      val _ = canvas.requestFocusInWindow()
    if SwingUtilities.isEventDispatchThread then showWindow.run()
    else SwingUtilities.invokeAndWait(showWindow)

  def stop(): Unit =
    val dispose: Runnable = () =>
      frame.setVisible(false)
      frame.dispose()
    if SwingUtilities.isEventDispatchThread then dispose.run()
    else SwingUtilities.invokeAndWait(dispose)

  def viewportSize: ViewportSize =
    val d = pixelSize.get()
    metrics.viewportSize(d.width, d.height)

  def currentPreferredWindowSize: PreferredWindowSize =
    val d = frame.getSize
    PreferredWindowSize(d.width, d.height).normalized

  def resizeToPreferred(size: PreferredWindowSize): Unit =
    val normalized = size.normalized
    SwingUtilities.invokeLater { () =>
      val dimension      = new Dimension(normalized.width, normalized.height)
      val canvasFallback = SwingWindow.canvasFallbackSize(dimension, chromeMode, chromeMetricsRef.get())
      canvas.setPreferredSize(canvasFallback)
      frame.setSize(dimension)
      frame.validate()
      frame.setLocationRelativeTo(null)
      publishCanvasResize(canvas.getSize(), canvasFallback)
      val _ = canvas.requestFocusInWindow()
    }

  def metrics: CellMetrics =
    metricsRef.get()

  def updateMetrics(newMetrics: CellMetrics): Unit =
    updateMetrics(newMetrics, newMetrics)

  def updateMetrics(newMetrics: CellMetrics, newChromeMetrics: CellMetrics): Unit =
    metricsRef.set(newMetrics)
    applyChromeMetrics(newChromeMetrics)
    val d = pixelSize.get()
    pendingResize.set(Some(newMetrics.viewportSize(d.width, d.height)))
    onResizeCallbackRef.get().foreach(_.apply())

  def updateChromeTheme(theme: Theme): Unit =
    if usesCustomChrome then
      val palette = SwingWindow.ChromePalette.fromTheme(theme)
      chromePaletteRef.set(palette)
      val applyPalette: Runnable = () => applyChromePalette(palette)
      if SwingUtilities.isEventDispatchThread then applyPalette.run()
      else SwingUtilities.invokeLater(applyPalette)

  def detectedDeviceTextScale: Double =
    DisplayScale.forComponent(canvas).textScale

  private def chromeControlFont: Font =
    new Font(Font.SANS_SERIF, Font.PLAIN, chromeMetricsRef.get().titleFontSize)

  private def chromeButtonSize: Dimension =
    val chrome = chromeMetricsRef.get()
    new Dimension(chrome.buttonWidth, chrome.titleBarHeight)

  private def chromeSpacerSize: Dimension =
    val chrome = chromeMetricsRef.get()
    new Dimension(3 * chrome.buttonWidth, chrome.titleBarHeight)

  private def chromeTitleBarSize: Dimension =
    new Dimension(0, chromeMetricsRef.get().titleBarHeight)

  private def applyChromeMetrics(metrics: CellMetrics): Unit =
    if usesCustomChrome then
      val chrome = SwingWindow.ChromeMetrics.fromCellMetrics(metrics)
      chromeMetricsRef.set(chrome)
      val controlFont = chromeControlFont
      controlButtonsRef.get().foreach(button => button.setPreferredSize(chromeButtonSize))
      titleLabelRef.get().foreach(_.setFont(controlFont))
      titleSpacerRef.get().foreach(_.setPreferredSize(chromeSpacerSize))
      titleBarRef.get().foreach(_.setPreferredSize(chromeTitleBarSize))
      frame.setMinimumSize(new Dimension(chrome.minWidth, chrome.minHeight))
      updateShape()
      frame.revalidate()
    else frame.setMinimumSize(new Dimension(SwingWindow.BaseMinWidth, SwingWindow.BaseMinHeight))

  private def applyChromePalette(palette: SwingWindow.ChromePalette): Unit =
    controlButtonsRef.get().foreach(_.repaint())
    controlPanelRef.get().foreach(_.setBackground(palette.titleBackground))
    titleSpacerRef.get().foreach(_.setBackground(palette.titleBackground))
    titleLabelRef.get().foreach(_.setForeground(palette.titleForeground))
    titleBarRef.get().foreach { titleBar =>
      titleBar.setBackground(palette.titleBackground)
      titleBar.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, palette.border))
      titleBar.repaint()
    }

  private def publishCanvasResize(canvasSize: Dimension): Unit =
    publishCanvasResize(canvasSize, pixelSize.get())

  private def publishCanvasResize(canvasSize: Dimension, fallbackSize: Dimension): Unit =
    val snapshot = SwingWindow.canvasResizeSnapshot(metrics, canvasSize, fallbackSize)
    pixelSize.set(snapshot.pixelSize)
    pendingResize.set(Some(snapshot.viewportSize))
    onResizeCallbackRef.get().foreach(_.apply())

  def doResizeIfNecessary(): Option[ViewportSize] =
    pendingResize.getAndSet(None)

object SwingWindow:
  val DefaultMetrics: CellMetrics = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 13)
  val BaseMinWidth: Int           = 400
  val BaseMinHeight: Int          = 300

  case class ChromeMetrics(
      titleBarHeight: Int,
      buttonWidth: Int,
      margin: Int,
      cornerArc: Int,
      minWidth: Int,
      minHeight: Int,
      titleFontSize: Int
  )

  case class CanvasResizeSnapshot(pixelSize: Dimension, viewportSize: ViewportSize)

  def copyImage(source: BufferedImage): BufferedImage =
    val copy = new BufferedImage(source.getWidth, source.getHeight, source.getType)
    val g    = copy.createGraphics()
    try g.drawImage(source, 0, 0, null)
    finally g.dispose()
    copy

  private[serenity] def setAccessibleNameIfAvailable(component: JComponent, name: String): Unit =
    Option(component.getAccessibleContext).foreach(_.setAccessibleName(name))

  enum ChromeControlKind(val accessibleName: String):
    case Minimize extends ChromeControlKind("Minimize")
    case Maximize extends ChromeControlKind("Maximize")
    case Restore  extends ChromeControlKind("Restore")
    case Close    extends ChromeControlKind("Close")

  case class ChromeIconLine(x1: Int, y1: Int, x2: Int, y2: Int)

  case class ChromeControlState(
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

  case class ChromePalette(
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

  object ChromeMetrics:
    private val BaseTitleBarHeight = 32
    private val BaseButtonWidth    = 46
    private val BaseMargin         = 6
    private val BaseCornerArc      = 12
    private val BaseTitleFontSize  = 13

    def fromCellMetrics(metrics: CellMetrics): ChromeMetrics =
      val scale = (metrics.lineHeight.toDouble / DefaultMetrics.lineHeight.toDouble).max(1.0)
      ChromeMetrics(
        titleBarHeight = scaledInt(BaseTitleBarHeight, scale),
        buttonWidth = scaledInt(BaseButtonWidth, scale),
        margin = scaledInt(BaseMargin, scale),
        cornerArc = scaledInt(BaseCornerArc, scale),
        minWidth = scaledInt(BaseMinWidth, scale),
        minHeight = scaledInt(BaseMinHeight, scale),
        titleFontSize = scaledInt(BaseTitleFontSize, scale)
      )

    private def scaledInt(value: Int, scale: Double): Int =
      math.round(value.toDouble * scale).toInt.max(1)

  def canvasResizeSnapshot(
    metrics: CellMetrics,
    canvasSize: Dimension,
    fallbackSize: Dimension
  ): CanvasResizeSnapshot =
    val size =
      if canvasSize.width > 0 && canvasSize.height > 0 then new Dimension(canvasSize)
      else new Dimension(fallbackSize)
    CanvasResizeSnapshot(size, metrics.viewportSize(size.width, size.height))

  def canvasFallbackSize(
    windowSize: Dimension,
    chromeMode: WindowChromeMode,
    chromeMetrics: ChromeMetrics
  ): Dimension =
    val chromeHeight =
      chromeMode match
        case WindowChromeMode.Custom => chromeMetrics.titleBarHeight
        case WindowChromeMode.Native => 0
    new Dimension(windowSize.width.max(1), (windowSize.height - chromeHeight).max(1))

  def resource(
    metrics: CellMetrics = DefaultMetrics,
    chromeMetrics: CellMetrics = DefaultMetrics,
    chromeMode: WindowChromeMode = WindowChromeMode.Native,
    preferredWindowSize: Option[PreferredWindowSize] = None
  ): Resource[IO, SwingWindow] =
    Resource.make(
      IO.blocking {
        val initialSize = preferredWindowSize.map(_.normalized).getOrElse(PreferredWindowSize(1024, 768))
        val win = new SwingWindow(
          new Dimension(initialSize.width, initialSize.height),
          metrics,
          chromeMode,
          chromeMetrics
        )
        win.start()
        win
      }
    )(win => IO.blocking(win.stop()))
