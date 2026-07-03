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

class SwingWindow(
    initialPixelSize: Dimension,
    initialMetrics: CellMetrics,
    chromeMode: WindowChromeMode = WindowChromeMode.Native,
    initialChromeMetrics: CellMetrics
):

  private val ColBar        = new Color(0x2b2b2b)
  private val ColBarFg      = new Color(0xcccccc)
  private val ColBtnHover   = new Color(0x3f3f3f)
  private val ColCloseHover = new Color(0xc42b1c)

  private val initialChromeLayoutMetrics = SwingWindow.ChromeMetrics.fromCellMetrics(initialChromeMetrics)
  private val initialCanvasPixelSize =
    SwingWindow.canvasFallbackSize(initialPixelSize, chromeMode, initialChromeLayoutMetrics)
  private val pixelSize           = new AtomicReference(initialCanvasPixelSize)
  private val metricsRef          = new AtomicReference(initialMetrics)
  private val chromeMetricsRef    = new AtomicReference(initialChromeLayoutMetrics)
  private val pendingResize       = new AtomicReference[Option[ViewportSize]](None)
  private val closeLatch          = new CountDownLatch(1)
  private val renderedImageRef    = new AtomicReference[Option[BufferedImage]](None)
  private val savedBoundsRef      = new AtomicReference[Option[Rectangle]](None)
  private val maximizedRef        = new AtomicBoolean(false)
  private val maxBtnRef           = new AtomicReference[Option[JLabel]](None)
  private val controlButtonsRef   = new AtomicReference[scala.List[JLabel]](Nil)
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
    renderedImageRef.set(Some(image))
    SwingUtilities.invokeLater(() => canvas.repaint())

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

  private def makeCtrlBtn(label: String, isClose: Boolean): JLabel =
    new JLabel(label, SwingConstants.CENTER):
      setOpaque(true)
      setBackground(ColBar)
      setForeground(ColBarFg)
      setFont(chromeControlFont)
      setPreferredSize(chromeButtonSize)
      addMouseListener(new MouseAdapter:
        override def mouseEntered(e: MouseEvent): Unit =
          setBackground(if isClose then ColCloseHover else ColBtnHover)
        override def mouseExited(e: MouseEvent): Unit =
          setBackground(ColBar)
        override def mouseReleased(e: MouseEvent): Unit =
          if e.getX >= 0 && e.getX < getWidth && e.getY >= 0 && e.getY < getHeight then
            if isClose then closeLatch.countDown()
            else if label == "─" then frame.setExtendedState(Frame.ICONIFIED)
            else toggleMaximize())

  private val titleBar: JPanel =
    val minBtn = makeCtrlBtn("─", isClose = false)
    val maxBtn = makeCtrlBtn("□", isClose = false)
    maxBtnRef.set(Some(maxBtn))
    val closeBtn = makeCtrlBtn("✕", isClose = true)
    controlButtonsRef.set(scala.List(minBtn, maxBtn, closeBtn))

    val btnPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 0, 0)):
      setBackground(ColBar)
    btnPanel.add(minBtn)
    btnPanel.add(maxBtn)
    btnPanel.add(closeBtn)

    val spacer = new JPanel:
      setBackground(ColBar)
      setPreferredSize(chromeSpacerSize)
    titleSpacerRef.set(Some(spacer))

    val titleLabel = new JLabel("Serenity", SwingConstants.CENTER):
      setForeground(ColBarFg)
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
      setBackground(ColBar)
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
          maxBtnRef.get().foreach(_.setText(if isMax then "❐" else "□"))
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
      controlButtonsRef.get().foreach { button =>
        button.setFont(controlFont)
        button.setPreferredSize(chromeButtonSize)
      }
      titleLabelRef.get().foreach(_.setFont(controlFont))
      titleSpacerRef.get().foreach(_.setPreferredSize(chromeSpacerSize))
      titleBarRef.get().foreach(_.setPreferredSize(chromeTitleBarSize))
      frame.setMinimumSize(new Dimension(chrome.minWidth, chrome.minHeight))
      updateShape()
      frame.revalidate()
    else frame.setMinimumSize(new Dimension(SwingWindow.BaseMinWidth, SwingWindow.BaseMinHeight))

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
