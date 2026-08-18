package com.serenity.ui.terminal

import java.awt.*
import java.awt.event.*
import java.awt.geom.RoundRectangle2D
import java.awt.image.BufferedImage
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import javax.imageio.ImageIO
import javax.swing.*

import scala.jdk.CollectionConverters.*

import cats.effect.{IO, Resource}
import com.serenity.animation.WindowSitter
import com.serenity.config.{PreferredWindowSize, WindowChromeMode}
import com.serenity.ui.accessibility.{AccessibilitySnapshot, SwingAccessibilityBridge}
import com.serenity.ui.display.DisplayScale
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.theme.Theme

class SwingWindow(
    initialPixelSize: Dimension,
    initialMetrics: CellMetrics,
    chromeMode: WindowChromeMode = WindowChromeMode.Auto,
    initialChromeMetrics: CellMetrics,
    initialWindowSitter: WindowSitter = WindowSitter.default,
    initialWindowSitterVisible: Boolean = true
):

  private val usesCustomChrome           = SwingWindow.shouldUseCustomChrome(chromeMode)
  private val effectiveChromeMode        = if usesCustomChrome then WindowChromeMode.Custom else chromeMode
  private val usesNativeThemedChrome     = chromeMode == WindowChromeMode.NativeThemed
  private val initialChromeLayoutMetrics = SwingWindow.ChromeMetrics.fromCellMetrics(initialChromeMetrics)

  private val initialCanvasResizeSnapshot =
    SwingWindow.fallbackCanvasResizeSnapshot(
      initialMetrics,
      initialPixelSize,
      effectiveChromeMode,
      initialChromeLayoutMetrics
    )

  private val initialCanvasPixelSize = initialCanvasResizeSnapshot.pixelSize
  private val pixelSize              = new AtomicReference(initialCanvasPixelSize)
  private val metricsRef             = new AtomicReference(initialMetrics)
  private val chromeMetricsRef       = new AtomicReference(initialChromeLayoutMetrics)
  private val chromePaletteRef       = new AtomicReference(SwingWindow.ChromePalette.fromTheme(Theme.default))
  private val nativeChromeThemeCache = new SwingWindow.NativeChromeThemeCache
  private val pendingResize          = new AtomicReference[Option[ViewportSize]](None)
  private val closeLatch             = new CountDownLatch(1)
  private val baseImageRef           = new AtomicReference[Option[BufferedImage]](None)
  private val publishedImagesRef     = new AtomicReference(SwingWindow.PublishedImages.empty)
  private val previousCursorRectsRef = new AtomicReference[scala.List[Rectangle]](Nil)
  private val baseImagePool          = new SwingWindow.ReusableImagePool
  private val cursorOverlayPool      = new SwingWindow.ReusableImagePool
  private val savedBoundsRef         = new AtomicReference[Option[Rectangle]](None)
  private val maximizedRef           = new AtomicBoolean(false)
  private val maxBtnRef              = new AtomicReference[Option[ChromeControlButton]](None)
  private val controlButtonsRef      = new AtomicReference[scala.List[ChromeControlButton]](Nil)
  private val controlPanelRef        = new AtomicReference[Option[JPanel]](None)
  private val titleBarRef            = new AtomicReference[Option[JPanel]](None)
  private val titleLabelRef          = new AtomicReference[Option[SwingWindow.DecorativeTitleLabel]](None)
  private val titleSpacerRef         = new AtomicReference[Option[JPanel]](None)
  private val onResizeCallbackRef    = new AtomicReference[Option[() => Unit]](None)
  private val resizeGlassPaneRef     = new AtomicReference[Option[JComponent]](None)
  private val roundedCornerMaskRef   = new AtomicReference[Option[Int]](None)
  private val roundedContentBuffers  = new SwingWindow.RoundedCornerMaskBufferCache
  private val perPixelTranslucencySupported =
    SwingWindow.perPixelTranslucencySupported
  private val shapeUpdateCoalescer = new SwingWindow.CoalescedEdtUpdate(() => updateShape())

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
      val published = publishedImagesRef.get()
      published.base.foreach(img => g.drawImage(img, 0, 0, getWidth, getHeight, null))
      published.overlay.foreach(img => g.drawImage(img, 0, 0, getWidth, getHeight, null))

  private val accessibilityBridge = new SwingAccessibilityBridge(canvas)

  /** Publish the semantic projection of the custom-painted canvas to Swing accessibility clients. */
  def updateAccessibility(snapshot: AccessibilitySnapshot): Unit =
    val publish: Runnable = () => accessibilityBridge.publish(snapshot, metrics)
    if SwingUtilities.isEventDispatchThread then publish.run()
    else SwingUtilities.invokeLater(publish)

  def onImageReady(image: BufferedImage): Unit =
    onImageReady(image, None)

  /** Publish a finished base frame, repainting only `dirtyRegion` when the rest of the frame is known to be identical
    * to what is already on screen.
    *
    * The bounded repaint is dropped whenever a cursor overlay was part of the displayed frame: dropping that overlay
    * changes pixels outside the region, and stale caret pixels would survive a partial repaint.
    */
  def onImageReady(image: BufferedImage, dirtyRegion: Option[Rectangle]): Unit =
    val displayedOverlay = publishedImagesRef.get().overlay
    baseImagePool.publish(image)
    baseImageRef.set(Some(image))
    cursorOverlayPool.clearPublished()
    publishedImagesRef.set(SwingWindow.PublishedImages(Some(image), None))
    dirtyRegion.filter(_ => displayedOverlay.isEmpty) match
      case Some(region) if region.width > 0 && region.height > 0 =>
        SwingUtilities.invokeLater(() => canvas.repaint(region.x, region.y, region.width, region.height))
      case Some(_) => ()
      case None    => SwingUtilities.invokeLater(() => canvas.repaint())

  def onBaseImageReady(image: BufferedImage): Unit =
    baseImagePool.publish(image)
    baseImageRef.set(Some(image))
    cursorOverlayPool.clearPublished()
    publishedImagesRef.set(SwingWindow.PublishedImages(Some(image), None))

  /** Publish a freshly-painted cursor overlay and repaint just the pixels it actually changed.
    *
    * The overlay image is cleared and redrawn from scratch every call, so a caret that moved needs both its old and new
    * position repainted -- not just whatever the base frame changed. `baseDirtyRegion` is the caller's own
    * bounded-repaint region for the base frame (`None` for "the whole canvas changed"); `drawOverlay` paints the
    * overlay and reports back the pixel rects it painted. The final repaint is bounded to the union of all three, or
    * unbounded whenever `baseDirtyRegion` itself is `None`.
    */
  def onCursorOverlayReady(baseDirtyRegion: Option[Rectangle])(
    drawOverlay: BufferedImage => scala.List[Rectangle]
  ): Boolean =
    baseImageRef.get() match
      case Some(baseImage) =>
        val overlayImage = cursorOverlayPool.acquire(baseImage.getWidth, baseImage.getHeight, baseImage.getType)
        SwingWindow.clearImage(overlayImage)
        val currentCursorRects = drawOverlay(overlayImage)
        cursorOverlayPool.publish(overlayImage)
        publishedImagesRef.set(SwingWindow.PublishedImages(Some(baseImage), Some(overlayImage)))
        val previousCursorRects = previousCursorRectsRef.getAndSet(currentCursorRects)
        SwingWindow.combinedCursorRepaintRegion(baseDirtyRegion, previousCursorRects, currentCursorRects) match
          case Some(region) if region.width > 0 && region.height > 0 =>
            SwingUtilities.invokeLater(() => canvas.repaint(region.x, region.y, region.width, region.height))
          case Some(_) => ()
          case None    => SwingUtilities.invokeLater(() => canvas.repaint())
        true
      case None =>
        false

  private[serenity] def acquireBaseImage(width: Int, height: Int, imageType: Int): BufferedImage =
    baseImagePool.acquire(width, height, imageType)

  private def updateShape(): Unit =
    val roundedCornerMask = SwingWindow.roundedCornerMask(
      usesCustomChrome,
      maximizedRef.get(),
      perPixelTranslucencySupported,
      chromeMetricsRef.get().cornerArc
    )
    val refreshRoundedCornerMask =
      SwingWindow.shouldRefreshRoundedCornerMask(roundedCornerMaskRef.get(), roundedCornerMask)
    roundedCornerMaskRef.set(roundedCornerMask)

    if roundedCornerMask.nonEmpty
    then frame.setShape(null)
    else if usesCustomChrome && !maximizedRef.get() then
      val d      = frame.getSize
      val chrome = chromeMetricsRef.get()
      frame.setShape(new RoundRectangle2D.Double(0, 0, d.width, d.height, chrome.cornerArc, chrome.cornerArc))
    else if usesCustomChrome then frame.setShape(null)

    if refreshRoundedCornerMask then resizeGlassPaneRef.get().foreach(_.repaint())

  private def scheduleShapeUpdate(): Unit =
    if usesCustomChrome then shapeUpdateCoalescer.schedule(SwingUtilities.invokeLater)

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
    val controlLayout = SwingWindow.ChromeControlLayout.current
    val buttonPairs   = controlLayout.controls.map(kind => kind -> makeCtrlBtn(kind))
    maxBtnRef.set(buttonPairs.collectFirst { case (SwingWindow.ChromeControlKind.Maximize, button) => button })
    controlButtonsRef.set(buttonPairs.map(_._2))

    val btnPanel = new JPanel(new FlowLayout(controlLayout.flowAlignment, 0, 0)):
      setBackground(chromePaletteRef.get().titleBackground)
    controlPanelRef.set(Some(btnPanel))
    buttonPairs.foreach((_, button) => btnPanel.add(button))

    val spacer = new JPanel:
      setBackground(chromePaletteRef.get().titleBackground)
      setPreferredSize(chromeSpacerSize)
    titleSpacerRef.set(Some(spacer))

    val titleLabel = new SwingWindow.DecorativeTitleLabel(initialWindowSitter.glyph, initialWindowSitterVisible)
    titleLabel.setForeground(chromePaletteRef.get().titleForeground)
    titleLabel.setFont(chromeControlFont)
    titleLabelRef.set(Some(titleLabel))

    val dragAdapter = new MouseAdapter:
      final private case class DragAnchor(x: Int, y: Int)
      private val anchorRef = new AtomicReference(DragAnchor(0, 0))

      override def mousePressed(e: MouseEvent): Unit =
        anchorRef.set(DragAnchor(e.getXOnScreen, e.getYOnScreen))

      override def mouseDragged(e: MouseEvent): Unit =
        val anchor = anchorRef.get()
        val decision = SwingWindow.titleBarDragDecision(
          maximized = maximizedRef.get(),
          anchorX = anchor.x,
          anchorY = anchor.y,
          pointerX = e.getXOnScreen,
          pointerY = e.getYOnScreen
        )
        if decision.restoreFirst then
          frame.setExtendedState(frame.getExtendedState & ~Frame.MAXIMIZED_BOTH)
          maximizedRef.set(false)
          anchorRef.set(DragAnchor(e.getXOnScreen, e.getYOnScreen))
        else
          decision.moveDelta.foreach {
            case (dx, dy) =>
              val loc = frame.getLocation
              frame.setLocation(loc.x + dx, loc.y + dy)
              anchorRef.set(DragAnchor(e.getXOnScreen, e.getYOnScreen))
          }

      override def mouseClicked(e: MouseEvent): Unit =
        if e.getClickCount == 2 then toggleMaximize()

    val bar = new JPanel(new BorderLayout):
      setBackground(chromePaletteRef.get().titleBackground)
      setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, chromePaletteRef.get().border))
      setPreferredSize(chromeTitleBarSize)
    titleBarRef.set(Some(bar))
    controlLayout.placement match
      case SwingWindow.ChromeControlPlacement.Left =>
        bar.add(btnPanel, BorderLayout.WEST)
        bar.add(spacer, BorderLayout.EAST)
      case SwingWindow.ChromeControlPlacement.Right =>
        bar.add(spacer, BorderLayout.WEST)
        bar.add(btnPanel, BorderLayout.EAST)
    bar.add(titleLabel, BorderLayout.CENTER)
    bar.addMouseListener(dragAdapter)
    bar.addMouseMotionListener(dragAdapter)
    titleLabel.addMouseListener(dragAdapter)
    titleLabel.addMouseMotionListener(dragAdapter)
    bar

  private class ResizeGlassPane extends JComponent:
    setOpaque(false)
    setFocusable(false)

    final private case class ResizeState(
        resizing: Boolean = false,
        resizeDir: Int = 0,
        pressX: Int = 0,
        pressY: Int = 0,
        pressBounds: Rectangle = new Rectangle()
    )

    final private case class BoundsBox(x: Int, y: Int, width: Int, height: Int)
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

  private class RoundedContentPane(layout: LayoutManager) extends JPanel(layout):
    setOpaque(false)

    override def paint(g: Graphics): Unit =
      if SwingWindow.shouldUsePerPixelRoundedCorners(
            usesCustomChrome,
            maximizedRef.get(),
            perPixelTranslucencySupported
          ) && getWidth > 0 && getHeight > 0
      then
        val buffers = roundedContentBuffers.acquire(getWidth, getHeight, chromeMetricsRef.get().cornerArc)
        val _       = g.drawImage(buffers.render(contentsGraphics => super.paint(contentsGraphics)), 0, 0, null)
      else super.paint(g)

  private val frame: JFrame =
    val f = new JFrame(SwingWindow.WindowTitle)
    f.setIconImages(SwingWindow.applicationIconImages.asJava)
    f.setUndecorated(usesCustomChrome)
    if usesCustomChrome && perPixelTranslucencySupported then f.setBackground(SwingWindow.Transparent)
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
          scheduleShapeUpdate()
    )
    val content = new RoundedContentPane(new BorderLayout):
      setBackground(Color.BLACK)
    if usesCustomChrome then content.add(titleBar, BorderLayout.NORTH)
    content.add(canvas, BorderLayout.CENTER)
    f.setContentPane(content)
    if usesCustomChrome then
      val glassPane = new ResizeGlassPane
      resizeGlassPaneRef.set(Some(glassPane))
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
      if usesNativeThemedChrome then updateNativeChromeTheme(chromePaletteRef.get())
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
      val dimension = new Dimension(normalized.width, normalized.height)
      val canvasFallback =
        SwingWindow
          .fallbackCanvasResizeSnapshot(metrics, dimension, effectiveChromeMode, chromeMetricsRef.get())
          .pixelSize
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
    val snapshot = SwingWindow.fontMetricsUpdateSnapshot(newMetrics, canvas.getSize(), pixelSize.get())
    pixelSize.set(snapshot.pixelSize)
    pendingResize.set(Some(snapshot.viewportSize))
    onResizeCallbackRef.get().foreach(_.apply())

  def updateChromeTheme(theme: Theme): Unit =
    if usesCustomChrome then
      val palette = SwingWindow.ChromePalette.fromTheme(theme)
      chromePaletteRef.set(palette)
      val applyPalette: Runnable = () => applyChromePalette(palette)
      if SwingUtilities.isEventDispatchThread then applyPalette.run()
      else SwingUtilities.invokeLater(applyPalette)
    else if usesNativeThemedChrome then updateNativeChromeTheme(SwingWindow.ChromePalette.fromTheme(theme))

  /** Update the decorative sitter without changing the window's title-bar interactions. */
  def updateWindowSitter(sitter: WindowSitter, visible: Boolean): Unit =
    val update: Runnable = () => titleLabelRef.get().foreach(_.updateDecoration(sitter.glyph, visible))
    if SwingUtilities.isEventDispatchThread then update.run()
    else SwingUtilities.invokeLater(update)

  private def updateNativeChromeTheme(palette: SwingWindow.ChromePalette): Unit =
    if nativeChromeThemeCache.recordIfChanged(palette, WindowsNativeChrome.isSupported()) then
      chromePaletteRef.set(palette)
      val applyPalette: Runnable = () =>
        val _ = WindowsNativeChrome.apply(frame, palette)
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
    val previous = SwingWindow.CanvasResizeSnapshot(pixelSize.get(), viewportSize)
    pixelSize.set(snapshot.pixelSize)
    if SwingWindow.shouldPublishCanvasResize(previous, snapshot) then
      pendingResize.set(Some(snapshot.viewportSize))
      onResizeCallbackRef.get().foreach(_.apply())

  def doResizeIfNecessary(): Option[ViewportSize] =
    pendingResize.getAndSet(None)

object SwingWindow:
  private val ApplicationIconResource = "/icons/serenity.png"
  private val RoundedCornerMaskScale  = 2
  private[serenity] val Transparent   = new Color(0, 0, 0, 0)

  final private[serenity] case class PublishedImages(
      base: Option[BufferedImage],
      overlay: Option[BufferedImage]
  )

  private[serenity] object PublishedImages:
    val empty: PublishedImages = PublishedImages(None, None)

  final private[serenity] class ReusableImagePool:
    private val published = new AtomicReference[Option[BufferedImage]](None)
    private val spare     = new AtomicReference[Option[BufferedImage]](None)

    def acquire(width: Int, height: Int, imageType: Int): BufferedImage =
      spare
        .getAndSet(None)
        .filter(image => image.getWidth == width && image.getHeight == height && image.getType == imageType)
        .getOrElse(new BufferedImage(width, height, imageType))

    def publish(image: BufferedImage): Unit =
      val previous = published.getAndSet(Some(image))
      previous.filterNot(_ eq image).foreach(previousImage => spare.set(Some(previousImage)))

    def clearPublished(): Unit =
      published.getAndSet(None).foreach(image => spare.set(Some(image)))

  private[serenity] lazy val applicationIconImages: scala.List[Image] =
    Option(getClass.getResource(ApplicationIconResource))
      .flatMap(url => Option(ImageIO.read(url)))
      .toList

  val DefaultMetrics: CellMetrics           = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 13)
  val BaseMinWidth: Int                     = 400
  val BaseMinHeight: Int                    = 300
  private[serenity] val WindowTitle: String = "Serenity"

  /** A semantic application title with an optional visual-only sitter decoration. */
  final private[serenity] class DecorativeTitleLabel(initialDecoration: String, initiallyVisible: Boolean)
      extends JLabel(WindowTitle, SwingConstants.CENTER):
    private val decorationRef        = new AtomicReference(initialDecoration)
    private val decorationVisibleRef = new AtomicBoolean(initiallyVisible)

    SwingWindow.setAccessibleNameIfAvailable(this, WindowTitle)

    def updateDecoration(decoration: String, visible: Boolean): Unit =
      decorationRef.set(decoration)
      decorationVisibleRef.set(visible)
      revalidate()
      repaint()

    override def getPreferredSize: Dimension =
      val titleSize = super.getPreferredSize
      val decorationWidth =
        Option.when(decorationVisibleRef.get())(getFontMetrics(getFont).stringWidth(decorationRef.get())).getOrElse(0)
      new Dimension(titleSize.width + decorationWidth, titleSize.height)

    override def paintComponent(g: Graphics): Unit =
      val decoration = Option.when(decorationVisibleRef.get())(decorationRef.get()).getOrElse("")
      val title      = WindowTitle + decoration
      val font       = getFont
      val metrics    = g.getFontMetrics(font)
      val x          = (getWidth - metrics.stringWidth(title)) / 2
      val y          = (getHeight - metrics.getHeight) / 2 + metrics.getAscent
      g.setFont(font)
      g.setColor(getForeground)
      g.drawString(title, x, y)

  private[serenity] def perPixelTranslucencySupported: Boolean =
    GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice
      .isWindowTranslucencySupported(GraphicsDevice.WindowTranslucency.PERPIXEL_TRANSLUCENT)

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

  final private[serenity] class RoundedCornerMaskBufferCache:
    private val buffersRef = new AtomicReference[Option[RoundedCornerMaskBuffers]](None)

    @annotation.tailrec
    final def acquire(width: Int, height: Int, cornerArc: Int): RoundedCornerMaskBuffers =
      buffersRef.get() match
        case Some(buffers) if buffers.matches(width, height, cornerArc) => buffers
        case current =>
          val replacement = RoundedCornerMaskBuffers.create(width, height, cornerArc)
          if buffersRef.compareAndSet(current, Some(replacement)) then replacement
          else acquire(width, height, cornerArc)

  final private[serenity] class RoundedCornerMaskBuffers private (
      val width: Int,
      val height: Int,
      val cornerArc: Int,
      private val contents: BufferedImage,
      private val maskImage: BufferedImage,
      private val masked: BufferedImage
  ):

    def matches(otherWidth: Int, otherHeight: Int, otherCornerArc: Int): Boolean =
      width == otherWidth && height == otherHeight && cornerArc == otherCornerArc.max(0)

    def render(paintContents: Graphics => Unit): BufferedImage =
      val contentsGraphics = contents.createGraphics()
      try
        contentsGraphics.setComposite(AlphaComposite.Clear)
        contentsGraphics.fillRect(0, 0, width, height)
        contentsGraphics.setComposite(AlphaComposite.SrcOver)
        paintContents(contentsGraphics)
      finally contentsGraphics.dispose()
      mask(contents)

    def mask(source: BufferedImage): BufferedImage =
      val maskedGraphics = masked.createGraphics()
      try
        maskedGraphics.setComposite(AlphaComposite.Src)
        maskedGraphics.drawImage(source, 0, 0, null)
        maskedGraphics.setComposite(AlphaComposite.DstIn)
        maskedGraphics.drawImage(maskImage, 0, 0, null)
        masked
      finally maskedGraphics.dispose()

  private[serenity] object RoundedCornerMaskBuffers:

    def create(width: Int, height: Int, cornerArc: Int): RoundedCornerMaskBuffers =
      val normalizedWidth    = width.max(1)
      val normalizedHeight   = height.max(1)
      val normalizedArc      = cornerArc.max(0)
      val supersampledWidth  = normalizedWidth * RoundedCornerMaskScale
      val supersampledHeight = normalizedHeight * RoundedCornerMaskScale
      val supersampledArc    = normalizedArc * RoundedCornerMaskScale
      val supersampledMask = new BufferedImage(
        supersampledWidth,
        supersampledHeight,
        BufferedImage.TYPE_INT_ARGB
      )
      val maskGraphics = supersampledMask.createGraphics()
      try
        maskGraphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        maskGraphics.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
        maskGraphics.setColor(Color.WHITE)
        maskGraphics.fill(
          new RoundRectangle2D.Double(0, 0, supersampledWidth, supersampledHeight, supersampledArc, supersampledArc)
        )
      finally maskGraphics.dispose()

      val mask               = new BufferedImage(normalizedWidth, normalizedHeight, BufferedImage.TYPE_INT_ARGB)
      val downsampleGraphics = mask.createGraphics()
      try
        downsampleGraphics.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
        downsampleGraphics.setRenderingHint(
          RenderingHints.KEY_INTERPOLATION,
          RenderingHints.VALUE_INTERPOLATION_BICUBIC
        )
        downsampleGraphics.drawImage(supersampledMask, 0, 0, normalizedWidth, normalizedHeight, null)
      finally downsampleGraphics.dispose()

      new RoundedCornerMaskBuffers(
        normalizedWidth,
        normalizedHeight,
        normalizedArc,
        new BufferedImage(normalizedWidth, normalizedHeight, BufferedImage.TYPE_INT_ARGB),
        mask,
        new BufferedImage(normalizedWidth, normalizedHeight, BufferedImage.TYPE_INT_ARGB)
      )

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

  final private[serenity] class CoalescedEdtUpdate(update: () => Unit):
    private val queued = new AtomicBoolean(false)

    def schedule(enqueue: Runnable => Unit): Unit =
      if queued.compareAndSet(false, true) then
        enqueue(
          new Runnable:
            def run(): Unit =
              queued.set(false)
              update()
        )

  final case class ChromeMetrics(
      titleBarHeight: Int,
      buttonWidth: Int,
      margin: Int,
      cornerArc: Int,
      minWidth: Int,
      minHeight: Int,
      titleFontSize: Int
  )

  final case class CanvasResizeSnapshot(pixelSize: Dimension, viewportSize: ViewportSize)

  private[serenity] def shouldPublishCanvasResize(
    previous: CanvasResizeSnapshot,
    current: CanvasResizeSnapshot
  ): Boolean =
    previous.viewportSize != current.viewportSize

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

  def shouldRepaintBaseFrameBeforeCursorOverlay(cursorVisible: Boolean): Boolean =
    !cursorVisible

  /** The bound `onCursorOverlayReady` should pass to `canvas.repaint(...)`.
    *
    * `None` (an unbounded base frame) always wins, since a structural change may have moved pixels the cursor rects
    * alone wouldn't cover. Otherwise the result covers the base region plus every cursor rect from both the previous
    * and current frame -- a rect that isn't part of the union is either off-screen or zero-sized, since a cursor that
    * stopped being drawn still needs its last position repainted. Zero-sized rects (including the `(0, 0, 0, 0)`
    * sentinel callers use for "nothing" and "no cursor") are dropped before unioning: `Rectangle` still treats a
    * zero-sized rect as covering its `(x, y)` corner, which would otherwise drag every union back to the origin.
    */
  private[serenity] def combinedCursorRepaintRegion(
    baseDirtyRegion: Option[Rectangle],
    previousCursorRects: scala.List[Rectangle],
    currentCursorRects: scala.List[Rectangle]
  ): Option[Rectangle] =
    baseDirtyRegion.map { base =>
      (base :: previousCursorRects ::: currentCursorRects)
        .filter(rect => rect.width > 0 && rect.height > 0)
        .reduceOption(_.union(_))
        .getOrElse(new Rectangle(0, 0, 0, 0))
    }

  private[serenity] def publishRenderedBaseFrame(
    image: BufferedImage,
    replaceRenderedImage: Boolean,
    setRenderedImage: Option[BufferedImage] => Unit,
    repaint: () => Unit
  ): Unit =
    if replaceRenderedImage then
      setRenderedImage(Some(image))
      repaint()

  def copyImage(source: BufferedImage): BufferedImage =
    val copy = new BufferedImage(source.getWidth, source.getHeight, source.getType)
    val g    = copy.createGraphics()
    try g.drawImage(source, 0, 0, null)
    finally g.dispose()
    copy

  private[serenity] def clearImage(image: BufferedImage): Unit =
    val graphics = image.createGraphics()
    try
      graphics.setComposite(AlphaComposite.Clear)
      graphics.fillRect(0, 0, image.getWidth, image.getHeight)
    finally graphics.dispose()

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

  /** Recalculate the cell viewport after a font change using the currently laid-out canvas when available. */
  def fontMetricsUpdateSnapshot(
    metrics: CellMetrics,
    canvasSize: Dimension,
    previousCanvasSize: Dimension
  ): CanvasResizeSnapshot =
    canvasResizeSnapshot(metrics, canvasSize, previousCanvasSize)

  def canvasFallbackSize(
    windowSize: Dimension,
    chromeMode: WindowChromeMode,
    chromeMetrics: ChromeMetrics
  ): Dimension =
    val chromeHeight =
      chromeMode match
        case WindowChromeMode.Custom => chromeMetrics.titleBarHeight
        case WindowChromeMode.Auto | WindowChromeMode.Native | WindowChromeMode.NativeThemed => 0
    new Dimension(windowSize.width.max(1), (windowSize.height - chromeHeight).max(1))

  def fallbackCanvasResizeSnapshot(
    metrics: CellMetrics,
    windowSize: Dimension,
    chromeMode: WindowChromeMode,
    chromeMetrics: ChromeMetrics
  ): CanvasResizeSnapshot =
    canvasResizeSnapshot(
      metrics,
      new Dimension(0, 0),
      canvasFallbackSize(windowSize, chromeMode, chromeMetrics)
    )

  def resource(
    metrics: CellMetrics = DefaultMetrics,
    chromeMetrics: CellMetrics = DefaultMetrics,
    chromeMode: WindowChromeMode = WindowChromeMode.Auto,
    preferredWindowSize: Option[PreferredWindowSize] = None,
    initialWindowSitter: WindowSitter = WindowSitter.default,
    initialWindowSitterVisible: Boolean = true
  ): Resource[IO, SwingWindow] =
    Resource.make(
      IO.blocking {
        val initialSize = preferredWindowSize.map(_.normalized).getOrElse(PreferredWindowSize(1024, 768))
        val win = new SwingWindow(
          new Dimension(initialSize.width, initialSize.height),
          metrics,
          chromeMode,
          chromeMetrics,
          initialWindowSitter,
          initialWindowSitterVisible
        )
        win.start()
        win
      }
    )(win => IO.blocking(win.stop()))
