package com.serenity.ui.terminal

import java.awt.*
import java.awt.event.*
import java.awt.image.BufferedImage
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import javax.imageio.ImageIO
import javax.swing.*

import scala.jdk.CollectionConverters.*

import cats.effect.{IO, Resource}
import com.serenity.config.{PreferredWindowSize, WindowChromeMode}
import com.serenity.diagnostics.FrameTimings
import com.serenity.ui.accessibility.{AccessibilityPublishGate, AccessibilitySnapshot, SwingAccessibilityBridge}
import com.serenity.ui.display.DisplayScale
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.theme.Theme

class SwingWindow(
    initialPixelSize: Dimension,
    initialMetrics: CellMetrics,
    chromeMode: WindowChromeMode = WindowChromeMode.Auto,
    initialChromeMetrics: CellMetrics,
    frameTimings: FrameTimings = FrameTimings(),
    env: Map[String, String] = sys.env,
    windowTitle: String = SwingWindow.WindowTitle
):

  private val usesCustomChrome =
    SwingWindow.shouldUseCustomChrome(
      chromeMode,
      System.getProperty("os.name", ""),
      env,
      SwingWindow.isNativeWaylandToolkit(Toolkit.getDefaultToolkit.getClass.getName)
    )

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

  private val initialCanvasPixelSize   = initialCanvasResizeSnapshot.pixelSize
  private val pixelSize                = new AtomicReference(initialCanvasPixelSize)
  private val metricsRef               = new AtomicReference(initialMetrics)
  private val chromeMetricsRef         = new AtomicReference(initialChromeLayoutMetrics)
  private val chromePaletteRef         = new AtomicReference(SwingWindow.ChromePalette.fromTheme(Theme.default))
  private val nativeChromeThemeCache   = new SwingWindow.ChromePaletteCache
  private val customChromePaletteCache = new SwingWindow.ChromePaletteCache
  private val pendingResize            = new AtomicReference[Option[ViewportSize]](None)
  private val closeLatch               = new CountDownLatch(1)
  private val baseImageRef             = new AtomicReference[Option[BufferedImage]](None)
  private val publishedCaretsRef       = new AtomicReference[scala.List[SwingWindow.CaretPaint]](Nil)
  private val previousCursorRectsRef   = new AtomicReference[scala.List[Rectangle]](Nil)
  private val pendingRepaintRef        = new AtomicReference[Option[SwingWindow.CanvasRepaint]](None)
  private val baseImagePool            = new SwingWindow.ReusableImagePool
  private val savedBoundsRef           = new AtomicReference[Option[Rectangle]](None)
  private val maximizedRef             = new AtomicBoolean(false)
  private val maxBtnRef                = new AtomicReference[Option[ChromeControlButton]](None)
  private val controlButtonsRef        = new AtomicReference[scala.List[ChromeControlButton]](Nil)
  private val controlPanelRef          = new AtomicReference[Option[JPanel]](None)
  private val titleBarRef              = new AtomicReference[Option[JPanel]](None)
  private val titleLabelRef            = new AtomicReference[Option[JLabel]](None)
  private val titleSpacerRef           = new AtomicReference[Option[JPanel]](None)
  private val onResizeCallbackRef      = new AtomicReference[Option[() => Unit]](None)
  private val onFocusCallbackRef       = new AtomicReference[Option[Boolean => Unit]](None)

  def setOnResize(cb: () => Unit): Unit = onResizeCallbackRef.set(Some(cb))

  /** Register a callback fired with `true` when the window gains keyboard focus and `false` when it loses it. */
  def setOnFocusChange(cb: Boolean => Unit): Unit = onFocusCallbackRef.set(Some(cb))

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
      val paintStart = frameTimings.paintStarted()
      val g2         = g.create().asInstanceOf[Graphics2D]
      try
        val base = baseImagePool.leasePublished()
        SwingWindow.paintPresentedFrame(g2, base, publishedCaretsRef.get(), getWidth, getHeight)
      finally
        baseImagePool.releaseLease()
        g2.dispose()
        frameTimings.paintFinished(paintStart)

  private val accessibilityBridge      = new SwingAccessibilityBridge(canvas)
  private val accessibilityPublishGate = new AccessibilityPublishGate

  /** Publish the semantic projection of the custom-painted canvas to Swing accessibility clients. */
  def updateAccessibility(snapshot: AccessibilitySnapshot): Unit =
    val currentMetrics = metrics
    if accessibilityPublishGate.admit(snapshot, currentMetrics) then
      val publish: Runnable = () => accessibilityBridge.publish(snapshot, currentMetrics)
      if SwingUtilities.isEventDispatchThread then publish.run()
      else SwingUtilities.invokeLater(publish)

  def onImageReady(image: BufferedImage): Unit =
    onImageReady(image, None)

  /** Publish a finished base frame, repainting only `dirtyRects` when the rest of the frame is known to be identical to
    * what is already on screen -- plus any carets this frame drops, whose pixels would otherwise survive.
    */
  def onImageReady(image: BufferedImage, dirtyRects: Option[scala.List[Rectangle]]): Unit =
    val displayedCarets = publishedCaretsRef.getAndSet(Nil)
    baseImagePool.publish(image)
    baseImageRef.set(Some(image))
    requestRepaint(SwingWindow.cursorRepaint(dirtyRects, displayedCarets.map(_.rect), Nil))

  private def requestRepaint(repaint: SwingWindow.CanvasRepaint): Unit =
    if repaint != SwingWindow.CanvasRepaint.Rects(Nil) then
      frameTimings.framePublished()
      val earlier =
        pendingRepaintRef.getAndUpdate(pending => Some(pending.fold(repaint)(SwingWindow.mergedRepaint(_, repaint))))
      if earlier.isEmpty then SwingUtilities.invokeLater(() => paintPendingRepaint())

  /** Each rect is painted on its own and at once: `canvas.repaint(rect)` would let Swing's `RepaintManager` fold them
    * into one dirty rect spanning all of them, the very copy splitting them avoids. Frames published before this runs
    * have already merged into the pending repaint, so a burst of frames is still painted once.
    */
  private def paintPendingRepaint(): Unit =
    pendingRepaintRef.getAndSet(None).foreach {
      case SwingWindow.CanvasRepaint.Whole        => canvas.repaint()
      case SwingWindow.CanvasRepaint.Rects(rects) => rects.foreach(rect => canvas.paintImmediately(rect))
    }

  def onBaseImageReady(image: BufferedImage): Unit =
    publishedCaretsRef.set(Nil)
    baseImagePool.publish(image)
    baseImageRef.set(Some(image))

  /** Publish the carets to fill over the current base frame and repaint just the pixels they changed.
    *
    * A caret that moved needs both its old and new position repainted -- not just whatever the base frame changed.
    * `baseDirtyRects` are the caller's own bounded-repaint rects for the base frame (`None` for "the whole canvas
    * changed"); `paintCarets` reports the carets to fill. The repaint covers all three ([[SwingWindow.cursorRepaint]]),
    * or the whole canvas whenever `baseDirtyRects` itself is `None`.
    */
  def onCursorOverlayReady(baseDirtyRects: Option[scala.List[Rectangle]])(
    paintCarets: => scala.List[SwingWindow.CaretPaint]
  ): Boolean =
    baseImageRef.get() match
      case Some(_) =>
        val carets = paintCarets
        publishedCaretsRef.set(carets)
        val currentCursorRects  = carets.map(_.rect)
        val previousCursorRects = previousCursorRectsRef.getAndSet(currentCursorRects)
        requestRepaint(SwingWindow.cursorRepaint(baseDirtyRects, previousCursorRects, currentCursorRects))
        true
      case None =>
        false

  private[serenity] def acquireBaseImage(width: Int, height: Int, imageType: Int): BufferedImage =
    baseImagePool.acquire(width, height, imageType)

  private def toggleMaximize(): Unit =
    if maximizedRef.get() then
      frame.setExtendedState(Frame.NORMAL)
      savedBoundsRef.get().foreach(frame.setBounds)
    else
      savedBoundsRef.set(Some(frame.getBounds))
      frame.setExtendedState(Frame.MAXIMIZED_BOTH)

  private def activateChromeControl(kind: SwingWindow.ChromeControlKind): Unit =
    kind match
      case SwingWindow.ChromeControlKind.Minimize =>
        frame.setExtendedState(Frame.ICONIFIED)
      case SwingWindow.ChromeControlKind.Maximize | SwingWindow.ChromeControlKind.Restore =>
        toggleMaximize()
      case SwingWindow.ChromeControlKind.Close =>
        closeLatch.countDown()

  private val chromeTitleBar: ChromeTitleBar = new ChromeTitleBar(
    chromePaletteRef,
    maximizedRef,
    () => frame,
    () => chromeControlFont,
    () => chromeButtonSize,
    () => chromeSpacerSize,
    () => chromeTitleBarSize,
    () => toggleMaximize(),
    activateChromeControl,
    windowTitle
  )

  maxBtnRef.set(chromeTitleBar.maxButton)
  controlButtonsRef.set(chromeTitleBar.controlButtons)
  controlPanelRef.set(Some(chromeTitleBar.controlPanel))
  titleSpacerRef.set(Some(chromeTitleBar.titleSpacer))
  titleLabelRef.set(Some(chromeTitleBar.titleLabel))
  titleBarRef.set(Some(chromeTitleBar.panel))

  private val frame: JFrame =
    val f = new JFrame(windowTitle)
    f.setIconImages(SwingWindow.applicationIconImages.asJava)
    f.setUndecorated(usesCustomChrome)
    f.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    f.addWindowListener(
      new WindowAdapter:
        override def windowClosing(e: WindowEvent): Unit = closeLatch.countDown()
    )
    f.addWindowFocusListener(
      new WindowFocusListener:
        override def windowGainedFocus(e: WindowEvent): Unit = onFocusCallbackRef.get().foreach(_.apply(true))
        override def windowLostFocus(e: WindowEvent): Unit   = onFocusCallbackRef.get().foreach(_.apply(false))
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
      }
    )
    val content = new JPanel(new BorderLayout):
      setBackground(Color.BLACK)
    if usesCustomChrome then content.add(chromeTitleBar.panel, BorderLayout.NORTH)
    content.add(canvas, BorderLayout.CENTER)
    f.setContentPane(content)
    if usesCustomChrome then
      val glassPane = new ResizeGlassPane(() => frame, chromeMetricsRef, maximizedRef)
      f.setGlassPane(glassPane)
      glassPane.setVisible(true)
    f.pack()
    if usesCustomChrome then
      val chrome = chromeMetricsRef.get()
      f.setMinimumSize(new Dimension(chrome.minWidth, chrome.minHeight))
    else f.setMinimumSize(new Dimension(SwingWindow.BaseMinWidth, SwingWindow.BaseMinHeight))
    f.setLocationRelativeTo(null)
    f

  def awaitClose: IO[Unit] = SwingWindow.awaitCloseLatch(closeLatch)

  /** Raises the window for a later launch that handed its files over to this one (#2023). */
  def bringToFront(): Unit =
    SwingUtilities.invokeLater { () =>
      if (frame.getExtendedState & Frame.ICONIFIED) != 0 then
        frame.setExtendedState(frame.getExtendedState & ~Frame.ICONIFIED)
      frame.toFront()
      frame.requestFocus()
      val _ = canvas.requestFocusInWindow()
    }

  def start(): Unit =
    val showWindow: Runnable = () =>
      frame.setVisible(true)
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
      if customChromePaletteCache.recordIfChanged(palette, supported = true) then
        chromePaletteRef.set(palette)
        val applyPalette: Runnable = () => applyChromePalette(palette)
        if SwingUtilities.isEventDispatchThread then applyPalette.run()
        else SwingUtilities.invokeLater(applyPalette)
    else if usesNativeThemedChrome then updateNativeChromeTheme(SwingWindow.ChromePalette.fromTheme(theme))

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

object SwingWindow extends SwingWindowChromeSupport with SwingWindowImageSupport with SwingWindowLayoutSupport:
  private val ApplicationIconResource = "/icons/serenity.png"

  /** Blocks until the window-close latch is counted down (chrome close control or the AWT `windowClosing` event).
    *
    * Uses `IO.interruptible` rather than `IO.blocking` so that cancellation actually interrupts the parked `await()`.
    * The in-app Quit path races this against the quit signal (`AppRuntime.coordinateExternalQuit`); when quit wins, the
    * loser is cancelled, and an uninterruptible `IO.blocking` would leave the run IO waiting forever on a native
    * `await()` that can never reach a cancellation boundary -- the app would exit only via the chrome close control,
    * hanging on the in-app Quit option (thread parked here, `main` parked in `IOApp`).
    */
  private[serenity] def awaitCloseLatch(latch: CountDownLatch): IO[Unit] =
    IO.interruptible(latch.await())

  private[serenity] lazy val applicationIconImages: scala.List[Image] =
    Option(getClass.getResource(ApplicationIconResource))
      .flatMap(url => Option(ImageIO.read(url)))
      .toList

  val DefaultMetrics: CellMetrics           = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 13)
  val BaseMinWidth: Int                     = 400
  val BaseMinHeight: Int                    = 300
  private[serenity] val WindowTitle: String = "Serenity"

  def resource(
    metrics: CellMetrics = DefaultMetrics,
    chromeMetrics: CellMetrics = DefaultMetrics,
    chromeMode: WindowChromeMode = WindowChromeMode.Auto,
    preferredWindowSize: Option[PreferredWindowSize] = None,
    frameTimings: FrameTimings = FrameTimings(),
    title: String = WindowTitle
  ): Resource[IO, SwingWindow] =
    Resource.make(
      IO.blocking {
        val initialSize = preferredWindowSize.map(_.normalized).getOrElse(PreferredWindowSize(1024, 768))
        val win = new SwingWindow(
          new Dimension(initialSize.width, initialSize.height),
          metrics,
          chromeMode,
          chromeMetrics,
          frameTimings,
          windowTitle = title
        )
        win.start()
        win
      }
    )(win => IO.blocking(win.stop()))
