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
  private val onFocusCallbackRef     = new AtomicReference[Option[Boolean => Unit]](None)
  private val resizeGlassPaneRef     = new AtomicReference[Option[JComponent]](None)
  private val roundedCornerMaskRef   = new AtomicReference[Option[Int]](None)
  private val roundedContentBuffers  = new SwingWindow.RoundedCornerMaskBufferCache
  private val perPixelTranslucencySupported =
    SwingWindow.perPixelTranslucencySupported
  private val shapeUpdateCoalescer = new SwingWindow.CoalescedEdtUpdate(() => updateShape())

  /** Whether `canvas` should paint its own background as genuinely transparent this frame -- see
    * [[SwingWindow.shouldPaintTransparentContent]]. Kept up to date by [[updateChromeTheme]], which already runs once
    * per frame (`Main`'s `syncChromeTheme`), so no extra wiring is needed to keep this current.
    */
  private val contentTransparentRef = new AtomicBoolean(false)

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
      val g2 = g.create().asInstanceOf[Graphics2D]
      try SwingWindow.paintCanvasBackground(g2, getWidth, getHeight, contentTransparentRef.get())
      finally g2.dispose()
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

  private def activateChromeControl(kind: SwingWindow.ChromeControlKind): Unit =
    kind match
      case SwingWindow.ChromeControlKind.Minimize =>
        frame.setExtendedState(Frame.ICONIFIED)
      case SwingWindow.ChromeControlKind.Maximize | SwingWindow.ChromeControlKind.Restore =>
        toggleMaximize()
      case SwingWindow.ChromeControlKind.Close =>
        closeLatch.countDown()

  private val chromeTitleBar: ChromeTitleBar = new ChromeTitleBar(
    initialWindowSitter,
    initialWindowSitterVisible,
    chromePaletteRef,
    maximizedRef,
    () => frame,
    () => chromeControlFont,
    () => chromeButtonSize,
    () => chromeSpacerSize,
    () => chromeTitleBarSize,
    () => toggleMaximize(),
    activateChromeControl
  )

  maxBtnRef.set(chromeTitleBar.maxButton)
  controlButtonsRef.set(chromeTitleBar.controlButtons)
  controlPanelRef.set(Some(chromeTitleBar.controlPanel))
  titleSpacerRef.set(Some(chromeTitleBar.titleSpacer))
  titleLabelRef.set(Some(chromeTitleBar.titleLabel))
  titleBarRef.set(Some(chromeTitleBar.panel))

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
          updateShape()
      }
    )
    f.addComponentListener(
      new ComponentAdapter:
        override def componentResized(e: ComponentEvent): Unit =
          scheduleShapeUpdate()
    )
    val content = new RoundedContentPane(
      new BorderLayout,
      usesCustomChrome,
      maximizedRef,
      perPixelTranslucencySupported,
      chromeMetricsRef,
      roundedContentBuffers
    ):
      setBackground(Color.BLACK)
    if usesCustomChrome then content.add(chromeTitleBar.panel, BorderLayout.NORTH)
    content.add(canvas, BorderLayout.CENTER)
    f.setContentPane(content)
    if usesCustomChrome then
      val glassPane = new ResizeGlassPane(() => frame, chromeMetricsRef, maximizedRef)
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

  def awaitClose: IO[Unit] = SwingWindow.awaitCloseLatch(closeLatch)

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
    contentTransparentRef.set(
      SwingWindow.shouldPaintTransparentContent(
        usesCustomChrome,
        perPixelTranslucencySupported,
        theme.background.getAlpha
      )
    )
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
  private[serenity] val Transparent = new Color(0, 0, 0, 0)

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
