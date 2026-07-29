package com.serenity

import java.awt.image.BufferedImage
import java.awt.{Color, Dimension}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import javax.accessibility.AccessibleContext
import javax.swing.{JComponent, JLabel, JPanel}

import com.serenity.config.WindowChromeMode
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.terminal.{SwingWindow, WindowsNativeChrome}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SwingWindowChromeMetricsSpec extends AnyFlatSpec with Matchers:

  "SwingWindow application icons" should "load the bundled taskbar icon" in {
    val icons = SwingWindow.applicationIconImages

    icons should not be empty
    icons.head.getWidth(null) should be > 0
    icons.head.getHeight(null) should be > 0
  }

  "SwingWindow.ChromeMetrics" should "scale title chrome from the current UI metrics" in {
    val base   = SwingWindow.ChromeMetrics.fromCellMetrics(CellMetrics(charWidth = 8, lineHeight = 16, ascent = 13))
    val scaled = SwingWindow.ChromeMetrics.fromCellMetrics(CellMetrics(charWidth = 16, lineHeight = 32, ascent = 26))

    scaled.titleBarHeight shouldBe base.titleBarHeight * 2
    scaled.buttonWidth shouldBe base.buttonWidth * 2
    scaled.margin shouldBe base.margin * 2
    scaled.cornerArc shouldBe base.cornerArc * 2
    scaled.titleFontSize shouldBe base.titleFontSize * 2
  }

  "SwingWindow" should "use per-pixel translucency for non-maximized custom chrome when supported" in {
    SwingWindow.shouldUsePerPixelRoundedCorners(
      usesCustomChrome = true,
      maximized = false,
      perPixelTranslucencySupported = true
    ) shouldBe true

    SwingWindow.shouldUsePerPixelRoundedCorners(
      usesCustomChrome = true,
      maximized = true,
      perPixelTranslucencySupported = true
    ) shouldBe false

    SwingWindow.shouldUsePerPixelRoundedCorners(
      usesCustomChrome = true,
      maximized = false,
      perPixelTranslucencySupported = false
    ) shouldBe false
  }

  it should "use rounded custom chrome only for Auto mode on Linux" in {
    SwingWindow.shouldUseCustomChrome(WindowChromeMode.Auto, "Linux") shouldBe true
    SwingWindow.shouldUseCustomChrome(WindowChromeMode.Auto, "Windows 11") shouldBe false
    SwingWindow.shouldUseCustomChrome(WindowChromeMode.Native, "Linux") shouldBe false
    SwingWindow.shouldUseCustomChrome(WindowChromeMode.NativeThemed, "Linux") shouldBe false
    SwingWindow.shouldUseCustomChrome(WindowChromeMode.Custom, "Linux") shouldBe true
  }

  it should "render and hide sitter updates while keeping the semantic title stable" in {
    val titleLabel = new SwingWindow.DecorativeTitleLabel("rest", initiallyVisible = true)
    titleLabel.setForeground(Color.WHITE)

    val restingImage = renderTitle(titleLabel)
    val restingSize  = titleLabel.getPreferredSize

    titleLabel.updateDecoration("active", visible = true)
    val activeImage = renderTitle(titleLabel)
    val activeSize  = titleLabel.getPreferredSize

    titleLabel.updateDecoration("active", visible = false)
    val hiddenImage = renderTitle(titleLabel)
    val hiddenSize  = titleLabel.getPreferredSize

    activeSize.width should be > restingSize.width
    hiddenSize.width shouldBe SwingWindow.DecorativeTitleLabel("", initiallyVisible = false).getPreferredSize.width
    imagePixels(activeImage) should not be imagePixels(restingImage)
    imagePixels(hiddenImage) should not be imagePixels(activeImage)
    titleLabel.getText shouldBe SwingWindow.WindowTitle
    titleLabel.getAccessibleContext.getAccessibleName shouldBe SwingWindow.WindowTitle
  }

  it should "keep the decorative sitter title component accessible without naming the decoration" in {
    val titleLabel = new SwingWindow.DecorativeTitleLabel("rest", initiallyVisible = true)
    val titleBar   = new JPanel
    titleBar.add(titleLabel)

    val child = titleBar.getAccessibleContext.getAccessibleChild(0)
    child.getAccessibleContext should not be null
    child.getAccessibleContext.getAccessibleName shouldBe SwingWindow.WindowTitle

    titleLabel.updateDecoration("active", visible = true)

    child.getAccessibleContext should not be null
    child.getAccessibleContext.getAccessibleName shouldBe SwingWindow.WindowTitle
  }

  private def renderTitle(label: JLabel): BufferedImage =
    val size = label.getPreferredSize
    label.setSize(size)
    val image = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_ARGB)
    val graphics = image.createGraphics()
    try
      graphics.setColor(Color.BLACK)
      graphics.fillRect(0, 0, size.width, size.height)
      label.paint(graphics)
    finally graphics.dispose()
    image

  private def imagePixels(image: BufferedImage): Vector[Int] =
    image.getRGB(0, 0, image.getWidth, image.getHeight, null, 0, image.getWidth).toVector

  it should "refresh the per-pixel corner mask when chrome metrics change" in {
    val base   = SwingWindow.ChromeMetrics.fromCellMetrics(CellMetrics(charWidth = 8, lineHeight = 16, ascent = 13))
    val scaled = SwingWindow.ChromeMetrics.fromCellMetrics(CellMetrics(charWidth = 16, lineHeight = 32, ascent = 26))

    val before = SwingWindow.roundedCornerMask(
      usesCustomChrome = true,
      maximized = false,
      perPixelTranslucencySupported = true,
      cornerArc = base.cornerArc
    )
    val after = SwingWindow.roundedCornerMask(
      usesCustomChrome = true,
      maximized = false,
      perPixelTranslucencySupported = true,
      cornerArc = scaled.cornerArc
    )

    SwingWindow.shouldRefreshRoundedCornerMask(before, after) shouldBe true
  }

  it should "coalesce burst resize shape updates until the queued update runs" in {
    val updates   = new AtomicInteger(0)
    val queued    = new java.util.concurrent.ConcurrentLinkedQueue[Runnable]()
    val coalescer = new SwingWindow.CoalescedEdtUpdate(() => updates.incrementAndGet())

    coalescer.schedule(queued.add)
    coalescer.schedule(queued.add)
    coalescer.schedule(queued.add)

    queued.size shouldBe 1
    updates.get() shouldBe 0

    queued.remove().run()
    updates.get() shouldBe 1

    coalescer.schedule(queued.add)
    queued.size shouldBe 1
  }

  it should "antialias a per-pixel rounded-corner mask over the composed window contents" in {
    val contents = new BufferedImage(32, 32, BufferedImage.TYPE_INT_ARGB)
    val graphics = contents.createGraphics()
    try
      graphics.setColor(new Color(0x22, 0x44, 0x66))
      graphics.fillRect(0, 0, contents.getWidth, contents.getHeight)
    finally graphics.dispose()

    val masked = new SwingWindow.RoundedCornerMaskBufferCache().acquire(32, 32, cornerArc = 16).mask(contents)
    val alphas =
      for
        x <- 0 until masked.getWidth
        y <- 0 until masked.getHeight
      yield (masked.getRGB(x, y) >>> 24) & 0xff

    ((masked.getRGB(0, 0) >>> 24) & 0xff) shouldBe 0
    ((masked.getRGB(16, 0) >>> 24) & 0xff) shouldBe 255
    alphas.exists(alpha => alpha > 0 && alpha < 255) shouldBe true
    val topCornerEdgeAlpha = (masked.getRGB(4, 0) >>> 24) & 0xff
    topCornerEdgeAlpha should be > 0
    topCornerEdgeAlpha should be < 40
  }

  "SwingWindow.RoundedCornerMaskBufferCache" should "reuse buffers until their size or corner arc changes" in {
    val cache        = new SwingWindow.RoundedCornerMaskBufferCache
    val initial      = cache.acquire(width = 640, height = 480, cornerArc = 12)
    val sameGeometry = cache.acquire(width = 640, height = 480, cornerArc = 12)
    val resized      = cache.acquire(width = 800, height = 480, cornerArc = 12)
    val resizedArc   = cache.acquire(width = 800, height = 480, cornerArc = 24)

    sameGeometry should be theSameInstanceAs initial
    resized should not be theSameInstanceAs(initial)
    resizedArc should not be theSameInstanceAs(resized)
  }

  it should "clear prior frame pixels before masking a reused rounded buffer" in {
    val buffers = new SwingWindow.RoundedCornerMaskBufferCache().acquire(width = 32, height = 32, cornerArc = 16)

    buffers.render { graphics =>
      graphics.setColor(Color.RED)
      graphics.fillRect(0, 0, 32, 32)
    }
    val refreshed = buffers.render { graphics =>
      graphics.setColor(Color.BLUE)
      graphics.fillRect(16, 1, 1, 1)
    }

    ((refreshed.getRGB(16, 16) >>> 24) & 0xff) shouldBe 0
    refreshed.getRGB(16, 1) shouldBe Color.BLUE.getRGB
  }

  it should "derive viewport size from the live canvas size when available" in {
    val metrics        = CellMetrics(charWidth = 10, lineHeight = 20, ascent = 15)
    val canvasSize     = new Dimension(640, 480)
    val requestedFrame = new Dimension(1200, 900)

    val snapshot = SwingWindow.canvasResizeSnapshot(metrics, canvasSize, requestedFrame)

    snapshot.pixelSize shouldBe new Dimension(640, 480)
    snapshot.viewportSize.width shouldBe 64
    snapshot.viewportSize.height shouldBe 24
  }

  "SwingWindow font metric updates" should "recalculate the viewport from the live canvas size" in {
    val updatedMetrics = CellMetrics(charWidth = 20, lineHeight = 40, ascent = 30)
    val liveCanvasSize = new Dimension(800, 480)
    val staleFallback  = new Dimension(1200, 900)

    val snapshot = SwingWindow.fontMetricsUpdateSnapshot(updatedMetrics, liveCanvasSize, staleFallback)

    snapshot.pixelSize shouldBe liveCanvasSize
    snapshot.viewportSize shouldBe ViewportSize(40, 12)
  }

  it should "publish a resize only when the cell viewport changes" in {
    val current         = SwingWindow.CanvasResizeSnapshot(new Dimension(640, 480), ViewportSize(64, 24))
    val sameViewport    = SwingWindow.CanvasResizeSnapshot(new Dimension(645, 495), ViewportSize(64, 24))
    val changedViewport = SwingWindow.CanvasResizeSnapshot(new Dimension(650, 500), ViewportSize(65, 25))

    SwingWindow.shouldPublishCanvasResize(current, sameViewport) shouldBe false
    SwingWindow.shouldPublishCanvasResize(current, changedViewport) shouldBe true
  }

  it should "fall back to the requested window size before the canvas has been laid out" in {
    val metrics        = CellMetrics(charWidth = 10, lineHeight = 20, ascent = 15)
    val canvasSize     = new Dimension(0, 0)
    val requestedFrame = new Dimension(1200, 900)

    val snapshot = SwingWindow.canvasResizeSnapshot(metrics, canvasSize, requestedFrame)

    snapshot.pixelSize shouldBe new Dimension(1200, 900)
    snapshot.viewportSize.width shouldBe 120
    snapshot.viewportSize.height shouldBe 45
  }

  it should "subtract custom title chrome from fallback canvas height" in {
    val requestedWindow = new Dimension(1200, 900)
    val chrome = SwingWindow.ChromeMetrics.fromCellMetrics(CellMetrics(charWidth = 10, lineHeight = 20, ascent = 15))

    val fallback = SwingWindow.canvasFallbackSize(requestedWindow, WindowChromeMode.Custom, chrome)

    fallback.width shouldBe requestedWindow.width
    fallback.height shouldBe requestedWindow.height - chrome.titleBarHeight
  }

  it should "use the full fallback canvas size for native chrome" in {
    val requestedWindow = new Dimension(1200, 900)
    val chrome = SwingWindow.ChromeMetrics.fromCellMetrics(CellMetrics(charWidth = 10, lineHeight = 20, ascent = 15))

    val fallback = SwingWindow.canvasFallbackSize(requestedWindow, WindowChromeMode.Native, chrome)

    fallback shouldBe requestedWindow
  }

  it should "use the full fallback canvas size for native-themed chrome" in {
    val requestedWindow = new Dimension(1200, 900)
    val chrome = SwingWindow.ChromeMetrics.fromCellMetrics(CellMetrics(charWidth = 10, lineHeight = 20, ascent = 15))

    SwingWindow.canvasFallbackSize(requestedWindow, WindowChromeMode.NativeThemed, chrome) shouldBe requestedWindow
  }

  "WindowsNativeChrome" should "advertise support only for Windows platform names" in {
    WindowsNativeChrome.isSupported("Windows 11") shouldBe true
    WindowsNativeChrome.isSupported("Mac OS X") shouldBe false
    WindowsNativeChrome.isSupported("Linux") shouldBe false
  }

  it should "request compositor-rounded corners for native themed windows" in {
    WindowsNativeChrome.WindowCornerPreference shouldBe 33
    WindowsNativeChrome.RoundedCornerPreference shouldBe 2
  }

  "SwingWindow.NativeChromeThemeCache" should "avoid reapplying an unchanged supported palette" in {
    val cache        = new SwingWindow.NativeChromeThemeCache
    val lightPalette = SwingWindow.ChromePalette.fromTheme(Theme.light)
    val darkPalette  = SwingWindow.ChromePalette.fromTheme(Theme.dark)

    cache.recordIfChanged(lightPalette, supported = true) shouldBe true
    cache.recordIfChanged(lightPalette, supported = true) shouldBe false
    cache.recordIfChanged(darkPalette, supported = true) shouldBe true
    cache.recordIfChanged(darkPalette, supported = true) shouldBe false
  }

  it should "leave the palette uncached when native chrome is unsupported" in {
    val cache   = new SwingWindow.NativeChromeThemeCache
    val palette = SwingWindow.ChromePalette.fromTheme(Theme.light)

    cache.recordIfChanged(palette, supported = false) shouldBe false
    cache.recordIfChanged(palette, supported = true) shouldBe true
  }

  it should "derive custom chrome fallback viewport from the post-title-bar canvas" in {
    val metrics         = CellMetrics(charWidth = 10, lineHeight = 20, ascent = 15)
    val requestedWindow = new Dimension(1200, 900)
    val chrome          = SwingWindow.ChromeMetrics.fromCellMetrics(metrics)

    val snapshot = SwingWindow.fallbackCanvasResizeSnapshot(metrics, requestedWindow, WindowChromeMode.Custom, chrome)

    snapshot.pixelSize shouldBe new Dimension(1200, 900 - chrome.titleBarHeight)
    snapshot.viewportSize.width shouldBe 120
    snapshot.viewportSize.height shouldBe (900 - chrome.titleBarHeight) / metrics.lineHeight
  }

  "SwingWindow.ChromePalette" should "derive custom chrome colours from the active theme" in {
    val theme = Theme.light.copy(
      border = new Color(0x111111),
      panelBorder = new Color(0x222222)
    )
    val palette = SwingWindow.ChromePalette.fromTheme(theme)

    palette.titleBackground shouldBe theme.panel.background
    palette.titleForeground shouldBe theme.panel.foreground
    palette.border shouldBe theme.panelBorder
    palette.border should not be theme.border
    palette.closeHoverBackground shouldBe theme.error.foreground
  }

  it should "derive distinct custom chrome colours for dark and light themes" in {
    val darkPalette  = SwingWindow.ChromePalette.fromTheme(Theme.dark)
    val lightPalette = SwingWindow.ChromePalette.fromTheme(Theme.light)

    darkPalette.titleBackground should not be lightPalette.titleBackground
    darkPalette.titleForeground should not be lightPalette.titleForeground
    darkPalette.buttonHoverBackground should not be lightPalette.buttonHoverBackground
  }

  it should "derive pressed button colours from the active theme" in {
    val palette = SwingWindow.ChromePalette.fromTheme(Theme.light)

    palette.buttonPressedBackground should not be palette.titleBackground
    palette.buttonPressedBackground should not be palette.buttonHoverBackground
    palette.closePressedBackground should not be palette.closeHoverBackground
  }

  it should "derive focused button affordance colours from the active theme" in {
    val palette = SwingWindow.ChromePalette.fromTheme(Theme.light)

    palette.focusBorder shouldBe Theme.light.highlighted.foreground
    palette.focusBorder should not be palette.border
  }

  it should "ignore missing accessibility contexts when naming custom chrome controls" in {
    val component = new JComponent:
      override def getAccessibleContext: AccessibleContext = null

    noException should be thrownBy SwingWindow.setAccessibleNameIfAvailable(
      component,
      SwingWindow.ChromeControlKind.Close.accessibleName
    )
  }

  "SwingWindow.TitleBarDrag" should "move by pointer delta while custom chrome is restored" in
    SwingWindow
      .titleBarDragDecision(
        maximized = false,
        anchorX = 100,
        anchorY = 200,
        pointerX = 140,
        pointerY = 185
      )
      .shouldBe(SwingWindow.TitleBarDragDecision(restoreFirst = false, moveDelta = Some((40, -15))))

  it should "restore before continuing a custom chrome drag from maximized state" in
    SwingWindow
      .titleBarDragDecision(
        maximized = true,
        anchorX = 100,
        anchorY = 200,
        pointerX = 140,
        pointerY = 185
      )
      .shouldBe(SwingWindow.TitleBarDragDecision(restoreFirst = true, moveDelta = None))

  "SwingWindow.BaseFramePublish" should "defer repaint when a visible cursor overlay will be published next" in {
    SwingWindow.shouldRepaintBaseFrameBeforeCursorOverlay(cursorVisible = true).shouldBe(false)
    SwingWindow.shouldRepaintBaseFrameBeforeCursorOverlay(cursorVisible = false).shouldBe(true)
  }

  it should "keep the displayed image unchanged for a base-only publish" in {
    val image        = new BufferedImage(2, 2, BufferedImage.TYPE_INT_ARGB)
    val rendered     = AtomicReference[Option[BufferedImage]](None)
    val repaintCount = AtomicInteger(0)

    SwingWindow.publishRenderedBaseFrame(
      image,
      replaceRenderedImage = false,
      setRenderedImage = rendered.set,
      repaint = () => repaintCount.incrementAndGet()
    )

    rendered.get().shouldBe(None)
    repaintCount.get().shouldBe(0)
  }

  it should "replace the displayed image and repaint for a visible base publish" in {
    val image        = new BufferedImage(2, 2, BufferedImage.TYPE_INT_ARGB)
    val rendered     = AtomicReference[Option[BufferedImage]](None)
    val repaintCount = AtomicInteger(0)

    SwingWindow.publishRenderedBaseFrame(
      image,
      replaceRenderedImage = true,
      setRenderedImage = rendered.set,
      repaint = () => repaintCount.incrementAndGet()
    )

    rendered.get().shouldBe(Some(image))
    repaintCount.get().shouldBe(1)
  }

  "SwingWindow.ChromeControlLayout" should "place macOS controls on the left in platform order" in
    SwingWindow.ChromeControlLayout
      .forOs("Mac OS X")
      .shouldBe(
        SwingWindow.ChromeControlLayout(
          placement = SwingWindow.ChromeControlPlacement.Left,
          controls = List(
            SwingWindow.ChromeControlKind.Close,
            SwingWindow.ChromeControlKind.Minimize,
            SwingWindow.ChromeControlKind.Maximize
          )
        )
      )

  it should "place non-macOS controls on the right in Windows/Linux order" in {
    SwingWindow.ChromeControlLayout
      .forOs("Windows 11")
      .shouldBe(
        SwingWindow.ChromeControlLayout(
          placement = SwingWindow.ChromeControlPlacement.Right,
          controls = List(
            SwingWindow.ChromeControlKind.Minimize,
            SwingWindow.ChromeControlKind.Maximize,
            SwingWindow.ChromeControlKind.Close
          )
        )
      )
    SwingWindow.ChromeControlLayout
      .forOs("Linux")
      .shouldBe(
        SwingWindow.ChromeControlLayout(
          placement = SwingWindow.ChromeControlPlacement.Right,
          controls = List(
            SwingWindow.ChromeControlKind.Minimize,
            SwingWindow.ChromeControlKind.Maximize,
            SwingWindow.ChromeControlKind.Close
          )
        )
      )
  }

  "SwingWindow.ChromeControlPaint" should "resolve button colours by state and control kind" in {
    val palette = SwingWindow.ChromePalette.fromTheme(Theme.light)

    SwingWindow.ChromeControlPaint.background(
      SwingWindow.ChromeControlKind.Minimize,
      palette,
      SwingWindow.ChromeControlState()
    ) shouldBe palette.titleBackground
    SwingWindow.ChromeControlPaint.background(
      SwingWindow.ChromeControlKind.Minimize,
      palette,
      SwingWindow.ChromeControlState(hovered = true)
    ) shouldBe palette.buttonHoverBackground
    SwingWindow.ChromeControlPaint.background(
      SwingWindow.ChromeControlKind.Minimize,
      palette,
      SwingWindow.ChromeControlState(pressed = true)
    ) shouldBe palette.buttonPressedBackground
    SwingWindow.ChromeControlPaint.background(
      SwingWindow.ChromeControlKind.Close,
      palette,
      SwingWindow.ChromeControlState(hovered = true)
    ) shouldBe palette.closeHoverBackground
    val contrastPalette = palette.copy(
      titleForeground = new Color(0x010203),
      closeHoverForeground = new Color(0xf0e0d0)
    )
    SwingWindow.ChromeControlPaint.foreground(
      SwingWindow.ChromeControlKind.Close,
      contrastPalette,
      SwingWindow.ChromeControlState(pressed = true)
    ) shouldBe contrastPalette.closeHoverForeground
    SwingWindow.ChromeControlPaint.focusBorder(palette, SwingWindow.ChromeControlState()) shouldBe None
    SwingWindow.ChromeControlPaint.focusBorder(palette, SwingWindow.ChromeControlState(focused = true)) shouldBe
      Some(palette.focusBorder)
  }

  "SwingWindow.ChromeIconGeometry" should "draw every control icon inside the same centered box" in {
    val icons = List(
      SwingWindow.ChromeControlKind.Minimize,
      SwingWindow.ChromeControlKind.Maximize,
      SwingWindow.ChromeControlKind.Restore,
      SwingWindow.ChromeControlKind.Close
    )

    icons.foreach { kind =>
      val lines = SwingWindow.ChromeIconGeometry.lines(kind, width = 46, height = 32)

      lines should not be empty
      lines.foreach { line =>
        line.x1 should be >= 14
        line.x1 should be <= 32
        line.x2 should be >= 14
        line.x2 should be <= 32
        line.y1 should be >= 7
        line.y1 should be <= 25
        line.y2 should be >= 7
        line.y2 should be <= 25
      }
    }
  }

end SwingWindowChromeMetricsSpec
