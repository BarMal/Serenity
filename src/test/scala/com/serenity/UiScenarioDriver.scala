package com.serenity

import java.awt.font.FontRenderContext
import java.awt.image.BufferedImage
import java.awt.{Color, Font}
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO
import com.serenity.config.ConfigManager
import com.serenity.markdown.{MarkdownBlockLens, MarkdownDocumentPreview}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.renderer.{Java2DRenderSurface, RenderSurface, Renderer}
import com.serenity.ui.theme.TextStyle
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

/** Fixed rendering environment for a deterministic, headless UI scenario. */
case class UiScenarioEnvironment(
    viewport: ViewportSize = ViewportSize(100, 30),
    cellMetrics: CellMetrics = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 12),
    deviceScale: Double = 1.0,
    themeName: String = "dark"
)

/** Semantic data captured with each rendered scenario frame. */
case class ScenarioFrameEvidence(
    focus: Focus,
    surfaceRects: Map[SurfaceId, LayoutRect],
    itemRects: Map[SurfaceId, List[LayoutRect]],
    sourcePreviewMappings: Map[BufferId, Set[Int]],
    previewPlacements: Map[BufferId, ScenarioPreviewPlacement],
    visiblePreviewSourceLines: Map[BufferId, Set[Int]],
    visibleText: List[String],
    drawnText: List[ScenarioDrawnText],
    paintedRegions: List[ScenarioPaintedRegion],
    borders: List[ScenarioBorder],
    styleCalls: List[ScenarioStyleCall],
    drawnItems: Map[SurfaceId, List[ScenarioDrawnItem]],
    drawnImageRects: List[LayoutRect],
    renderedContentRows: Set[Int],
    animationComplete: Boolean,
    layoutViolations: List[LayoutContractViolation]
)

/** Text and its cell bounds as actually submitted to the render surface. */
case class ScenarioDrawnText(text: String, bounds: LayoutRect)

/** A renderer region paired with the semantic colours active while it was painted. */
case class ScenarioPaintedRegion(bounds: LayoutRect, foreground: Color, background: Color)

/** A rounded surface border submitted with its semantic focus or elevation colour. */
case class ScenarioBorder(bounds: LayoutRect, color: Color)

/** A text-style transition submitted while rendering a scenario frame. */
case class ScenarioStyleCall(action: String, style: TextStyle)

/** A layout hit target paired with the text bounds actually drawn into it. */
case class ScenarioDrawnItem(hitTarget: LayoutRect, textBounds: List[ScenarioDrawnText])

/** The actual preview draw bounds and source window used to compose an inline Markdown frame. */
case class ScenarioPreviewPlacement(firstSourceLine: Int, firstPreviewRow: Int, bounds: LayoutRect)

/** A rendered frame and the semantic evidence used to diagnose assertion failures. */
case class ScenarioFrame(image: BufferedImage, evidence: ScenarioFrameEvidence)

/** Deterministic state-pipeline and Java2D renderer driver for UI regression scenarios. */
final class UiScenarioDriver private (
    val stateManager: StateManager,
    val environment: UiScenarioEnvironment,
    artifactDirectory: Option[Path]
):

  private val codeFont = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val uiFont   = Font(Font.SANS_SERIF, Font.PLAIN, 12)

  def dispatch(event: com.serenity.keystroke.events.Event): IO[Unit] =
    stateManager.applyEvent(event)

  def updateState(update: AppState => AppState): IO[Unit] =
    stateManager.updateState(update)

  def state: IO[AppState] =
    stateManager.getCurrentState

  def advanceToSettled(maxTicks: Int = 256): IO[Boolean] =
    def loop(remaining: Int): IO[Boolean] =
      stateManager.advanceAnimationsOnTick().flatMap { active =>
        if !active then IO.pure(true)
        else if remaining <= 0 then IO.pure(false)
        else loop(remaining - 1)
      }
    loop(maxTicks)

  /** Render one frame and return state/layout evidence without consulting private renderer state. */
  def renderFrame(name: String): IO[ScenarioFrame] =
    state.map { current =>
      val logicalWidth  = environment.viewport.width * environment.cellMetrics.charWidth
      val logicalHeight = environment.viewport.height * environment.cellMetrics.lineHeight
      val image = new BufferedImage(
        Java2DRenderSurface.deviceImageDimension(logicalWidth, environment.deviceScale),
        Java2DRenderSurface.deviceImageDimension(logicalHeight, environment.deviceScale),
        BufferedImage.TYPE_INT_ARGB
      )
      val surface = new Java2DRenderSurface(
        image,
        environment.cellMetrics,
        codeFont,
        _ => (),
        logicalWidth,
        logicalHeight,
        environment.deviceScale,
        environment.deviceScale
      )
      val recordingSurface = new ScenarioRecordingSurface(surface, environment.cellMetrics)
      Renderer.render(
        current,
        cursorVisible = true,
        recordingSurface,
        environment.viewport,
        codeFont,
        codeFont,
        uiFont,
        environment.cellMetrics,
        environment.cellMetrics,
        cursorColor = None
      )
      val layout   = LayoutEngine.calculateLayoutWithUI(current, environment.viewport)
      val contract = EditorLayoutContract.from(current, environment.viewport, layout)
      val frame    = ScenarioFrame(image, evidenceFor(current, contract, image, recordingSurface))
      artifactDirectory.foreach { directory =>
        Files.createDirectories(directory)
        javax.imageio.ImageIO.write(image, "png", directory.resolve(s"$name.png").toFile)
      }
      frame
    }

  /** Assert a semantic frame contract and retain a diagnostic PNG on failure when configured. */
  def verifyFrame(name: String)(contract: ScenarioFrameEvidence => Either[String, Unit]): IO[ScenarioFrame] =
    renderFrame(name).flatMap { frame =>
      contract(frame.evidence) match
        case Right(_) => IO.pure(frame)
        case Left(reason) =>
          val diagnostic =
            s"$reason; focus=${frame.evidence.focus}; surfaces=${frame.evidence.surfaceRects}; " +
              s"items=${frame.evidence.itemRects}; previews=${frame.evidence.previewPlacements}; " +
              s"violations=${frame.evidence.layoutViolations}"
          IO.raiseError(new AssertionError(diagnostic))
    }

  private def evidenceFor(
    state: AppState,
    contract: EditorLayoutContract,
    image: BufferedImage,
    recordingSurface: ScenarioRecordingSurface
  ): ScenarioFrameEvidence =
    val surfaceRects =
      (contract.pinnedSurfaceRects ++ contract.expandedSurfaceRects) ++ contract.floatingOverlayRects.toMap
    val itemRects = surfaceRects.keys.map { surfaceId =>
      val rects = contract.panelRowSlots(surfaceId).collect {
        case SurfaceContentRowSlot(SurfaceContentRowKind.Item(_), y) =>
          LayoutRect(0, y, environment.viewport.width, 1)
      } ++ contract.overlayRowSlots(surfaceId).collect {
        case SurfaceContentRowSlot(SurfaceContentRowKind.Item(_), y) =>
          val content = contract.overlayContentRect(surfaceId).getOrElse(LayoutRect(0, y, 0, 1))
          LayoutRect(content.x, y, content.width, 1)
      }
      surfaceId -> rects
    }.toMap
    val mappings = state.buffers.map {
      case (bufferId, buffer) =>
        bufferId -> MarkdownBlockLens.activeBlockLineSet(
          buffer.content.toString.linesIterator.toVector,
          buffer.cursors.headOption.map(_.line)
        )
    }
    val previewPlacements = contract.workspace.paneLayouts.toList.flatMap {
      case (paneId, paneLayout) =>
        for
          pane       <- state.layout.editorPanes.get(paneId).toList
          bufferId   <- pane.bufferId.toList
          buffer     <- state.buffers.get(bufferId).toList
          drawnImage <- recordingSurface.drawnImages.find(_.bounds == paneLayout.contentRect).toList
          placement  <- previewPlacementFor(buffer, Some(drawnImage)).toList
        yield bufferId -> placement
    }.toMap
    val visiblePreviewSourceLines = previewPlacements.map {
      case (bufferId, placement) =>
        val buffer = state.buffers(bufferId)
        bufferId -> compositedPreviewSourceLines(image, state.theme.background, buffer, placement, mappings(bufferId))
    }
    val visibleText = state.focusedBufferId.toList.flatMap { bufferId =>
      state.buffers.get(bufferId).toList.flatMap { buffer =>
        buffer.content.toString.linesIterator.drop(buffer.viewport.topLine).take(buffer.viewport.visibleLines).toList
      }
    }
    val renderedContentRows =
      (0 until environment.viewport.height * environment.cellMetrics.lineHeight).collect {
        case row
            if (0 until environment.viewport.width * environment.cellMetrics.charWidth)
              .exists(column => image.getRGB(column, row) != state.theme.background.getRGB) =>
          row
      }.toSet
    val drawnItems = itemRects.view.mapValues { targets =>
      targets.map { target =>
        ScenarioDrawnItem(
          target,
          recordingSurface.drawnText.filter(text => target.containsRect(text.bounds))
        )
      }
    }.toMap
    ScenarioFrameEvidence(
      state.focus,
      surfaceRects,
      itemRects,
      mappings,
      previewPlacements,
      visiblePreviewSourceLines,
      visibleText,
      recordingSurface.drawnText,
      recordingSurface.paintedRegions,
      recordingSurface.borders,
      recordingSurface.styleCalls,
      drawnItems,
      recordingSurface.drawnImages.map(_.bounds),
      renderedContentRows,
      animationComplete = state.surfaceAnimations.values.forall(_.animationState.animations.isEmpty) &&
        state.buffers.values.forall(_.animations.animations.isEmpty),
      contract.violations
    )

  private def previewPlacementFor(buffer: Buffer, image: Option[ScenarioDrawnImage]): Option[ScenarioPreviewPlacement] =
    for
      drawnImage <- image
      if buffer.language.contains(com.serenity.lsp.config.LanguageId.Markdown)
    yield
      val lines       = buffer.content.linesFrom(0, buffer.content.lineCount)
      val activeLine  = buffer.cursors.headOption.map(_.line).filter(line => line >= 0 && line < lines.length)
      val activeBlock = activeLine.map(line => MarkdownBlockLens.currentBlock(lines, line))
      val sourceLimit = math.max(32, buffer.viewport.visibleLines.max(1) * 4)
      val viewportTop = buffer.viewport.topLine.max(0).min(math.max(0, lines.length - 1))
      val precedingHeading = activeLine
        .filter(line => line > 0 && lines(line).trim.isEmpty && lines(line - 1).trim.matches("^#{1,6}\\s+.*"))
        .map(_ - 1)
      val preferredTop = precedingHeading.orElse(activeBlock.map(_.start.min(viewportTop))).getOrElse(viewportTop)
      val firstSourceLine = activeBlock
        .map { block =>
          val limit = math.max(sourceLimit, block.end - block.start + 1)
          if block.end >= preferredTop + limit then (block.end - limit + 1).max(0) else preferredTop
        }
        .getOrElse(preferredTop)
      ScenarioPreviewPlacement(
        firstSourceLine,
        MarkdownDocumentPreview.previewRowForSourceLine(lines, firstSourceLine).getOrElse(firstSourceLine),
        drawnImage.bounds
      )

  private def compositedPreviewSourceLines(
    image: BufferedImage,
    background: Color,
    buffer: Buffer,
    placement: ScenarioPreviewPlacement,
    activeSourceLines: Set[Int]
  ): Set[Int] =
    MarkdownDocumentPreview
      .renderInlineDocument(buffer.content.linesFrom(0, buffer.content.lineCount))
      .zipWithIndex
      .collect {
        case (previewLine, previewRow)
            if previewLine.sourceLine.exists(source => !activeSourceLines.contains(source)) &&
              previewRow >= placement.firstPreviewRow &&
              finalPreviewRowHasContent(image, background, placement, previewRow - placement.firstPreviewRow) =>
          previewLine.sourceLine.get
      }
      .toSet

  private def finalPreviewRowHasContent(
    image: BufferedImage,
    background: Color,
    placement: ScenarioPreviewPlacement,
    localPreviewRow: Int
  ): Boolean =
    val scale = environment.deviceScale.max(1.0)
    val left  = math.floor(placement.bounds.x * environment.cellMetrics.charWidth * scale).toInt.max(0)
    val right = math.ceil(placement.bounds.right * environment.cellMetrics.charWidth * scale).toInt.min(image.getWidth)
    val top =
      math.floor((placement.bounds.y + localPreviewRow) * environment.cellMetrics.lineHeight * scale).toInt.max(0)
    val bottom = math
      .ceil((placement.bounds.y + localPreviewRow + 1) * environment.cellMetrics.lineHeight * scale)
      .toInt
      .min(image.getHeight)
    left < right && top < bottom && (top until bottom).exists { y =>
      (left until right).exists(x => image.getRGB(x, y) != background.getRGB)
    }

final private class ScenarioRecordingSurface(delegate: RenderSurface, metrics: CellMetrics) extends RenderSurface:
  private val drawnTextBuffer      = scala.collection.mutable.ListBuffer.empty[ScenarioDrawnText]
  private val drawnImageBuffer     = scala.collection.mutable.ListBuffer.empty[ScenarioDrawnImage]
  private val paintedRegionsBuffer = scala.collection.mutable.ListBuffer.empty[ScenarioPaintedRegion]
  private val bordersBuffer        = scala.collection.mutable.ListBuffer.empty[ScenarioBorder]
  private val styleCallsBuffer     = scala.collection.mutable.ListBuffer.empty[ScenarioStyleCall]
  private val foregroundColor      = AtomicReference(Color.BLACK)
  private val backgroundColor      = AtomicReference(Color.BLACK)

  def drawnText: List[ScenarioDrawnText] = drawnTextBuffer.toList

  def drawnImages: List[ScenarioDrawnImage] = drawnImageBuffer.toList

  def paintedRegions: List[ScenarioPaintedRegion] = paintedRegionsBuffer.toList

  def borders: List[ScenarioBorder] = bordersBuffer.toList

  def styleCalls: List[ScenarioStyleCall] = styleCallsBuffer.toList

  override def setFont(font: Font): Unit                    = delegate.setFont(font)
  override def fontRenderContext: Option[FontRenderContext] = delegate.fontRenderContext

  def setForegroundColor(color: Color): Unit =
    foregroundColor.set(color)
    delegate.setForegroundColor(color)

  def setBackgroundColor(color: Color): Unit =
    backgroundColor.set(color)
    delegate.setBackgroundColor(color)

  def getBackgroundColor: Color = delegate.getBackgroundColor

  def putString(x: Int, y: Int, text: String): Unit =
    recordText(text, LayoutRect(x, y, text.length.max(1), 1))
    recordPaint(LayoutRect(x, y, text.length.max(1), 1))
    delegate.putString(x, y, text)

  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit =
    recordPaint(LayoutRect(x, y, width, height))
    delegate.fillRect(x, y, width, height, char)

  def enableStyle(style: TextStyle): Unit =
    styleCallsBuffer += ScenarioStyleCall("enable", style)
    delegate.enableStyle(style)

  def disableStyle(style: TextStyle): Unit =
    styleCallsBuffer += ScenarioStyleCall("disable", style)
    delegate.disableStyle(style)

  override def setAlpha(alpha: Float): Unit = delegate.setAlpha(alpha)
  override def blurRegion(x: Int, y: Int, width: Int, height: Int, radius: Float): Unit =
    delegate.blurRegion(x, y, width, height, radius)
  override def applyPostProcessing(effect: com.serenity.config.PostProcessingEffect): Unit =
    delegate.applyPostProcessing(effect)
  override def devicePixelScaleX: Double = delegate.devicePixelScaleX
  override def devicePixelScaleY: Double = delegate.devicePixelScaleY

  override def strokeRoundRect(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    arcPx: Int,
    color: Color,
    strokeWidth: Float
  ): Unit =
    bordersBuffer += ScenarioBorder(LayoutRect(x, y, width, height), color)
    delegate.strokeRoundRect(x, y, width, height, arcPx, color, strokeWidth)

  def withRoundRectClip(x: Int, y: Int, width: Int, height: Int, arcPx: Int)(render: => Unit): Unit =
    delegate.withRoundRectClip(x, y, width, height, arcPx)(render)
  override def fillPixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: Color): Unit =
    delegate.fillPixelRect(xPx, yPx, widthPx, heightPx, color)

  override def drawRunPx(
    xPx: Float,
    yPx: Int,
    bgWidthPx: Float,
    lineHeightPx: Int,
    ascentPx: Int,
    text: String,
    clipGlyphToRun: Boolean = false
  ): Unit =
    val x     = math.floor(xPx / metrics.charWidth.max(1).toFloat).toInt
    val y     = math.floor(yPx / metrics.lineHeight.max(1).toFloat).toInt
    val width = math.ceil(bgWidthPx / metrics.charWidth.max(1).toFloat).toInt.max(1)
    recordText(text, LayoutRect(x, y, width, 1))
    recordPaint(LayoutRect(x, y, width, 1))
    delegate.drawRunPx(xPx, yPx, bgWidthPx, lineHeightPx, ascentPx, text, clipGlyphToRun)

  override def drawImage(image: BufferedImage, x: Int, y: Int, width: Int, height: Int): Unit =
    drawnImageBuffer += ScenarioDrawnImage(image, LayoutRect(x, y, width, height))
    delegate.drawImage(image, x, y, width, height)

  def hideCursor(): Unit  = delegate.hideCursor()
  def viewportWidth: Int  = delegate.viewportWidth
  def viewportHeight: Int = delegate.viewportHeight
  def flush(): Unit       = delegate.flush()

  private def recordText(text: String, bounds: LayoutRect): Unit =
    if text.nonEmpty then drawnTextBuffer += ScenarioDrawnText(text, bounds)

  private def recordPaint(bounds: LayoutRect): Unit =
    paintedRegionsBuffer += ScenarioPaintedRegion(bounds, foregroundColor.get, backgroundColor.get)

private case class ScenarioDrawnImage(image: BufferedImage, bounds: LayoutRect)

object UiScenarioDriver:

  def create(
    name: String,
    environment: UiScenarioEnvironment = UiScenarioEnvironment(),
    artifactDirectory: Option[Path] = None,
    initialConfig: com.serenity.config.AppConfig = com.serenity.config.AppConfig.default,
    uiPresetStore: Option[UiPresetStore] = None,
    isolatedConfig: Boolean = false,
    sessionRoot: Option[Path] = None
  )(using Balance): IO[UiScenarioDriver] =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger
    for
      configuredSessionRoot <- sessionRoot.fold(IO.blocking(Files.createTempDirectory(s"$name-ui-scenario")))(IO.pure)
      configuredInitialConfig <-
        if isolatedConfig then IO.blocking(ConfigManager.loadConfig(Some(isolatedConfigPath.toString)))
        else IO.pure(initialConfig)
      manager <- StateManager(
        logger,
        onFontConfigChanged = (_: FontConfig) => IO.unit,
        deviceTextScaleProvider = IO.pure(environment.deviceScale),
        sessionRootOverride = Some(configuredSessionRoot),
        initialConfig = configuredInitialConfig,
        uiPresetStore = uiPresetStore.getOrElse(UiPresetStore.default)
      )
      _ <- manager.handleViewportResize(environment.viewport)
      _ <- manager.updateState(_.copy(theme = themeFor(environment.themeName)))
    yield new UiScenarioDriver(manager, environment, artifactDirectory)

  private def themeFor(name: String): com.serenity.ui.theme.Theme =
    if name.equalsIgnoreCase("light") then com.serenity.ui.theme.Theme.light
    else com.serenity.ui.theme.Theme.dark

  private def isolatedConfigPath: Path =
    Path.of(getClass.getResource("/ui-scenarios/isolated-ui.conf").toURI)
