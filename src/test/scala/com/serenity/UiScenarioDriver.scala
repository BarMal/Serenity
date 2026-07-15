package com.serenity

import java.awt.Font
import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}

import cats.effect.IO
import com.serenity.markdown.MarkdownBlockLens
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.renderer.{Java2DRenderSurface, Renderer}
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
    visibleText: List[String],
    animationComplete: Boolean,
    layoutViolations: List[LayoutContractViolation]
)

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
      Renderer.render(
        current,
        cursorVisible = true,
        surface,
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
      val frame    = ScenarioFrame(image, evidenceFor(current, contract))
      artifactDirectory.foreach { directory =>
        Files.createDirectories(directory)
        javax.imageio.ImageIO.write(image, "png", directory.resolve(s"$name.png").toFile)
      }
      frame
    }

  private def evidenceFor(state: AppState, contract: EditorLayoutContract): ScenarioFrameEvidence =
    val surfaceRects =
      (contract.pinnedSurfaceRects ++ contract.expandedSurfaceRects) ++ contract.floatingOverlayRects.toMap
    val itemRects = surfaceRects.keys.map { surfaceId =>
      val rects = contract.panelRowSlots(surfaceId).map(slot => LayoutRect(0, slot.y, environment.viewport.width, 1)) ++
        contract.overlayRowSlots(surfaceId).map { slot =>
          val content = contract.overlayContentRect(surfaceId).getOrElse(LayoutRect(0, slot.y, 0, 1))
          LayoutRect(content.x, slot.y, content.width, 1)
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
    val visibleText = state.focusedBufferId.toList.flatMap { bufferId =>
      state.buffers.get(bufferId).toList.flatMap { buffer =>
        buffer.content.toString.linesIterator.drop(buffer.viewport.topLine).take(buffer.viewport.visibleLines).toList
      }
    }
    ScenarioFrameEvidence(
      state.focus,
      surfaceRects,
      itemRects,
      mappings,
      visibleText,
      animationComplete = state.surfaceAnimations.values.forall(_.animationState.animations.isEmpty) &&
        state.buffers.values.forall(_.animations.animations.isEmpty),
      contract.violations
    )

object UiScenarioDriver:

  def create(
    name: String,
    environment: UiScenarioEnvironment = UiScenarioEnvironment(),
    artifactDirectory: Option[Path] = None,
    initialConfig: com.serenity.config.AppConfig = com.serenity.config.AppConfig.default,
    uiPresetStore: Option[UiPresetStore] = None
  )(using Balance): IO[UiScenarioDriver] =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger
    for
      sessionRoot <- IO.blocking(Files.createTempDirectory(s"$name-ui-scenario"))
      manager <- StateManager(
        logger,
        onFontConfigChanged = (_: FontConfig) => IO.unit,
        deviceTextScaleProvider = IO.pure(environment.deviceScale),
        sessionRootOverride = Some(sessionRoot),
        initialConfig = initialConfig,
        uiPresetStore = uiPresetStore.getOrElse(UiPresetStore.default)
      )
      _ <- manager.handleViewportResize(environment.viewport)
    yield new UiScenarioDriver(manager, environment, artifactDirectory)
