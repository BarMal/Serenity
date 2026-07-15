package com.serenity.scenario

import java.awt.Font
import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import javax.imageio.ImageIO

import cats.effect.{IO, Ref}
import com.serenity.command.Command
import com.serenity.config.MarkdownViewMode
import com.serenity.keystroke.events.{Event, MouseClick, ResizeEvent}
import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.MarkdownDocumentPreview
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.renderer.*
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Deterministic end-to-end driver for state, event, layout, and Java2D frame scenarios. */
final class UiScenarioDriver private (
    stateManager: StateManager,
    viewportRef: Ref[IO, ViewportSize],
    val cellMetrics: CellMetrics,
    val deviceScale: Double,
    diagnosticDirectory: Path
):
  import UiScenarioDriver.*

  private val codeFont = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val uiFont   = Font(Font.SANS_SERIF, Font.PLAIN, 12)

  /** Creates a buffer and makes it the active pane's document. */
  def createBuffer(content: String): IO[BufferId] =
    for
      bufferId <- stateManager.createBuffer(content)
      state    <- stateManager.getCurrentState
      _ <- state.layout.activeEditorPaneId match
        case Some(paneId) => stateManager.setBufferForPane(paneId, bufferId)
        case None         => IO.unit
    yield bufferId

  /** Applies document language and lens mode through the public state manager contract. */
  def configureBuffer(bufferId: BufferId, language: Option[LanguageId], markdownViewMode: MarkdownViewMode): IO[Unit] =
    stateManager.updateState { state =>
      val buffers = state.buffers.updatedWith(bufferId)(_.map(_.copy(language = language)))
      state.copy(buffers = buffers, config = state.config.withMarkdownViewMode(markdownViewMode))
    }

  def setCursor(paneId: PaneId, cursor: CursorPosition): IO[Unit] =
    stateManager.setCursorPosition(paneId, cursor.line, cursor.column)

  /** Reads the public immutable app state for assertions that cannot be rendered as geometry. */
  def snapshot: IO[AppState] = stateManager.getCurrentState

  /** Applies a public state setup used by a scenario fixture. */
  def updateState(update: AppState => AppState): IO[Unit] = stateManager.updateState(update)

  /** Executes a command through the real command interpreter. */
  def execute(command: Command): IO[Unit] = stateManager.executeCommand(command)

  /** Routes an input event through the real application event pipeline. */
  def dispatch(event: Event): IO[Unit] =
    event match
      case ResizeEvent(size) => viewportRef.set(size) >> stateManager.handleViewportResize(size)
      case other             => stateManager.applyEvent(other)

  def click(column: Int, row: Int): IO[Unit] =
    dispatch(MouseClick(column, row))

  /** Advances deterministic animation ticks until no renderer-visible animation remains. */
  def advanceUntilSettled(maxTicks: Int = 240): IO[Boolean] =
    def loop(remaining: Int): IO[Boolean] =
      if remaining <= 0 then IO.pure(false)
      else
        stateManager.advanceAnimationsOnTick().flatMap(active => if active then loop(remaining - 1) else IO.pure(true))
    loop(maxTicks)

  /** Renders one Java2D frame and captures semantic geometry alongside the pixels. */
  def render(writeDiagnostic: Boolean = false): IO[ScenarioFrame] =
    for
      viewport <- viewportRef.get
      state    <- stateManager.getCurrentState
      frame = renderFrame(state, viewport)
      diagnostic <-
        if writeDiagnostic then writeFrame(frame.image)
        else IO.pure(None)
    yield frame.copy(diagnosticPng = diagnostic)

  private def renderFrame(state: AppState, viewport: ViewportSize): ScenarioFrame =
    val logicalWidth  = viewport.width * cellMetrics.charWidth
    val logicalHeight = viewport.height * cellMetrics.lineHeight
    val image = new BufferedImage(
      Java2DRenderSurface.deviceImageDimension(logicalWidth, deviceScale),
      Java2DRenderSurface.deviceImageDimension(logicalHeight, deviceScale),
      BufferedImage.TYPE_INT_ARGB
    )
    val surface = new Java2DRenderSurface(
      image,
      cellMetrics,
      codeFont,
      _ => (),
      logicalWidth,
      logicalHeight,
      deviceScale,
      deviceScale
    )
    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      viewport,
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      cellMetrics,
      None
    )
    val layout    = LayoutEngine.calculateLayout(state, viewport)
    val contract  = EditorLayoutContract.from(state, viewport, layout)
    val semantics = semanticEvidence(state, layout, contract)
    ScenarioFrame(
      image = image,
      activeFocus = state.focus,
      surfaceRects = semantics.surfaceRects,
      itemRects = semantics.itemRects,
      visibleText = semantics.visibleText,
      markdownMappings = markdownMappings(state),
      settled = !state.buffers.values.exists(_.animations.hasActiveAnimations) &&
        state.themeTransition.isEmpty && state.surfaceAnimations.isEmpty,
      diagnostics = contract.violations.map(_.toString)
    )

  private def semanticEvidence(
    state: AppState,
    layout: CalculatedLayout,
    contract: EditorLayoutContract
  ): SemanticEvidence =
    state.uiSurfaces.foldLeft(SemanticEvidence.empty) { (evidence, surface) =>
      surfaceRect(surface, layout)
        .map { rect =>
          val mode = surface.presentation match
            case SurfacePresentation.Floating(_, _) => SurfaceRenderMode.Floating
            case _                                  => SurfaceRenderMode.Pinned
          val resolved =
            SurfaceContentResolver.resolve(surface.content, rect, mode, state.config.commandRunnerItemGapRows)
          val rows = rowSlots(surface.id, contract).zip(resolved.rows).map {
            case (slot, row) =>
              ScenarioItemRect(row.plainText, LayoutRect(rect.x, slot.y, rect.width, 1))
          }
          evidence.copy(
            surfaceRects = evidence.surfaceRects.updated(surface.id, rect),
            itemRects = evidence.itemRects.updated(surface.id, rows),
            visibleText = evidence.visibleText ++ resolved.header.toList.map(_.plainText) ++ resolved.rows
              .map(_.plainText) ++ resolved.footer.toList.map(_.plainText)
          )
        }
        .getOrElse(evidence)
    }

  private def surfaceRect(surface: UiSurface, layout: CalculatedLayout): Option[LayoutRect] =
    EditorLayoutContract.panelRectFor(surface, layout).orElse(EditorLayoutContract.overlayRectFor(surface.id, layout))

  private def rowSlots(surfaceId: SurfaceId, contract: EditorLayoutContract): List[SurfaceContentRowSlot] =
    (contract.panelRowSlots(surfaceId) ++ contract.overlayRowSlots(surfaceId)).collect {
      case slot @ SurfaceContentRowSlot(SurfaceContentRowKind.Item(_), _) => slot
    }

  private def markdownMappings(state: AppState): Vector[MarkdownMapping] =
    state.buffers.values.toVector.flatMap { buffer =>
      Option
        .when(buffer.language.contains(LanguageId.Markdown)) {
          val source = buffer.content.collect().linesIterator.toVector
          MarkdownDocumentPreview.renderInlineDocument(source).zipWithIndex.flatMap {
            case (line, previewRow) =>
              line.sourceLine.map(sourceLine => MarkdownMapping(sourceLine, previewRow, line.text))
          }
        }
        .getOrElse(Vector.empty)
    }

  private def writeFrame(image: BufferedImage): IO[Option[Path]] =
    IO.blocking {
      Files.createDirectories(diagnosticDirectory)
      val target = diagnosticDirectory.resolve("ui-scenario-frame.png")
      ImageIO.write(image, "png", target.toFile)
      Some(target)
    }

object UiScenarioDriver:

  case class ScenarioItemRect(label: String, rect: LayoutRect)

  case class MarkdownMapping(sourceLine: Int, previewRow: Int, text: String)

  case class ScenarioFrame(
      image: BufferedImage,
      activeFocus: Focus,
      surfaceRects: Map[SurfaceId, LayoutRect],
      itemRects: Map[SurfaceId, List[ScenarioItemRect]],
      visibleText: List[String],
      markdownMappings: Vector[MarkdownMapping],
      settled: Boolean,
      diagnostics: List[String],
      diagnosticPng: Option[Path] = None
  )

  private case class SemanticEvidence(
      surfaceRects: Map[SurfaceId, LayoutRect],
      itemRects: Map[SurfaceId, List[ScenarioItemRect]],
      visibleText: List[String]
  )

  private object SemanticEvidence:
    val empty: SemanticEvidence = SemanticEvidence(Map.empty, Map.empty, Nil)

  def create(
    viewport: ViewportSize = ViewportSize(120, 36),
    deviceScale: Double = 1.0,
    diagnosticDirectory: Path = Path.of("test-results", "ui-scenarios")
  ): IO[UiScenarioDriver] =
    given Balance           = Balance.default
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val metrics             = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 12)
    for
      sessionRoot <- IO.blocking(Files.createTempDirectory("ui-scenario-session"))
      viewportRef <- Ref.of[IO, ViewportSize](viewport)
      logger      = LoggerFactory[IO].getLogger(using LoggerName("UiScenarioDriver"))
      presetStore = UiPresetStore(sessionRoot.resolve("ui-presets.json"))
      stateManager <- StateManager.apply(
        logger,
        deviceTextScaleProvider = IO.pure(deviceScale),
        sessionRootOverride = Some(sessionRoot),
        uiPresetStore = presetStore
      )
      _ <- stateManager.handleViewportResize(viewport)
    yield UiScenarioDriver(stateManager, viewportRef, metrics, deviceScale.max(1.0), diagnosticDirectory)
