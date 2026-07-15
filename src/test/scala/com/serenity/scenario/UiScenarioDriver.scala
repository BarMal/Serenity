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
    val layout                = LayoutEngine.calculateLayout(state, viewport)
    val contract              = EditorLayoutContract.from(state, viewport, layout)
    val markdownFrameEvidence = markdownEvidence(state, contract)
    val semantics             = semanticEvidence(state, layout, contract)
    ScenarioFrame(
      image = image,
      activeFocus = state.focus,
      surfaceRects = semantics.surfaceRects,
      itemRects = semantics.itemRects,
      visibleText = semantics.visibleText,
      markdownRows = markdownFrameEvidence.rows,
      markdownLenses = markdownFrameEvidence.lenses,
      settled = !state.buffers.values.exists(_.animations.hasActiveAnimations) &&
        state.themeTransition.isEmpty && state.surfaceAnimations.isEmpty,
      diagnostics = contract.violations.map(_.toString) ++ semantics.itemRects.toList.flatMap {
        case (surfaceId, items) =>
          items.collect {
            case item if item.renderedRect != item.hitRect =>
              s"surface=$surfaceId label=${item.label} rendered=${item.renderedRect} hit=${item.hitRect}"
          }
      }
    )

  private def semanticEvidence(
    state: AppState,
    layout: CalculatedLayout,
    contract: EditorLayoutContract
  ): SemanticEvidence =
    val renderedOverlays = OverlayViewModel.fromState(state, layout)
    val overlays = renderedOverlays.aboveCursor.toList ++
      (if renderedOverlays.belowCursorStack.nonEmpty then renderedOverlays.belowCursorStack
       else renderedOverlays.belowCursor.toList)
    overlays.foldLeft(SemanticEvidence.empty) { (evidence, overlay) =>
      overlay.surfaceId
        .map { surfaceId =>
          val renderedSlots = overlay.contentRowSlots.collect {
            case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), y) => index -> y
          }
          val hitSlots = rowSlots(surfaceId, contract).collect {
            case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), y) => index -> y
          }.toMap
          val rows = state
            .surfaceById(surfaceId)
            .flatMap {
              _.content match
                case SurfaceContent.ContextualToolbar(toolbarState) =>
                  Some(contextualToolbarItemRects(state, toolbarState, overlay))
                case _ => None
            }
            .getOrElse {
              renderedSlots.flatMap {
                case (index, y) =>
                  overlay.rows.lift(index).map { row =>
                    ScenarioItemRect(
                      row.plainText,
                      LayoutRect(overlay.resolvedContentRect.x, y, overlay.resolvedContentRect.width, 1),
                      LayoutRect(
                        contract.overlayContentRect(surfaceId).map(_.x).getOrElse(Int.MinValue),
                        hitSlots.getOrElse(index, Int.MinValue),
                        contract.overlayContentRect(surfaceId).map(_.width).getOrElse(0),
                        1
                      )
                    )
                  }
              }
            }
          evidence.copy(
            surfaceRects = evidence.surfaceRects.updated(surfaceId, overlay.rect),
            itemRects = evidence.itemRects.updated(surfaceId, rows),
            visibleText = evidence.visibleText ++ overlay.header.toList.map(_.plainText) ++ overlay.rows
              .map(_.plainText) ++ overlay.footer.toList.map(_.plainText)
          )
        }
        .getOrElse(evidence)
    }

  private def contextualToolbarItemRects(
    state: AppState,
    toolbarState: ContextualToolbarState,
    overlay: TextOverlayView
  ): List[ScenarioItemRect] =
    val contentRect = overlay.resolvedContentRect
    val items       = ContextualToolbar.itemsFor(state)
    val groups      = ContextualToolbar.rowGroups(items, contentRect.width, toolbarState.displayMode)
    groups.zipWithIndex.flatMap {
      case (group, rowIndex) =>
        val widths       = ContextualToolbar.itemCellWidths(group, contentRect.width, toolbarState.displayMode)
        val globalOffset = groups.take(rowIndex).map(_.length).sum
        group
          .zip(widths)
          .zipWithIndex
          .foldLeft((contentRect.x, List.empty[ScenarioItemRect])) {
            case ((x, evidence), ((item, width), localIndex)) =>
              val globalIndex = globalOffset + localIndex
              val hitColumns = (0 until contentRect.width).filter { column =>
                ContextualToolbar
                  .hitAt(rowIndex, column, contentRect.width, toolbarState, state)
                  .contains(ContextualToolbarHit.TopLevelItem(globalIndex))
              }
              val hitRect = hitColumns.headOption
                .map(first => LayoutRect(contentRect.x + first, contentRect.y + rowIndex, hitColumns.length, 1))
                .getOrElse(LayoutRect(Int.MinValue, Int.MinValue, 0, 0))
              val renderedRect = LayoutRect(x, contentRect.y + rowIndex, width, 1)
              val separator = Option
                .when(ContextualToolbar.hasTrailingGroupSeparator(item, group.lift(localIndex + 1)))(1)
                .getOrElse(0)
              val gap = Option.when(localIndex < group.length - 1)(1).getOrElse(0)
              x + width + separator + gap -> (evidence :+ ScenarioItemRect(item.id, renderedRect, hitRect))
          }
          ._2
    }

  private def surfaceRect(surface: UiSurface, layout: CalculatedLayout): Option[LayoutRect] =
    EditorLayoutContract.panelRectFor(surface, layout).orElse(EditorLayoutContract.overlayRectFor(surface.id, layout))

  private def rowSlots(surfaceId: SurfaceId, contract: EditorLayoutContract): List[SurfaceContentRowSlot] =
    (contract.panelRowSlots(surfaceId) ++ contract.overlayRowSlots(surfaceId)).collect {
      case slot @ SurfaceContentRowSlot(SurfaceContentRowKind.Item(_), _) => slot
    }

  private def markdownEvidence(state: AppState, contract: EditorLayoutContract): Renderer.MarkdownLensEvidence =
    val evidence =
      state.layout.editorPanes.toVector.flatMap {
        case (paneId, pane) =>
          for
            paneLayout <- contract.workspace.paneLayouts.get(paneId).toVector
            bufferId   <- pane.bufferId.toVector
            buffer     <- state.buffers.get(bufferId).toVector
            if buffer.language.contains(LanguageId.Markdown)
          yield
            val viewport =
              LayoutEngine.updateBufferViewportDimensions(buffer, paneLayout.contentRect, state.config.wordWrapEnabled)
            val snapshot = TextLayoutSnapshot.fromBuffer(
              buffer.copy(viewport = viewport),
              paneLayout.contentRect.width * cellMetrics.charWidth,
              textFont,
              TextLayoutSnapshot.defaultFontRenderContext(),
              wordWrapEnabled = state.config.wordWrapEnabled
            )
            Renderer.markdownLensEvidence(buffer, paneLayout.contentRect, snapshot)
      }
    Renderer.MarkdownLensEvidence(evidence.flatMap(_.rows), evidence.flatMap(_.lenses))

  private def writeFrame(image: BufferedImage): IO[Option[Path]] =
    IO.blocking {
      Files.createDirectories(diagnosticDirectory)
      val target = diagnosticDirectory.resolve("ui-scenario-frame.png")
      ImageIO.write(image, "png", target.toFile)
      Some(target)
    }

object UiScenarioDriver:

  case class ScenarioItemRect(label: String, renderedRect: LayoutRect, hitRect: LayoutRect)

  case class ScenarioFrame(
      image: BufferedImage,
      activeFocus: Focus,
      surfaceRects: Map[SurfaceId, LayoutRect],
      itemRects: Map[SurfaceId, List[ScenarioItemRect]],
      visibleText: List[String],
      markdownRows: Vector[Renderer.MarkdownLensRenderedRow],
      markdownLenses: Vector[Renderer.MarkdownLensRect],
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
    diagnosticDirectory: Path = Path.of("test-results", "ui-scenarios"),
    sessionRootOverride: Option[Path] = None
  ): IO[UiScenarioDriver] =
    given Balance           = Balance.default
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val metrics             = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 12)
    for
      sessionRoot <- sessionRootOverride.fold(IO.blocking(Files.createTempDirectory("ui-scenario-session")))(IO.pure)
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
