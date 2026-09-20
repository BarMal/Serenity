package com.serenity.state.manager

import java.awt.Font
import java.util.LinkedHashMap

import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.{AppConfig, InterfaceDensity, StatusLineConfig, TextAreaInsets}
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.RichTextDocument
import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.*

final private[manager] case class RopeIdentity private (value: Rope):

  override def equals(obj: Any): Boolean =
    obj match
      case that: RopeIdentity => (value: AnyRef) eq (that.value: AnyRef)
      case _                  => false

  override def hashCode(): Int =
    System.identityHashCode(value: AnyRef)

private[manager] object RopeIdentity:
  def apply(value: Rope): RopeIdentity =
    new RopeIdentity(value)

/** A surface's identity and presentation, plus only the content fields that actually feed its
  * [[com.serenity.ui.layout.LayoutEngine]] geometry (frame size/position). Command-palette surfaces carry a
  * `CommandRunner` that changes on every keystroke (search text, selection, edit state) without affecting layout, so
  * comparing full [[SurfaceContent]] equality here would defeat scene caching for the most input-heavy overlay in the
  * app. Every other surface kind keeps its full content, since several of them (context menus, comment lens, directory
  * listings, etc.) do size themselves from content.
  */
final private[manager] case class SurfaceGeometryKey(
    id: SurfaceId,
    presentation: SurfacePresentation,
    contentKey: Any
)

private[manager] object SurfaceGeometryKey:

  def from(surface: UiSurface): SurfaceGeometryKey =
    val contentKey: Any = surface.content match
      case SurfaceContent.CommandPalette(_) =>
        // Verified against every LayoutEngine consumer of CommandPalette content: none read the runner's
        // fields, only the surface's presence/type, so no runner-derived data belongs in the geometry key.
        "command-palette"
      case SurfaceContent.StartPage(_) =>
        // LayoutEngine.calculateFloatingSurfaceHeight/Width for StartPage never read the page's content: height
        // is unconditionally maxHeight and width is content-independent. Without this, the page's selectedIndex
        // (which changes on every arrow-key press) would defeat this cache on the very next mouse-hit-testing
        // call, forcing a full LayoutEngine.calculateLayoutWithUI rebuild -- the same class of bug #932 fixed for
        // the command palette's search text, but for the startup screen's own navigation.
        "start-page"
      case other =>
        other
    SurfaceGeometryKey(surface.id, surface.presentation, contentKey)

final private[manager] case class MouseTargetLayoutKey(
    viewportSize: ViewportSize,
    fontConfig: FontConfig,
    showLineNumbers: Boolean,
    wordWrapEnabled: Boolean,
    // Column-based document layout (issue #1338): these feed `AuthoritativeUiScene.forState`'s column-aware viewport
    // narrowing and `fromBufferColumn` snapshot, so a live column-mode toggle (or a target-width/gap change) must
    // invalidate the cached scene -- without them a toggle re-uses the full-width scene and has no visible effect.
    columnModeEnabled: Boolean,
    columnTargetWidthCells: Int,
    columnGap: Int,
    minimumPaneWidth: Int,
    textAreaInsets: TextAreaInsets,
    interfaceDensity: InterfaceDensity,
    uiElementGap: Double,
    showPaneHeaders: Boolean,
    statusLine: StatusLineConfig,
    commandRunnerVisibleRows: Int,
    commandRunnerItemGapRows: Double,
    commandRunnerCursorGapRows: Double,
    layoutState: Layout,
    focus: Focus,
    focusPaneId: Option[PaneId],
    orderedPaneIds: List[PaneId],
    paneBuffers: List[(PaneId, Option[BufferId])],
    paneSnapshotInputs: List[
      (
        PaneId,
        Option[(RopeIdentity, Viewport, TypographyRole, Option[LanguageId], Option[RichTextDocument])]
      )
    ],
    uiSurfaces: List[SurfaceGeometryKey],
    // The blocking modal layer (#814) lives outside `uiSurfaces`, so without it here two states that differ only in
    // which dialog is open share a scene key -- reopening a dialog after dismissing one returns the first's cached
    // scene, whose node references the now-gone surface id, and the renderer paints an empty frame (blank screen).
    modalStack: List[ModalDialog],
    derivedStatusLineSurface: Option[UiSurface],
    pinnedPanels: List[(SurfaceId, PanelPosition, Int)],
    lineNumberContent: List[(BufferId, RopeIdentity)]
)

private[manager] object MouseTargetLayoutKey:

  /** The state fields [[from]] actually reads. Everything else it derives (focusPaneId, orderedPaneIds, paneBuffers,
    * paneSnapshotInputs, pinnedPanels, lineNumberContent, derivedStatusLineSurface) is a pure function of these plus
    * viewportSize, so if none of these references changed since the last call, the previously computed key is still
    * correct and the full pane/buffer/surface walk can be skipped.
    */
  final private case class FastPathInputs(
      viewportSize: ViewportSize,
      config: AppConfig,
      layout: Layout,
      focus: Focus,
      buffers: Map[BufferId, Buffer],
      uiSurfaces: List[UiSurface],
      modalStack: List[ModalDialog]
  )

  /** `AtomicReference`-backed single-slot memo of the last [[MouseTargetLayoutKey]] computed, keyed on the
    * [[FastPathInputs]] it was computed from. `from` is called synchronously from mouse-hit-testing
    * (`ContextualToolbarHitTesting`, `CommandRunnerMouseHitTesting`, `MouseHitTestGeometry`,
    * `PinnedPanelMouseHitTesting`, `EditorContextMenuHitTesting`) and from the renderer's own scene preparation
    * (`AuthoritativeUiScene.forState` below) -- none of these run inside an IO fiber, so a `Ref[IO, ...]` here would
    * need forcing via `unsafeRunSync` right back into a synchronous `def` at every call site, hiding a plain
    * compare-and-set behind an effect type nothing here ever suspends on. This is the same reasoning already applied to
    * [[com.serenity.ui.renderer.RendererFrameState.BoundedRefCache]] (#1431/#1434) and to `ThemeManager`'s
    * highlight/lex caches (#1412/#1431/#1434): `AtomicReference` gives the same lock-free CAS semantics those modules
    * settled on, without threading `IO` through the entire mouse-targeting/rendering call graph.
    */
  private val lastComputation =
    new java.util.concurrent.atomic.AtomicReference[Option[(FastPathInputs, MouseTargetLayoutKey)]](None)

  private def unchangedSince(previous: FastPathInputs, current: FastPathInputs): Boolean =
    (previous.viewportSize == current.viewportSize) &&
      previous.config.eq(current.config) &&
      previous.layout.eq(current.layout) &&
      (previous.focus == current.focus) &&
      previous.buffers.eq(current.buffers) &&
      previous.uiSurfaces.eq(current.uiSurfaces) &&
      previous.modalStack.eq(current.modalStack)

  def from(state: AppState, viewportSize: ViewportSize): MouseTargetLayoutKey =
    val inputs = FastPathInputs(
      viewportSize,
      state.persisted.config,
      state.persisted.layout,
      state.persisted.focus,
      state.persisted.buffers,
      state.runtime.uiSurfaces,
      state.runtime.modalStack
    )
    lastComputation.get() match
      case Some((previousInputs, previousResult)) if unchangedSince(previousInputs, inputs) =>
        previousResult
      case _ =>
        val computed = compute(state, viewportSize)
        lastComputation.set(Some(inputs -> computed))
        computed

  private def compute(state: AppState, viewportSize: ViewportSize): MouseTargetLayoutKey =
    MouseTargetLayoutKey(
      viewportSize = viewportSize,
      fontConfig = state.persisted.config.editorConfig.fontConfig,
      showLineNumbers = state.persisted.config.surfaceConfig.showLineNumbers,
      wordWrapEnabled = state.persisted.config.surfaceConfig.wordWrapEnabled,
      columnModeEnabled = state.persisted.config.surfaceConfig.columnModeEnabled,
      columnTargetWidthCells = state.persisted.config.surfaceConfig.columnTargetWidthCells,
      columnGap = state.persisted.config.surfaceConfig.columnGap,
      minimumPaneWidth = state.persisted.config.editorConfig.minimumPaneWidth,
      textAreaInsets = state.persisted.config.surfaceConfig.textAreaInsets,
      interfaceDensity = state.persisted.config.interfaceDensity,
      uiElementGap = state.persisted.config.uiElementGap,
      showPaneHeaders = state.persisted.config.surfaceConfig.showPaneHeaders,
      statusLine = state.persisted.config.statusLine,
      commandRunnerVisibleRows = state.persisted.config.effectiveCommandRunnerVisibleRows,
      commandRunnerItemGapRows = state.persisted.config.effectiveCommandRunnerItemGapRows,
      commandRunnerCursorGapRows = state.persisted.config.effectiveCommandRunnerCursorGapRows,
      layoutState = state.persisted.layout,
      focus = state.persisted.focus,
      focusPaneId = state.persisted.focus match
        case Focus.EditorPane(paneId) if state.persisted.layout.editorPanes.contains(paneId) => Some(paneId)
        case _                                                                               => None,
      orderedPaneIds = state.persisted.layout.orderedPaneIds,
      paneBuffers = state.persisted.layout.orderedPaneIds.map(paneId =>
        paneId -> state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId)
      ),
      paneSnapshotInputs = state.persisted.layout.orderedPaneIds.map { paneId =>
        paneId -> state.persisted.layout.editorPanes
          .get(paneId)
          .flatMap(_.bufferId)
          .flatMap(state.persisted.buffers.get)
          .map(buffer =>
            (
              RopeIdentity(buffer.document.content),
              buffer.viewport,
              buffer.typographyRole,
              buffer.document.language,
              buffer.richText.richTextDocument
            )
          )
      },
      uiSurfaces = state.runtime.uiSurfaces.map(SurfaceGeometryKey.from),
      modalStack = state.runtime.modalStack,
      derivedStatusLineSurface = state.floatingStatusLineSurface,
      pinnedPanels = state.persisted.layout.workspaceTree.toList.flatMap { tree =>
        tree.dockedSurfaceIds.flatMap { id =>
          for
            position <- tree.positionForSurface(id)
            size     <- tree.currentSize(id, state.runtime.viewportSize)
          yield (id, position, size)
        }
      },
      lineNumberContent =
        if state.persisted.config.surfaceConfig.showLineNumbers then
          state.persisted.buffers.toList
            .sortBy(_._1.value)
            .map((bufferId, buffer) => bufferId -> RopeIdentity(buffer.document.content))
        else Nil
    )

/** The single owner of the prepared scene shared by rendering and mouse targeting. */
private[serenity] object AuthoritativeUiScene:

  final private case class SceneFontKey(family: String, style: Int, size: Float)

  final private case class SceneKey(
      layout: MouseTargetLayoutKey,
      paneFonts: List[(PaneId, SceneFontKey)],
      cellMetrics: Option[CellMetrics]
  )

  /** Bounded, `LinkedHashMap`(access-order) + `synchronized`-backed cache of the prepared scene, shared by rendering
    * and mouse targeting. `forState` below is called synchronously from the render entry points (`RendererEntryPoints`,
    * `RendererCursorOverlay`) and from every mouse-hit-testing call site (see [[forState]]'s doc comment) -- none of
    * them run inside an IO fiber, so this follows the same `Ref[IO, ...]`-was-tried-and-reverted reasoning as
    * [[com.serenity.ui.renderer.RendererFrameState.BoundedRefCache]] (#1431/#1434) and `ThemeManager`'s highlight/lex
    * caches (#1412/#1431/#1434): a synchronous API here would just force any `Ref[IO, ...]` back out via
    * `unsafeRunSync` at every call site, hiding a plain mutable map behind an effect type nothing here ever suspends
    * on. `synchronized` (rather than a lock-free CAS loop) is fine here because unlike those two modules this cache's
    * whole value -- a `LinkedHashMap` in access-order mode -- is itself mutable and non-swappable-by-reference, so
    * there is no immutable snapshot to CAS between; the critical sections are short (a `get` or a `put`), so contention
    * is not a concern in the paint/hit-testing hot path this serves.
    *
    * 64 is a conservative round number, not a measured bound: it comfortably covers every geometry/font/cell-metrics
    * combination a single window session realistically cycles through (a handful of panes times a handful of
    * font-size/theme/viewport combinations), mirroring the same capacity
    * [[com.serenity.ui.renderer.RendererFrameState.cacheCapacity]] uses for its own "no real bound, but must not grow
    * forever" caches. Raise it if a session with many concurrently open panes or frequent geometry changes sees
    * avoidable extra scene rebuilds from eviction churn.
    */
  private val prepared = new LinkedHashMap[SceneKey, UiSceneSnapshot](16, 0.75f, true):
    override def removeEldestEntry(
      eldest: java.util.Map.Entry[SceneKey, UiSceneSnapshot]
    ): Boolean =
      size() > 64

  /** `cellMetrics` is the "pixel" unit this scene's cell-based (non-measured) `TextLayoutSnapshot`s are built in --
    * `None` (every caller but the render entry points below) keeps this scene's long-standing behaviour of re-deriving
    * it from `codeFont`/each buffer's own font. A caller with its own notion of what a pixel means here -- `Renderer`,
    * rendering for a surface with no real `FontRenderContext` to measure against (TUI's `TerminalRenderSurface`, where
    * a pixel is defined to be exactly one terminal cell) -- passes that explicitly instead, so the snapshot this scene
    * hands back for painting (and for mouse hit-testing, which shares it) is expressed in the same unit the caller's
    * cursor-positioning math actually uses. `Some` here also forces `TextLayoutSnapshot`'s cell (non-measured) layout
    * path unconditionally: such a caller has no font rendering to measure with at all (#1105), so the font's own
    * measured-vs-cell auto-detection -- which can trip on a "monospaced" font's own rendering-stack quirks, independent
    * of whether the caller can draw a measured run -- must not override it (#1215).
    */
  def forState(
    state: AppState,
    viewportSize: ViewportSize,
    codeFont: Font,
    textFont: Font,
    cellMetrics: Option[CellMetrics] = None
  ): UiSceneSnapshot = synchronized {
    val paneFonts = state.persisted.layout.orderedPaneIds.flatMap { paneId =>
      state.persisted.layout.editorPanes
        .get(paneId)
        .flatMap(_.bufferId)
        .flatMap(state.persisted.buffers.get)
        .map { buffer =>
          val font = if buffer.usesTextFont then textFont else codeFont
          paneId -> SceneFontKey(font.getFamily, font.getStyle, font.getSize2D)
        }
    }
    val key = SceneKey(MouseTargetLayoutKey.from(state, viewportSize), paneFonts, cellMetrics)
    Option(prepared.get(key)).getOrElse {
      val layout = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
      val base   = UiSceneSnapshot.from(state, layout, viewportSize)
      // Pane rects are measured in the screen grid's cells, and that grid is the code font's, whatever font a
      // buffer draws with. Sizing a document-font pane from its own cells makes the snapshot wrap at a width the
      // pane does not have, so its rows run off the right edge instead of wrapping.
      val gridMetrics   = cellMetrics.getOrElse(CellMetrics.fromFont(codeFont))
      val surfaceConfig = state.persisted.config.surfaceConfig
      // Column-based document layout (issue #1338): mirrors `RendererPaneSetup.snapshotForBuffer`'s column branch so
      // this shared scene -- used by both painting and mouse hit-testing -- narrows the viewport to one column's width
      // and wraps at it whenever column mode and word wrap are both on. Without this the scene stayed full-width and a
      // column-mode toggle had no visible render effect (Phase 1 regression).
      val columnModeActive = surfaceConfig.columnModeEnabled && surfaceConfig.wordWrapEnabled
      val perPane = base.paneLayouts.flatMap {
        case (paneId, paneLayout) =>
          for
            pane     <- state.persisted.layout.editorPanes.get(paneId)
            bufferId <- pane.bufferId
            buffer   <- state.persisted.buffers.get(bufferId)
          yield
            val font        = if buffer.usesTextFont then textFont else codeFont
            val fontMetrics = cellMetrics.getOrElse(CellMetrics.fromFont(font))
            val width       = paneLayout.contentRect.width * gridMetrics.charWidth
            val heightPx    = paneLayout.contentRect.height * gridMetrics.lineHeight
            val baseViewport = LayoutEngine
              .updateBufferViewportDimensions(
                buffer,
                paneLayout.contentRect,
                surfaceConfig.wordWrapEnabled,
                columnModeEnabled = surfaceConfig.columnModeEnabled,
                columnTargetWidthCells = surfaceConfig.columnTargetWidthCells,
                columnGap = surfaceConfig.columnGap
              )
            val visibleColumns =
              if surfaceConfig.wordWrapEnabled then baseViewport.visibleColumns
              else
                val averageAdvance = math.max(
                  1.0f,
                  TextLayoutSnapshot
                    .caretXsForText(
                      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
                      font,
                      TextLayoutSnapshot.defaultFontRenderContext()
                    )
                    .lastOption
                    .getOrElse(0.0f) / 62.0f
                )
                math
                  .ceil(width.toDouble / averageAdvance.toDouble)
                  .toInt
                  .max(baseViewport.visibleColumns)
                  .max(paneLayout.contentRect.width + 64)
            val cursorColumn =
              buffer.editing.cursorPositions.headOption.map(_.column).getOrElse(baseViewport.leftColumn)
            val leftColumn =
              if surfaceConfig.wordWrapEnabled then 0
              else baseViewport.leftColumn.max(0).max(cursorColumn - visibleColumns + 1)
            val viewport = baseViewport.copy(
              leftColumn = leftColumn,
              visibleColumns = visibleColumns,
              visibleLines = math.max(1, heightPx / math.max(1, fontMetrics.lineHeight))
            )
            val proseScale = com.serenity.ui.theme.RichTextStyling.proseZoom(font.getSize2D)
            if columnModeActive then
              // `viewport.visibleColumns` is already the column's own width in cells (from the column-aware
              // `baseViewport` above), so its pixel width is exactly what the per-column snapshots should wrap at --
              // matching `RendererPaneSetup.snapshotForBuffer`'s column branch. `columnCount` fits "as many columns as
              // fit" the pane's full content width; each column is placed at `columnIndex * (columnWidth + gap)` cells.
              val columnWidthCells = math.max(1, viewport.visibleColumns)
              val columnWidthPx    = math.max(1, columnWidthCells * gridMetrics.charWidth)
              val columnCount =
                LayoutEngine.columnCount(
                  paneLayout.contentRect.width,
                  surfaceConfig.columnTargetWidthCells,
                  surfaceConfig.columnGap
                )
              val columnSnapshotList = TextLayoutSnapshot.fromBufferColumns(
                buffer.copy(viewport = viewport),
                columnWidthPx,
                font,
                cellMetricsOverride = Some(fontMetrics),
                forceCellLayout = cellMetrics.isDefined,
                proseScale = proseScale,
                columnCount = columnCount
              )
              // Only a genuinely multi-column page carries per-column placements. A single fitted column is fully
              // served by `textSnapshots` alone (identical to the pre-multi-column render path, including its
              // column-transition animation overlay), so emitting a lone placement would only make the renderer take
              // its multi-column branch for a page that has nothing to lay out side by side.
              val placements =
                if columnCount <= 1 then Vector.empty[ColumnSnapshotPlacement]
                else
                  columnSnapshotList.zipWithIndex.map {
                    case (snapshot, columnIndex) =>
                      ColumnSnapshotPlacement(
                        columnIndex = columnIndex,
                        xOffsetCells = columnIndex * (columnWidthCells + math.max(0, surfaceConfig.columnGap)),
                        columnWidthCells = columnWidthCells,
                        snapshot = snapshot
                      )
                  }
              // The single active-column snapshot every non-column consumer still reads is the page's first column
              // (index 0) -- the cursor's own column, since the viewport was snapped to the page holding it.
              val activeSnapshot = placements.headOption
                .map(_.snapshot)
                .getOrElse(
                  TextLayoutSnapshot.fromBufferColumn(
                    buffer.copy(viewport = viewport),
                    columnWidthPx,
                    font,
                    cellMetricsOverride = Some(fontMetrics),
                    forceCellLayout = cellMetrics.isDefined,
                    proseScale = proseScale
                  )
                )
              paneId -> (activeSnapshot, placements)
            else
              val single = TextLayoutSnapshot.fromBuffer(
                buffer.copy(viewport = viewport),
                width,
                font,
                wordWrapEnabled = surfaceConfig.wordWrapEnabled,
                cellMetricsOverride = Some(fontMetrics),
                forceCellLayout = cellMetrics.isDefined,
                // Match the render path's prose zoom so hit-testing rows/advances line up with what was drawn.
                proseScale = proseScale
              )
              paneId -> (single, Vector.empty[ColumnSnapshotPlacement])
      }
      val textSnapshots   = perPane.view.mapValues(_._1).toMap
      val columnSnapshots = perPane.view.mapValues(_._2).filter(_._2.nonEmpty).toMap
      val scene           = base.withTextSnapshots(textSnapshots).withColumnSnapshots(columnSnapshots)
      prepared.put(key, scene)
      scene
    }
  }

  /** The convenience entry point every mouse-hit-testing call site actually uses (`MouseTargetCache.fromState`,
    * `ContextualToolbarHitTesting`, `CommandRunnerMouseHitTesting`, `MouseHitTestGeometry`,
    * `PinnedPanelMouseHitTesting`, `EditorContextMenuHitTesting`). In TUI mode this must build the same cell-grid scene
    * `Renderer`'s TUI paint path builds (`CellMetrics.cellUnit`, matching `TuiRuntime`'s `CellMetricsOne`) -- otherwise
    * wrap geometry is measured from an AWT font pixel metric TUI mode never actually renders with, and a click into
    * wrapped prose lands on the wrong character (#1215-class mismatch).
    */
  def forState(state: AppState, viewportSize: ViewportSize): UiSceneSnapshot =
    forState(
      state,
      viewportSize,
      FontLoader.previewFontForRole(state.persisted.config.editorConfig.fontConfig, TypographyRole.Code),
      FontLoader.previewFontForRole(state.persisted.config.editorConfig.fontConfig, TypographyRole.Prose),
      cellMetrics = Option.when(state.runtime.isTuiMode)(CellMetrics.cellUnit)
    )

final private[manager] case class MouseTargetCache(
    layoutKey: MouseTargetLayoutKey,
    scene: UiSceneSnapshot
)

private[manager] object MouseTargetCache:

  def fromState(state: AppState, viewportSize: ViewportSize): MouseTargetCache =
    val layoutKey = MouseTargetLayoutKey.from(state, viewportSize)
    val scene     = AuthoritativeUiScene.forState(state, viewportSize)
    MouseTargetCache(layoutKey, scene)
