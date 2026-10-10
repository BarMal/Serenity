package com.serenity.state.manager

import java.awt.Font
import java.util.LinkedHashMap

import com.serenity.config.AppConfigOps.*
import com.serenity.config.{InterfaceDensity, StatusLineConfig, TextAreaInsets}
import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.MarkerMode
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
    // Count-driven columns (issue #1338, Phase 2 / slice 4): `resolvedColumnCount` reads this too, so a live change
    // to the explicit column count (with target width/gap unchanged) must also invalidate the cached scene.
    columnCount: Option[Int],
    minimumPaneWidth: Int,
    textAreaInsets: TextAreaInsets,
    proseColumnCells: Option[Int],
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
    // Hidden Markdown markers change how a line wraps and where its carets sit, and which lines are revealed follows the
    // caret, so neither the text nor the viewport alone says what a pane's snapshot looks like.
    inlineMarkdown: List[(PaneId, MarkerMode, Vector[Range.Inclusive])],
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

  final private[manager] case class Sized(state: AppState, viewportSize: ViewportSize)

  /** The key as a derived value (#1852), recomputed only when a state field [[compute]] reads is a different reference.
    * Every field it reads belongs in `references`: one left out serves a stale key, and a stale key serves a stale
    * scene (#932, #814). The viewport is compared by value in [[MouseTargetLayoutKeyCache]] instead, because callers
    * build a fresh `ViewportSize` per call.
    */
  private[manager] val derived: DerivedValue[Sized, Sized, MouseTargetLayoutKey] =
    DerivedValue(
      inputs = identity,
      references = readFields,
      compute = sized => compute(sized.state, sized.viewportSize)
    )

  private def readFields(sized: Sized): List[AnyRef] =
    val persisted = sized.state.persisted
    val runtime   = sized.state.runtime
    List(
      persisted.config,
      persisted.layout,
      persisted.focus,
      persisted.buffers,
      runtime.uiSurfaces,
      runtime.modalStack,
      // Read by `effectiveUiElementGap`, and by the status line text through `editingContext`.
      runtime.capabilities,
      // `pinnedPanels` sizes docked panels from the state's own viewport, not the one passed in.
      runtime.viewportSize,
      // `floatingStatusLineSurface` is hidden while typing.
      runtime.typingActivity
    )

  private def inlineMarkdownOf(state: AppState): List[(PaneId, MarkerMode, Vector[Range.Inclusive])] =
    state.persisted.layout.orderedPaneIds.flatMap { paneId =>
      for
        buffer <- state.persisted.layout.editorPanes
          .get(paneId)
          .flatMap(_.bufferId)
          .flatMap(state.persisted.buffers.get)
        mode = MarkerMode.of(
          state.persisted.config.inlineMarkdownViewMode,
          buffer.document.language,
          state.runtime.capabilities.isCellGrid
        )
        if mode != MarkerMode.Off
      yield (paneId, mode, if mode == MarkerMode.Live then RichTextContext.revealedLines(buffer) else Vector.empty)
    }

  private[manager] def compute(state: AppState, viewportSize: ViewportSize): MouseTargetLayoutKey =
    MouseTargetLayoutKey(
      viewportSize = viewportSize,
      fontConfig = state.persisted.config.editorConfig.fontConfig,
      showLineNumbers = state.persisted.config.surfaceConfig.showLineNumbers,
      wordWrapEnabled = state.persisted.config.surfaceConfig.wordWrapEnabled,
      columnModeEnabled = state.persisted.config.surfaceConfig.columnModeEnabled,
      columnTargetWidthCells = state.persisted.config.surfaceConfig.columnTargetWidthCells,
      columnGap = state.persisted.config.surfaceConfig.columnGap,
      columnCount = state.persisted.config.surfaceConfig.columnCount,
      minimumPaneWidth = state.persisted.config.editorConfig.minimumPaneWidth,
      textAreaInsets = state.persisted.config.surfaceConfig.textAreaInsets,
      proseColumnCells = ProseColumn.widthCells(state),
      interfaceDensity = state.persisted.config.interfaceDensity,
      uiElementGap = state.effectiveUiElementGap,
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
      inlineMarkdown = inlineMarkdownOf(state),
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

/** Instance-scoped (issue #1677) single-slot memo of the last [[MouseTargetLayoutKey]] computed, held as a
  * [[MouseTargetLayoutKey.derived]] memo for the viewport it was computed at. One instance lives on each
  * [[AuthoritativeUiScene]] -- itself one per render-owning entity -- rather than the JVM-wide singleton this used to
  * be a field of `object MouseTargetLayoutKey` itself, so two independently constructed scenes never share or contend
  * on this memo slot. `from` is called synchronously from mouse-hit-testing (`ContextualToolbarHitTesting`,
  * `CommandRunnerMouseHitTesting`, `MouseHitTestGeometry`, `PinnedPanelMouseHitTesting`, `EditorContextMenuHitTesting`,
  * via `AuthoritativeUiScene.layoutKeyFor`) and from the renderer's own scene preparation
  * (`AuthoritativeUiScene.forState` below) -- none of these run inside an IO fiber, so a `Ref[IO, ...]` here would need
  * forcing via `unsafeRunSync` right back into a synchronous `def` at every call site, hiding a plain compare-and-set
  * behind an effect type nothing here ever suspends on. This is the same reasoning already applied to
  * [[com.serenity.ui.renderer.RendererFrameState.BoundedRefCache]] (#1431/#1434) and to `ThemeManager`'s highlight/lex
  * caches (#1412/#1431/#1434): `AtomicReference` gives the same lock-free CAS semantics those modules settled on,
  * without threading `IO` through the entire mouse-targeting/rendering call graph.
  */
final private[manager] class MouseTargetLayoutKeyCache:

  private val lastComputation =
    new java.util.concurrent.atomic.AtomicReference[Option[(ViewportSize, Memo[MouseTargetLayoutKey])]](None)

  def from(state: AppState, viewportSize: ViewportSize): MouseTargetLayoutKey =
    val previous = lastComputation.get().collect { case (size, memo) if size == viewportSize => memo }
    val memo     = MouseTargetLayoutKey.derived.refreshed(previous, MouseTargetLayoutKey.Sized(state, viewportSize))
    if !previous.exists(_ eq memo) then lastComputation.set(Some(viewportSize -> memo))
    memo.value

/** The single owner of the prepared scene shared by rendering and mouse targeting.
  *
  * Instance-scoped (issue #1677): one instance is created per render-owning entity (held on [[RenderCaches]], threaded
  * explicitly to every render entry point and mouse-hit-testing call site) rather than a JVM-wide singleton object. Two
  * independently constructed instances share no cache state and never contend on the same lock, so two `StateManager`s
  * can render and hit-test concurrently in one JVM without one's prepared scenes leaking into, or being evicted by, the
  * other's.
  */
final private[serenity] class AuthoritativeUiScene(val wrappedLines: WrappedLineCache):
  import AuthoritativeUiScene.{SceneFontKey, SceneKey}

  private val layoutKeyCache = new MouseTargetLayoutKeyCache

  /** Instance-scoped (issue #1677): delegates to this scene's own [[MouseTargetLayoutKeyCache]] rather than the
    * JVM-wide memo `object MouseTargetLayoutKey` used to hold, so callers that need a layout key without a full scene
    * (mouse-hit-testing) share the exact memo [[forState]] below populates, scoped to this owner.
    */
  def layoutKeyFor(state: AppState, viewportSize: ViewportSize): MouseTargetLayoutKey =
    layoutKeyCache.from(state, viewportSize)

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
    * is not a concern in the paint/hit-testing hot path this serves. The monitor synchronized on is this instance's own
    * -- scoped to whichever owner constructed it, not shared JVM-wide -- so two owners' `forState` calls never block
    * each other.
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
    val key = SceneKey(layoutKeyFor(state, viewportSize), paneFonts, cellMetrics)
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
                surfaceConfig
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
              // `viewport.visibleColumns` is the column's own full band in cells (from the column-aware `baseViewport`
              // above). Slice 2: each column carries its own line-number rail on its left edge, so its TEXT wraps in the
              // band minus that rail (`columnTextWidthCells`) while the band itself (placement `columnWidthCells`) and
              // the inter-column offset stay full-width -- matching `RendererPaneSetup.snapshotForBuffer`'s column
              // branch. Slice 4: `resolvedColumnCount` is count-driven -- Auto fits "as many columns as fit" the pane's
              // full content width, `Some(n)` pins n; each column is placed at `columnIndex * (columnWidth + gap)`
              // cells.
              val columnWidthCells     = math.max(1, viewport.visibleColumns)
              val gutterWidthCells     = LayoutEngine.perColumnGutterWidth(state)
              val columnTextWidthCells = math.max(1, columnWidthCells - gutterWidthCells)
              val columnTextWidthPx    = math.max(1, columnTextWidthCells * gridMetrics.charWidth)
              val columnCount =
                LayoutEngine.resolvedColumnCount(paneLayout.contentRect.width, surfaceConfig)
              val columnSnapshotList = TextLayoutSnapshot.fromBufferColumns(
                buffer.copy(viewport = viewport),
                columnTextWidthPx,
                font,
                cellMetricsOverride = Some(fontMetrics),
                forceCellLayout = cellMetrics.isDefined,
                proseScale = proseScale,
                columnCount = columnCount,
                dropCapsEnabled = state.persisted.config.documentConfig.dropCapsEnabled,
                wrapCache = wrappedLines
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
                        snapshot = snapshot,
                        gutterWidthCells = gutterWidthCells
                      )
                  }
              // The single active-column snapshot every non-column consumer still reads is the page's first column
              // (index 0) -- the cursor's own column, since the viewport was snapped to the page holding it.
              val activeSnapshot = placements.headOption
                .map(_.snapshot)
                .getOrElse(
                  TextLayoutSnapshot.fromBufferColumn(
                    buffer.copy(viewport = viewport),
                    columnTextWidthPx,
                    font,
                    cellMetricsOverride = Some(fontMetrics),
                    forceCellLayout = cellMetrics.isDefined,
                    proseScale = proseScale,
                    dropCapsEnabled = state.persisted.config.documentConfig.dropCapsEnabled,
                    wrapCache = wrappedLines
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
                proseScale = proseScale,
                dropCapsEnabled = state.persisted.config.documentConfig.dropCapsEnabled,
                wrapCache = wrappedLines,
                markdownViewMode = state.persisted.config.inlineMarkdownViewMode
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
      cellMetrics = Option.when(state.runtime.capabilities.isCellGrid)(CellMetrics.cellUnit)
    )

private[serenity] object AuthoritativeUiScene:

  final private case class SceneFontKey(family: String, style: Int, size: Float)

  final private case class SceneKey(
      layout: MouseTargetLayoutKey,
      paneFonts: List[(PaneId, SceneFontKey)],
      cellMetrics: Option[CellMetrics]
  )

  def apply(wrappedLines: WrappedLineCache = WrappedLineCache.bounded()): AuthoritativeUiScene =
    new AuthoritativeUiScene(wrappedLines)

final private[manager] case class MouseTargetCache(
    layoutKey: MouseTargetLayoutKey,
    scene: UiSceneSnapshot
)

private[manager] object MouseTargetCache:

  def fromState(
    state: AppState,
    viewportSize: ViewportSize,
    authoritativeScene: AuthoritativeUiScene
  ): MouseTargetCache =
    val layoutKey = authoritativeScene.layoutKeyFor(state, viewportSize)
    val scene     = authoritativeScene.forState(state, viewportSize)
    MouseTargetCache(layoutKey, scene)
