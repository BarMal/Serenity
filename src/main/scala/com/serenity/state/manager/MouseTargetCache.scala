package com.serenity.state.manager

import java.awt.Font
import java.util.LinkedHashMap

import com.serenity.config.{AppConfig, CursorInfoBarMode, CursorInfoBarPlacement, InterfaceDensity, TextAreaInsets}
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
      case SurfaceContent.CommandPaletteSubmenu(runner, groupId, previewOnly) =>
        // LayoutEngine.calculateFloatingSurfaceHeight sizes this surface from the visible item count after the
        // submenu's own search filter (see CommandPaletteSubmenu height case) -- that's the only content-derived
        // input to its geometry.
        val allItems = runner.submenuItems(groupId)
        val itemCount = runner.activeSubmenu
          .filter(_.groupId == groupId)
          .map(_.filteredItems(allItems).size)
          .getOrElse(allItems.size)
        ("command-palette-submenu", groupId, previewOnly, itemCount)
      case other =>
        other
    SurfaceGeometryKey(surface.id, surface.presentation, contentKey)

final private[manager] case class MouseTargetLayoutKey(
    viewportSize: ViewportSize,
    fontConfig: FontConfig,
    showGutter: Boolean,
    showLineNumbers: Boolean,
    wordWrapEnabled: Boolean,
    minimumPaneWidth: Int,
    textAreaInsets: TextAreaInsets,
    interfaceDensity: InterfaceDensity,
    uiElementGap: Double,
    showPaneHeaders: Boolean,
    cursorInfoBarMode: CursorInfoBarMode,
    cursorInfoBarPlacement: CursorInfoBarPlacement,
    commandRunnerVisibleRows: Option[Int],
    commandRunnerItemGapRows: Double,
    commandRunnerCursorGapRows: Option[Double],
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
    derivedCursorInfoBarSurface: Option[UiSurface],
    pinnedPanels: List[(SurfaceId, PanelPosition, Int)],
    lineNumberContent: List[(BufferId, RopeIdentity)]
)

private[manager] object MouseTargetLayoutKey:

  /** The state fields [[from]] actually reads. Everything else it derives (focusPaneId, orderedPaneIds, paneBuffers,
    * paneSnapshotInputs, pinnedPanels, lineNumberContent, derivedCursorInfoBarSurface) is a pure function of these plus
    * viewportSize, so if none of these references changed since the last call, the previously computed key is still
    * correct and the full pane/buffer/surface walk can be skipped.
    */
  final private case class FastPathInputs(
      viewportSize: ViewportSize,
      config: AppConfig,
      layout: Layout,
      focus: Focus,
      buffers: Map[BufferId, Buffer],
      uiSurfaces: List[UiSurface]
  )

  private val lastComputation =
    new java.util.concurrent.atomic.AtomicReference[Option[(FastPathInputs, MouseTargetLayoutKey)]](None)

  private def unchangedSince(previous: FastPathInputs, current: FastPathInputs): Boolean =
    (previous.viewportSize == current.viewportSize) &&
      previous.config.eq(current.config) &&
      previous.layout.eq(current.layout) &&
      (previous.focus == current.focus) &&
      previous.buffers.eq(current.buffers) &&
      previous.uiSurfaces.eq(current.uiSurfaces)

  def from(state: AppState, viewportSize: ViewportSize): MouseTargetLayoutKey =
    val inputs = FastPathInputs(viewportSize, state.config, state.layout, state.focus, state.buffers, state.uiSurfaces)
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
      fontConfig = state.config.fontConfig,
      showGutter = state.config.showGutter,
      showLineNumbers = state.config.showLineNumbers,
      wordWrapEnabled = state.config.wordWrapEnabled,
      minimumPaneWidth = state.config.minimumPaneWidth,
      textAreaInsets = state.config.textAreaInsets,
      interfaceDensity = state.config.interfaceDensity,
      uiElementGap = state.config.uiElementGap,
      showPaneHeaders = state.config.showPaneHeaders,
      cursorInfoBarMode = state.config.cursorInfoBarMode,
      cursorInfoBarPlacement = state.config.cursorInfoBarPlacement,
      commandRunnerVisibleRows = state.config.commandRunnerVisibleRows,
      commandRunnerItemGapRows = state.config.commandRunnerItemGapRows,
      commandRunnerCursorGapRows = state.config.commandRunnerCursorGapRows,
      layoutState = state.layout,
      focus = state.focus,
      focusPaneId = state.focus match
        case Focus.EditorPane(paneId) if state.layout.editorPanes.contains(paneId) => Some(paneId)
        case _                                                                     => None,
      orderedPaneIds = state.layout.orderedPaneIds,
      paneBuffers =
        state.layout.orderedPaneIds.map(paneId => paneId -> state.layout.editorPanes.get(paneId).flatMap(_.bufferId)),
      paneSnapshotInputs = state.layout.orderedPaneIds.map { paneId =>
        paneId -> state.layout.editorPanes
          .get(paneId)
          .flatMap(_.bufferId)
          .flatMap(state.buffers.get)
          .map(buffer =>
            (
              RopeIdentity(buffer.content),
              buffer.viewport,
              buffer.typographyRole,
              buffer.language,
              buffer.richTextDocument
            )
          )
      },
      uiSurfaces = state.uiSurfaces.map(SurfaceGeometryKey.from),
      derivedCursorInfoBarSurface = state.cursorInfoBarSurface,
      pinnedPanels = state.uiSurfaces.collect {
        case UiSurface(id, _, SurfacePresentation.Pinned(position, size), _) => (id, position, size)
      },
      lineNumberContent =
        if state.config.showLineNumbers then
          state.buffers.toList.sortBy(_._1.value).map((bufferId, buffer) => bufferId -> RopeIdentity(buffer.content))
        else Nil
    )

/** The single owner of the prepared scene shared by rendering and mouse targeting. */
private[serenity] object AuthoritativeUiScene:

  final private case class SceneFontKey(family: String, style: Int, size: Float)
  final private case class SceneKey(layout: MouseTargetLayoutKey, paneFonts: List[(PaneId, SceneFontKey)])

  private val prepared = new LinkedHashMap[SceneKey, UiSceneSnapshot](16, 0.75f, true):
    override def removeEldestEntry(
      eldest: java.util.Map.Entry[SceneKey, UiSceneSnapshot]
    ): Boolean =
      size() > 64

  def forState(
    state: AppState,
    viewportSize: ViewportSize,
    codeFont: Font,
    textFont: Font
  ): UiSceneSnapshot = synchronized {
    val paneFonts = state.layout.orderedPaneIds.flatMap { paneId =>
      state.layout.editorPanes
        .get(paneId)
        .flatMap(_.bufferId)
        .flatMap(state.buffers.get)
        .map { buffer =>
          val font = if buffer.usesTextFont then textFont else codeFont
          paneId -> SceneFontKey(font.getFamily, font.getStyle, font.getSize2D)
        }
    }
    val key = SceneKey(MouseTargetLayoutKey.from(state, viewportSize), paneFonts)
    Option(prepared.get(key)).getOrElse {
      val layout = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
      val base   = UiSceneSnapshot.from(state, layout, viewportSize)
      // Pane rects are measured in the screen grid's cells, and that grid is the code font's, whatever font a
      // buffer draws with. Sizing a document-font pane from its own cells makes the snapshot wrap at a width the
      // pane does not have, so its rows run off the right edge instead of wrapping.
      val gridMetrics = CellMetrics.fromFont(codeFont)
      val snapshots = base.paneLayouts.flatMap {
        case (paneId, paneLayout) =>
          for
            pane     <- state.layout.editorPanes.get(paneId)
            bufferId <- pane.bufferId
            buffer   <- state.buffers.get(bufferId)
          yield
            val font        = if buffer.usesTextFont then textFont else codeFont
            val fontMetrics = CellMetrics.fromFont(font)
            val width       = paneLayout.contentRect.width * gridMetrics.charWidth
            val heightPx    = paneLayout.contentRect.height * gridMetrics.lineHeight
            val baseViewport = LayoutEngine
              .updateBufferViewportDimensions(buffer, paneLayout.contentRect, state.config.wordWrapEnabled)
            val visibleColumns =
              if state.config.wordWrapEnabled then baseViewport.visibleColumns
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
            val cursorColumn = buffer.cursors.headOption.map(_.column).getOrElse(baseViewport.leftColumn)
            val leftColumn =
              if state.config.wordWrapEnabled then 0
              else baseViewport.leftColumn.max(0).max(cursorColumn - visibleColumns + 1)
            val viewport = baseViewport.copy(
              leftColumn = leftColumn,
              visibleColumns = visibleColumns,
              visibleLines = math.max(1, heightPx / math.max(1, fontMetrics.lineHeight))
            )
            paneId -> TextLayoutSnapshot.fromBuffer(
              buffer.copy(viewport = viewport),
              width,
              font,
              wordWrapEnabled = state.config.wordWrapEnabled
            )
      }
      val scene = base.withTextSnapshots(snapshots)
      prepared.put(key, scene)
      scene
    }
  }

  def forState(state: AppState, viewportSize: ViewportSize): UiSceneSnapshot =
    forState(
      state,
      viewportSize,
      FontLoader.previewFontForRole(state.config.fontConfig, TypographyRole.Code),
      FontLoader.previewFontForRole(state.config.fontConfig, TypographyRole.Prose)
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
