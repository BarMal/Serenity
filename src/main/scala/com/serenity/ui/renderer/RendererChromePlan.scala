package com.serenity.ui.renderer

import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Which parts of one chrome element a frame repaints: every part, or only those named. */
final case class Repaint[A] private (everything: Boolean, parts: Set[A]):
  def includes(part: A): Boolean = everything || parts.contains(part)

  def nonEmpty: Boolean = everything || parts.nonEmpty

object Repaint:
  def all[A]: Repaint[A]                 = Repaint(everything = true, Set.empty[A])
  def only[A](parts: Set[A]): Repaint[A] = Repaint(everything = false, parts)

/** The chrome a frame paints -- spacers, pane headers, line numbers and the pinned status row -- as opposed to the
  * document rows. A persisting surface still holds what the chrome it does not repaint looked like.
  */
final case class ChromeRepaint(
    spacers: Boolean,
    statusRow: Boolean,
    headers: Repaint[PaneId],
    lineNumberRows: Repaint[Int]
):
  def paintsAny: Boolean = spacers || statusRow || headers.nonEmpty || lineNumberRows.nonEmpty

object ChromeRepaint:
  val Whole: ChromeRepaint = ChromeRepaint(spacers = true, statusRow = true, Repaint.all, Repaint.all)

/** The rects chrome is painted into. Chrome kept from an earlier frame is only valid while they hold still.
  *
  * A moved pane rect also repaints every pane row: the tab strip appearing or going away shifts them with no damage a
  * `DamageProducer` transition could report, as it knows the state but not the layout.
  */
final case class ChromeGeometry(
    spacers: List[LayoutRect],
    statusRow: Option[LayoutRect],
    lineNumbers: List[LayoutRect],
    panes: Map[PaneId, EditorPaneLayout]
)

object ChromeGeometry:

  def of(renderPlan: EditorPaneRenderPlan): ChromeGeometry =
    val contract = renderPlan.layoutContract
    ChromeGeometry(
      spacers = List(contract.leftSpacerRect, contract.rightSpacerRect),
      statusRow = contract.gutterRect,
      lineNumbers = contract.lineNumberRect.toList ++ contract.rightLineNumberRect.toList,
      panes = renderPlan.paneLayouts
    )

/** Decides which chrome a frame must repaint, and which pixels of it the frame keeps.
  *
  * Chrome is a function of state that [[Damage]] already reports: the status row of [[Damage.Chrome]], a header of
  * [[Damage.PaneChrome]], a line number of its own row's damage, and everything else of a layout, theme or config
  * change, which is [[Damage.Everything]]. So a frame needs to repaint only what its damage names, and the rest stays
  * where the persisting surface already holds it.
  */
private[renderer] object RendererChromePlan:

  /** What the frame's panes and this surface's last frame say beyond the damage itself. */
  final case class Signals(
      geometryChanged: Boolean,
      rowsOverflow: Boolean,
      surfaceOverChrome: Boolean,
      lastRowMoved: Set[Int]
  )

  /** The chrome this frame repaints: [[repaintFor]] over [[Signals]] read off the frame's panes and surfaces. */
  def plan(
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    panes: Map[PaneId, PaneFrameRecord],
    persistenceKey: SurfaceContentIdentity,
    damage: Damage,
    dirtyRowsByPane: Map[PaneId, Set[Int]],
    geometryChanged: Boolean
  ): ChromeRepaint =
    repaintFor(
      renderPlan.layoutContract,
      damage,
      dirtyRowsByPane,
      Signals(
        geometryChanged || !context.surface.addressesPixels,
        rowsOverflow(context, renderPlan, panes, persistenceKey),
        surfacesMayTouch(context, renderPlan, persistenceKey, damage, chromeRegions(context, renderPlan)),
        lastRowIfRowCountChanged(context, renderPlan.layoutContract.activePaneId, panes, persistenceKey)
      )
    )

  /** Everything is repainted when the chrome's rects moved or `damage` is `Everything`, and also when it names a
    * surface that may have left pixels over chrome (one that is, or was, over it, or whose place is not known): a panel
    * that opened, moved or closed there is a change nothing here tracks.
    *
    * `rowsOverflow` says glyphs reach past a pane's content rect, into the spacers and a right-hand counter. Chrome a
    * row's ink may have reached is repainted, as it is cleared of that ink only by being repainted.
    */
  def repaintFor(
    contract: EditorLayoutContract,
    damage: Damage,
    dirtyRowsByPane: Map[PaneId, Set[Int]],
    signals: Signals
  ): ChromeRepaint =
    if signals.geometryChanged || Damage.isEverything(damage) || signals.surfaceOverChrome then ChromeRepaint.Whole
    else
      ChromeRepaint(
        spacers = signals.rowsOverflow,
        statusRow = Damage.touchesChrome(damage),
        headers = Repaint.only(Damage.paneChromeIds(damage)),
        lineNumberRows =
          if signals.rowsOverflow && contract.rightLineNumberRect.isDefined then Repaint.all[Int]
          else lineNumberRowsToRepaint(contract.activePaneId, dirtyRowsByPane, signals.lastRowMoved)
      )

  /** Whether a surface `damage` names may have left pixels in `regions`: it is, or was last frame, over one, or its
    * place is not known.
    */
  def surfacesMayTouch(
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    persistenceKey: SurfaceContentIdentity,
    damage: Damage,
    regions: List[PixelRect]
  ): Boolean =
    val contract = renderPlan.layoutContract
    val previous = context.caches.frameState.previousFloatingSurfaceRectsFor(persistenceKey)
    Damage.surfaceIds(damage).exists { surfaceId =>
      val current =
        (if surfaceId == UiSurface.TabBarSurfaceId then contract.tabBarRect.toList
         else contract.overlayRect(surfaceId).toList ++ contract.panelRect(surfaceId).toList)
          .map(pixelRectOf(context.cellMetrics))
      val places = current ++ previous.get(surfaceId).toList
      places.isEmpty || places.exists(place => regions.exists(_.intersects(place)))
    }

  /** How far, in logical pixels, a row's glyphs can reach past the left and right of its band: antialiasing of a glyph
    * with a negative side bearing. A row is kept with that margin, or the clear would take it.
    */
  val BleedPx: Int = 1

  def withBleed(context: RenderContext)(band: PixelRect): PixelRect =
    if context.surface.addressesPixels then band.copy(xPx = band.xPx - BleedPx, widthPx = band.widthPx + 2 * BleedPx)
    else band

  /** Every place chrome is painted. */
  private def chromeRegions(context: RenderContext, renderPlan: EditorPaneRenderPlan): List[PixelRect] =
    val contract = renderPlan.layoutContract
    val panes = renderPlan.paneLayouts.values.toList.flatMap(layout =>
      List(layout.headerRect, layout.topSpacerRect, layout.bottomSpacerRect)
    )
    (List(contract.leftSpacerRect, contract.rightSpacerRect) ++ contract.gutterRect.toList ++
      contract.lineNumberRect.toList ++ contract.rightLineNumberRect.toList ++ panes)
      .map(pixelRectOf(context.cellMetrics))

  /** The active pane's last row when the pane showed another number of rows in the frame this surface last drew: the
    * counter below the last row belongs to it, and what lay there then is no longer what lies there now.
    */
  def lastRowIfRowCountChanged(
    context: RenderContext,
    activePaneId: Option[PaneId],
    panes: Map[PaneId, PaneFrameRecord],
    persistenceKey: SurfaceContentIdentity
  ): Set[Int] =
    for
      paneId   <- activePaneId.toSet
      record   <- panes.get(paneId).toSet
      previous <- context.caches.frameState.previousSnapshotsFor(persistenceKey).get(paneId).toSet
      if previous.visualLines.length != record.rowRects.length
      lastRow <- record.rowRects.indices.lastOption.toSet
    yield lastRow

  /** Whether any pane has a row whose glyphs reach past its content rect, now or in the frame this surface last drew.
    */
  def rowsOverflow(
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    panes: Map[PaneId, PaneFrameRecord],
    persistenceKey: SurfaceContentIdentity
  ): Boolean =
    panes.values.exists(_.overflowingRows.nonEmpty) ||
      context.caches.frameState.previousSnapshotsFor(persistenceKey).exists { (paneId, snapshot) =>
        renderPlan.paneLayouts
          .get(paneId)
          .exists(layout => RendererPaneSetup.overflowingRows(layout.contentRect, context, snapshot).nonEmpty)
      }

  /** A line number sits beside its row and repaints with it. An active pane the frame redraws without row records (a
    * markdown lens, an empty buffer) has no rows to pair them with, so its numbers repaint whole.
    */
  private def lineNumberRowsToRepaint(
    activePaneId: Option[PaneId],
    dirtyRowsByPane: Map[PaneId, Set[Int]],
    lastRowMoved: Set[Int]
  ): Repaint[Int] =
    activePaneId.flatMap(dirtyRowsByPane.get) match
      case Some(dirtyRows) => Repaint.only(dirtyRows ++ lastRowMoved)
      case None            => Repaint.all[Int]

  /** Paints the chrome `repaint` names, in the order a whole frame paints it. */
  def paint(state: AppState, context: RenderContext, renderPlan: EditorPaneRenderPlan, repaint: ChromeRepaint): Unit =
    if repaint.spacers then RendererPaneSetup.renderSpacerColumns(state, context, renderPlan.layoutContract)
    RendererGutter.renderLineNumbers(state, context, renderPlan, repaint.lineNumberRows)
    if repaint.statusRow then RendererGutter.renderGutter(state, context, renderPlan.layoutContract)

  /** The pixel bands `repaint` leaves alone, to be handed to `clearViewportExcept` so they survive the clear. */
  def preservedRects(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    panes: Map[PaneId, PaneFrameRecord],
    repaint: ChromeRepaint
  ): List[PixelRect] =
    val contract = renderPlan.layoutContract
    val editorPaneLayouts =
      state.persisted.layout.orderedPaneIds.filter(state.persisted.layout.editorPanes.contains).flatMap { paneId =>
        renderPlan.paneLayouts.get(paneId).map(paneId -> _)
      }
    val spacers =
      if repaint.spacers then Nil
      else
        List(contract.leftSpacerRect, contract.rightSpacerRect) ++
          editorPaneLayouts.flatMap((_, layout) => List(layout.topSpacerRect, layout.bottomSpacerRect))
    val statusRow = contract.gutterRect.filter(_ => !repaint.statusRow).toList
    val headers = editorPaneLayouts.collect {
      case (paneId, layout) if !repaint.headers.includes(paneId) =>
        contract.paneHeaderRect(paneId).getOrElse(layout.headerRect)
    }
    (spacers ++ statusRow ++ headers).map(pixelRectOf(context.cellMetrics)) ++
      keptLineNumberBands(state, context, renderPlan, panes, repaint.lineNumberRows)

  /** The line-number bands of the active pane's rows that `rows` leaves alone, in both counters. */
  private def keptLineNumberBands(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    panes: Map[PaneId, PaneFrameRecord],
    rows: Repaint[Int]
  ): List[PixelRect] =
    val contract = renderPlan.layoutContract
    for
      paneId <- state.persisted.layout.activeEditorPaneId.toList
      record <- panes.get(paneId).toList
      if state.persisted.config.surfaceConfig.showLineNumbers
      rect <- contract.lineNumberRect.toList ++ contract.rightLineNumberRect.toList
      band <- RendererGutter.keptRowBands(state, context, record.snapshot, rect, rows)
    yield band

  def pixelRectOf(cellMetrics: CellMetrics)(rect: LayoutRect): PixelRect =
    val leftPx = cellMetrics.toPixelX(rect.x)
    val topPx  = cellMetrics.toPixelY(rect.y)
    PixelRect(leftPx, topPx, cellMetrics.toPixelX(rect.right) - leftPx, cellMetrics.toPixelY(rect.bottom) - topPx)
