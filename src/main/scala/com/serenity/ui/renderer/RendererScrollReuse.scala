package com.serenity.ui.renderer

import com.serenity.state.models.*
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.*

/** A copy of pixels the persisting surface already holds: `from` moved `dyPx` rows down, or up when negative. */
final case class PixelBlit(from: PixelRect, dyPx: Int)

/** Lets a frame move the pixels of rows that scrolled, or that a wrapped row above them pushed down, instead of drawing
  * them again.
  *
  * A persisting surface holds the pixels of its last frame, and [[RowMoves]] finds the rows of this frame that were
  * there too, a fixed distance away. Those rows are copied, the line numbers beside them with them, and everything else
  * is drawn as before. The copy is only ever a stand-in for drawing the same row at its new place, so it is skipped
  * whenever that could differ: damage names the row's line, an overlay may have painted over the pixels, or the surface
  * cannot copy pixels one-for-one.
  */
private[renderer] object RendererScrollReuse:

  /** What a pane redraws: the rows it paints itself, and the copies that supply the rows it does not. */
  final case class PaneRedraw(dirtyRows: Set[Int], blits: List[PixelBlit])

  object PaneRedraw:
    def everyRow(record: PaneFrameRecord): PaneRedraw = PaneRedraw(record.rowRects.indices.toSet, Nil)

  /** Moves the pixels `plan` copies, then clears everything it keeps nothing of. The copies come first: the clear would
    * otherwise wipe the rows they read.
    */
  def clearKeeping(surface: RenderSurface, background: RenderColor, plan: FramePlan): Unit =
    plan.blits.foreach(blit => surface.pixelCopying.foreach(_.copyPixels(blit.from, blit.dyPx)))
    surface.clearViewportExcept(background, plan.preserved)

  /** Whether this frame may copy rows at all. An overlay over the editor leaves its own pixels in the rows beneath it,
    * which a copy would carry to where they do not belong, and a modal dims the whole frame the same way; damage to a
    * surface means one of them is moving.
    */
  def rowsMayMove(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    persistenceKey: SurfaceContentIdentity,
    damage: Damage,
    geometryChanged: Boolean
  ): Boolean =
    context.surface.pixelCopying.isDefined && !geometryChanged && !Damage.isEverything(damage) &&
      state.runtime.modalStack.isEmpty &&
      !RendererChromePlan.surfacesMayTouch(
        context,
        renderPlan,
        persistenceKey,
        damage,
        contentAreas(context, renderPlan)
      ) &&
      overlayRects(context, renderPlan, persistenceKey).forall(overlay =>
        renderPlan.paneLayouts.values.forall(pane =>
          contentArea(context, renderPlan, pane).forall(area => !overlay.intersects(area))
        )
      )

  private def overlayRects(
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    persistenceKey: SurfaceContentIdentity
  ): List[PixelRect] =
    val contract = renderPlan.layoutContract
    val current =
      (contract.aboveCursorOverlayRects ++ contract.belowCursorOverlayRects ++ contract.floatingOverlayRects)
        .map(_._2) ++ contract.expandedSurfaceRects.values
    current.map(RendererChromePlan.pixelRectOf(context.cellMetrics)) ++
      context.caches.frameState.previousFloatingSurfaceRectsFor(persistenceKey).values

  private def contentAreas(context: RenderContext, renderPlan: EditorPaneRenderPlan): List[PixelRect] =
    renderPlan.paneLayouts.values.toList.flatMap(contentArea(context, renderPlan, _))

  /** Everything a copied row can touch: the pane's rows, and the line numbers that sit beside them. */
  private def contentArea(
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    pane: EditorPaneLayout
  ): Option[PixelRect] =
    val toPixels = RendererChromePlan.pixelRectOf(context.cellMetrics)
    val contract = renderPlan.layoutContract
    PixelRect.unionOf(
      (pane.contentRect :: (contract.lineNumberRect.toList ++ contract.rightLineNumberRect.toList)).map(toPixels)
    )

  /** The rows `record` draws itself, given the rows `damageDirty` already names, and the copies that supply the rest.
    * Without `movable` a row that changed place is simply a row that changed, as [[DirtyLineDiff.dirtyRows]] reports.
    */
  def planPane(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    paneId: PaneId,
    record: PaneFrameRecord,
    persistenceKey: SurfaceContentIdentity,
    damageDirty: Set[Int],
    movable: Boolean
  ): PaneRedraw =
    val previous = context.caches.frameState.previousSnapshotsFor(persistenceKey).get(paneId)
    val changed  = DirtyLineDiff.dirtyRows(previous, record.snapshot)
    val moved =
      for
        snapshot    <- previous.toList if movable && (changed -- damageDirty).nonEmpty
        contentRect <- renderPlan.paneLayouts.get(paneId).map(_.contentRect).toList
        bands = bandsFor(state, context, renderPlan, paneId, contentRect, snapshot, record.snapshot)
        run <- RowMoves.reusableRuns(snapshot, record.snapshot, bands, damageDirty)
      yield run -> bands
    val reused = moved.flatMap((run, _) => run.rows).toSet
    PaneRedraw(damageDirty ++ (changed -- reused), blitsFor(moved))

  /** Bands of the pane's own rows, and of the active pane's line-number columns: both must move together. */
  private def bandsFor(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    paneId: PaneId,
    contentRect: LayoutRect,
    previous: TextLayoutSnapshot,
    current: TextLayoutSnapshot
  ): List[RowBands] =
    val contract = renderPlan.layoutContract
    val numbers =
      if state.persisted.layout.activeEditorPaneId
            .contains(paneId) && state.persisted.config.surfaceConfig.showLineNumbers
      then contract.lineNumberRect.toList ++ contract.rightLineNumberRect.toList
      else Nil
    val reach = math.ceil(SurfaceTextInset.px(state.persisted.config)).toInt
    def widened(bands: Vector[PixelRect]): Vector[PixelRect] =
      bands.map(band => band.copy(xPx = band.xPx - reach, widthPx = band.widthPx + 2 * reach))
    RowBands(
      RendererPaneSetup.paneRowRects(contentRect, context, previous).map(RendererChromePlan.withBleed(context)),
      RendererPaneSetup.paneRowRects(contentRect, context, current).map(RendererChromePlan.withBleed(context))
    ) :: numbers.map { rect =>
      RowBands(
        widened(RendererGutter.counterRowBands(state, context, previous, rect)),
        widened(RendererGutter.counterRowBands(state, context, current, rect))
      )
    }

  /** Copies in the order that never overwrites a source before it is read: bottom-most first when rows move down. */
  private def blitsFor(moved: List[(RowRun, List[RowBands])]): List[PixelBlit] =
    val copies =
      for
        (run, bands) <- moved
        band         <- bands
        first = band.previous(run.fromRow)
        last  = band.previous(run.fromRow + run.rowCount - 1)
      yield (
        run.firstRow,
        PixelBlit(
          PixelRect(first.xPx, first.yPx, first.widthPx, last.bottomPx - first.yPx),
          band.current(run.firstRow).yPx - first.yPx
        )
      )
    val movesDown = copies.headOption.exists(_._2.dyPx > 0)
    copies.sortBy((firstRow, _) => if movesDown then -firstRow else firstRow).map(_._2)
