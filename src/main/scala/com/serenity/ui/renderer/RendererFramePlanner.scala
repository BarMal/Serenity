package com.serenity.ui.renderer

import com.serenity.state.models.*
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.*

/** What a frame redraws, moves and keeps, per pane and of its chrome, and the screen rects it changed (`None`: all). */
final case class FramePlan(
    dirtyRowsByPane: Map[PaneId, Set[Int]],
    chrome: ChromeRepaint,
    blits: List[PixelBlit],
    preserved: List[PixelRect],
    repaintRegion: Option[List[PixelRect]],
    keptPanels: Set[SurfaceId] = Set.empty
)

/** Decides what a frame actually has to (re)draw and paints the resulting layer stack: the row-level reuse plan
  * ([[planFrame]]/[[dirtyRowsFor]]), the modal/panel layer caching ([[paintModalLayer]]/[[PanelLayers.paint]]), and the
  * top-level per-frame orchestration ([[renderFrame]]/[[paintFrameLayers]]) that ties them together. The seam between
  * "what changed" (fed by [[RendererFrameState]]) and "what to paint" (the feature-specific renderers in the rest of
  * this package).
  *
  * It owns the layer-buffer reuse policy for the whole package, which is why [[PanelLayers.paint]]/[[panelDirtyCheck]]
  * are public: [[RendererFloatingPanels]] paints each panel *through* them rather than deciding for itself when a
  * cached panel image is still valid, since that decision depends on frame-wide damage the panel renderer never sees.
  */
object RendererFramePlanner:

  private val ChromeLayerId         = LayerId("chrome")
  private val EditorContentLayerId  = LayerId("editor-content")
  private val PinnedPanelsLayerId   = LayerId("pinned-panels")
  private val FloatingPanelsLayerId = LayerId("floating-panels")
  private val ModalLayerId          = LayerId("modal")

  def renderFrame(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    scene: UiSceneSnapshot,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[RenderColor],
    output: Option[FrameOutput],
    damage: Damage,
    caches: com.serenity.state.manager.RenderCaches = com.serenity.state.manager.RenderCaches.create()
  ): Option[EditorPaneRenderPlan] =
    surface.hideCursor()

    val editorRenderPlan = state.startPageSurface.flatMap {
      _.content match
        case SurfaceContent.StartPage(page) => Some(page)
        case _                              => None
    } match
      case Some(page) =>
        surface.clearViewport(state.persisted.theme.background)
        forgetPreservedContent(surface, output, caches)
        FrameCounting.planned(caches.frameCounters, None, viewportSize, cellMetrics)
        RendererStartPage.renderStartPage(
          page,
          surface,
          viewportSize,
          state.persisted.theme,
          uiFont,
          cellMetrics,
          uiMetrics
        )
        val floatContext =
          RenderContext(
            surface,
            scene.calculatedLayout,
            cursorVisible,
            cursorColor,
            codeFont,
            textFont,
            uiFont,
            cellMetrics,
            uiMetrics,
            caches = caches
          )
        val panels = FramePanels.of(state, floatContext, scene)
        RendererFloatingPanels.renderFloatingPanels(state, floatContext, Damage.Everything, panels)
        // This branch never reaches `paintFrameLayers`, so paint the modal layer here or a blocking modal opened from
        // the startup page (the open-file dialog) would be invisible (#1289).
        paintModalLayer(state, floatContext, scene, isDirty = true, panels)
        None
      case None =>
        val prepared = caches.frameState
          .preparedSceneFor(surface)
          .filter(_.matches(scene, codeFont, textFont, uiFont, cellMetrics, uiMetrics, viewportSize))
          .getOrElse {
            val next = prepareScene(
              state,
              surface,
              viewportSize,
              scene,
              cursorVisible,
              cursorColor,
              codeFont,
              textFont,
              uiFont,
              cellMetrics,
              uiMetrics,
              caches
            )
            caches.frameState.rememberPreparedScene(surface, next)
            next
          }
        val finalizedScene = prepared.scene
        val context = RenderContext(
          surface,
          finalizedScene.calculatedLayout,
          cursorVisible,
          cursorColor,
          codeFont,
          textFont,
          uiFont,
          cellMetrics,
          uiMetrics,
          caches,
          Some(viewportSize)
        )
        // The prepared scene is reused while the layout is, which says nothing of the annotations drawn on it.
        val editorRenderPlan = prepared.renderPlan.copy(
          annotations = RendererPaneSetup.annotationsFor(
            state,
            context,
            prepared.renderPlan.snapshots,
            prepared.renderPlan.columnSnapshots
          )
        )
        val panels     = FramePanels.of(state, context, finalizedScene)
        val scrollbars = RendererScrollbars.visualsFor(state, context, editorRenderPlan, panels)
        val framePlan  = planFrame(state, context, editorRenderPlan, viewportSize, output, damage, panels, scrollbars)
        // Recorded *after* `planFrame` (and thus `dirtyRowsFor`) has read this surface's snapshots for this frame, so
        // this frame's own snapshots become "previous" only for the frame after it -- see
        // `previousSnapshots`' doc comment.
        caches.frameState.rememberSnapshots(surface, editorRenderPlan.recordSnapshots)
        caches.frameState.screenSnapshots.remember(output, editorRenderPlan.recordSnapshots)
        val settled = RendererScrollbars.release(context, scrollbars, framePlan)
        framePlan match
          case Some(plan) if plan.preserved.nonEmpty =>
            RendererScrollReuse.clearKeeping(surface, state.persisted.theme.background, plan)
          case _ =>
            surface.clearViewport(state.persisted.theme.background)
        val framePanels = panels.keeping(framePlan.fold(Set.empty[SurfaceId])(_.keptPanels))
        paintFrameLayers(state, context, editorRenderPlan, finalizedScene, framePlan, damage, framePanels)
        RendererScrollbars.paint(context, scrollbars, settled)
        RendererScrollbars.paintTerminal(state, context, editorRenderPlan, panels)
        commitFramePlan(framePlan, output)
        FrameCounting.planned(caches.frameCounters, framePlan, viewportSize, cellMetrics)
        Some(editorRenderPlan)

    surface.flush()
    FrameCounting.flushed(caches.frameCounters, surface)
    editorRenderPlan

  /** Paint one frame's editor content as an explicit, z-ordered stack of layers rather than a hard-coded sequence of
    * calls -- the compositing seam #1100 introduces. Every layer except the modal still paints directly into the shared
    * `context.surface` (no buffer of its own), so for those, [[LayerCompositor.orderedForComposite]] resolving to
    * today's existing paint order is the whole of what changes visually: nothing. The modal layer is the first to own a
    * real per-surface buffer and consume [[LayerCompositor.dirtyLayers]] (#1100 stage 2) -- see [[paintModalLayer]] for
    * why it, specifically, is safe to cache and skip while the others aren't yet.
    */
  private def paintFrameLayers(
    state: AppState,
    context: RenderContext,
    editorRenderPlan: EditorPaneRenderPlan,
    scene: UiSceneSnapshot,
    framePlan: Option[FramePlan],
    damage: Damage,
    panels: FramePanels
  ): Unit =
    val modalDamage = currentModalDamage(state, damage)
    val layers = List(
      Layer(ChromeLayerId, zOrder = 0, LayerEffect.identity, damage),
      Layer(EditorContentLayerId, zOrder = 1, LayerEffect.identity, damage),
      Layer(PinnedPanelsLayerId, zOrder = 2, LayerEffect.identity, damage),
      Layer(FloatingPanelsLayerId, zOrder = 3, LayerEffect.identity, damage),
      Layer(ModalLayerId, zOrder = 4, LayerEffect.identity, modalDamage)
    )
    val dirtyLayerIds = LayerCompositor.dirtyLayers(layers).map(_.id).toSet
    PanelLayers.forgetStale(state, panels, context.surface, context.caches)
    val chrome = framePlan.map(_.chrome).getOrElse(ChromeRepaint.Whole)
    val paintByLayer: Map[LayerId, () => Unit] = Map(
      ChromeLayerId -> { () => RendererChromePlan.paint(state, context, editorRenderPlan, chrome) },
      EditorContentLayerId -> { () =>
        RendererPaneContent.renderEditorPanes(
          state,
          context,
          editorRenderPlan,
          framePlan.map(_.dirtyRowsByPane).getOrElse(Map.empty),
          chrome
        )
        RendererChapterGhosts.render(
          state,
          context,
          editorRenderPlan,
          framePlan.map(_.dirtyRowsByPane).getOrElse(Map.empty)
        )
      },
      PinnedPanelsLayerId -> { () => RendererFloatingPanels.renderPinnedPanels(state, context, scene, damage, panels) },
      FloatingPanelsLayerId -> { () => RendererFloatingPanels.renderFloatingPanels(state, context, damage, panels) },
      ModalLayerId -> { () => paintModalLayer(state, context, scene, dirtyLayerIds.contains(ModalLayerId), panels) }
    )
    LayerCompositor.orderedForComposite(layers).foreach(layer => paintByLayer(layer.id)())

  /** Whether the modal layer needs repainting this frame -- `Damage.Nothing` means it doesn't, matching
    * `renderModalLayer`'s own `scene.modalBackdrop.foreach` no-op. `runtime.modalStack` (#814) can hold more than one
    * dialog (a confirmation opened on top of another), so this checks every dialog currently open rather than assuming
    * at most one -- any one of them being named by `damage` (or `damage` being `Everything`) makes the whole layer
    * dirty, since `renderModalLayer` repaints the entire stack together, not dialog-by-dialog.
    */
  private def currentModalDamage(state: AppState, damage: Damage): Damage =
    state.runtime.modalStack
      .map(dialog => Damage.narrowToSurface(damage, dialog.id))
      .find(_ != Damage.Nothing)
      .getOrElse(Damage.Nothing)

  /** Paint the modal layer, reusing its last-painted buffer instead of repainting when it's safe to: the surface
    * supports [[LayerBufferSupport]] (GUI/Java2D only -- a `TerminalRenderSurface` reports `layerBuffers = None` and
    * this falls straight through to `renderModalLayer`, painting directly into the shared surface every frame exactly
    * as before this stage, since a terminal has no sub-cell buffering concept to give the modal its own buffer with),
    * the modal is actually present this frame (`scene.modalBackdrop` is defined), the layer is clean (`isDirty` is
    * false, i.e. `DamageProducer`'s `modalOnlyContentChange` carve-out held for this transition), and the cached image
    * still matches this frame's viewport size and cursor-blink state.
    *
    * Painting `renderModalLayer` into a fresh, fully-transparent [[LayerBufferSupport.newLayerSurface]] instead of
    * `context.surface` directly, then compositing the result back at full opacity, is pixel-identical to painting
    * directly -- see [[Java2DRenderSurface.newLayerSurface]]'s doc comment for why, and for the one correctness
    * precondition (`renderModalLayer` never reads pixels back off the surface it paints onto) that keeps this safe.
    */
  private def paintModalLayer(
    state: AppState,
    context: RenderContext,
    scene: UiSceneSnapshot,
    isDirty: Boolean,
    panels: FramePanels
  ): Unit =
    PanelLayers.supportFor(state, context) match
      case None => RendererFloatingPanels.renderModalLayer(state, context, scene, panels)
      case Some(support) =>
        scene.modalBackdrop match
          case None =>
            context.caches.frameState.forgetModalLayerBuffer(context.surface)
          case Some(_) =>
            val cached = context.caches.frameState.cachedModalLayerFor(context.surface)
            val reusable = !isDirty && cached.exists { c =>
              c.viewportWidth == context.surface.viewportWidth &&
              c.viewportHeight == context.surface.viewportHeight &&
              c.cursorVisible == context.cursorVisible
            }
            if reusable then cached.foreach(c => context.surface.pixels.compositeFullSurfaceLayer(c.image))
            else
              val capture      = LayerCapture()
              val layerSurface = support.newLayerSurface(capture.onFlush, cached.map(_.image))
              RendererFloatingPanels.renderModalLayer(state, context.copy(surface = layerSurface), scene, panels)
              layerSurface.flush()
              capture.image.foreach { image =>
                val newlyCached = CachedModalLayer(
                  image,
                  context.surface.viewportWidth,
                  context.surface.viewportHeight,
                  context.cursorVisible
                )
                context.caches.frameState.rememberModalLayerBuffer(context.surface, newlyCached)
                context.surface.pixels.compositeFullSurfaceLayer(image)
              }

  /** Whether a pinned/expanded/floating panel identified by `surfaceId` must repaint this frame rather than reuse its
    * cached buffer -- the panel generalisation of [[paintModalLayer]]'s `isDirty` flag (#1100 stage 3).
    *
    * `Damage.narrowToSurface(damage, surfaceId)` is `Damage.Nothing` whenever this frame's damage doesn't name this
    * surface, even if it names something else entirely (editor content elsewhere, chrome, another surface). That is
    * safe because a panel, like the modal, never reads pixels back off the frame it paints onto: it only redraws when
    * its own `Damage.Surface` entry says so.
    */
  def panelDirtyCheck(damage: Damage)(surfaceId: SurfaceId): Boolean =
    Damage.narrowToSurface(damage, surfaceId) != Damage.Nothing

  /** Drop every reuse promise attached to this surface and force the next repaint to cover the whole canvas.
    *
    * Used by frames that repaint the canvas from scratch (the start page) or that stand `planFrame`'s optimisation down
    * entirely, where no pane row survives -- the accumulated damage this identity was tracking is now moot, since
    * everything just got redrawn from nothing this surface's own bookkeeping remembers.
    */
  def forgetPreservedContent(
    surface: RenderSurface,
    output: Option[FrameOutput],
    caches: com.serenity.state.manager.RenderCaches
  ): Unit =
    surface.persistentContentKey.foreach { key =>
      caches.frameState.forgetBufferState(key)
      // The previous frame's snapshots and panel rects are reuse promises about pixels this surface no longer holds,
      // so they go with the rest of them. `preparedScenes` stays: it is a layout memo, not a promise about pixels, and
      // its own `matches` check is what decides whether it still applies.
      caches.frameState.forgetPreviousFrameState(key)
      caches.frameState.paintedPanels.forget(key)
    }
    output.foreach { value =>
      caches.frameState.forgetScreenState(value.screenToken)
      value.repaintRegion.set(None)
    }

  private def commitFramePlan(framePlan: Option[FramePlan], output: Option[FrameOutput]): Unit =
    output.foreach(_.repaintRegion.set(framePlan.flatMap(_.repaintRegion)))

  /** Decide which pane rows this frame still has to draw, and which pixel bands it may keep from an earlier frame.
    *
    * Returns `None` — meaning "draw everything, remember nothing" — whenever the frame cannot be reasoned about safely:
    * a surface that does not preserve pixels. A floating/pinned/modal/expanded layer being visible no longer stands the
    * whole optimisation down by itself (`#1000`, retiring the old `overlaysMayCoverPanes` check) --
    * `DamageProducer.fullRenderDamage` reports `Everything` whenever `uiSurfaces`/`focus` actually change, which still
    * wipes out any stale shadow/blur/translucency bleed the instant an overlay appears, moves, resizes or changes
    * content, while leaving row reuse active on every other frame an overlay merely sits on screen. `damage` is always
    * folded into every tracked identity first, regardless of which branch this frame takes, so a pixel buffer that sits
    * idle through a stood-down frame does not lose the damage that frame reported.
    */
  private def planFrame(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    viewportSize: ViewportSize,
    output: Option[FrameOutput],
    damage: Damage,
    panels: FramePanels,
    scrollbars: Map[PaneId, ScrollbarVisual]
  ): Option[FramePlan] =
    context.caches.frameState.accumulateBufferDamage(output, damage)
    context.caches.frameState.accumulateScreenDamage(output, damage)

    val plan = context.surface.persistentContentKey
      .map { persistenceKey =>
        val panes   = RendererPaneSetup.paneRecordsFor(state, context, renderPlan)
        val paneIds = panes.keySet
        val allPanesReusable =
          renderPlan.paneLayouts.keySet.forall(panes.contains) &&
            state.persisted.layout.orderedPaneIds.forall(panes.contains)

        val bufferDamageSinceLastDraw = context.caches.frameState.drainBufferDamage(output, persistenceKey)
        val inputsOrPanesChanged =
          context.caches.frameState.drawStateChanged(
            persistenceKey,
            paneIds,
            RendererFrameState.renderInputsFor(context, viewportSize)
          )
        val geometryChanged =
          context.caches.frameState.chromeGeometryChanged(persistenceKey, ChromeGeometry.of(renderPlan))
        val effectiveDamage =
          if inputsOrPanesChanged || geometryChanged then Damage.Everything else bufferDamageSinceLastDraw

        // A ghost is derived from a note's text and from which chapters are empty, neither of which a buffer's own
        // damage reports. Rows whose ghost changed are found by comparing frames below; the screen's repaint region
        // has no such comparison, so it is withheld from panes showing ghosts.
        val ghostPaneIds =
          panes.filter((_, record) => RendererChapterGhosts.paneShowsGhosts(state, record.bufferId)).keySet
        val movable =
          RendererScrollReuse.rowsMayMove(state, context, renderPlan, persistenceKey, effectiveDamage, geometryChanged)
        val previousGhosts = context.caches.frameState.previousRowGhostsFor(persistenceKey)
        val redraws = panes.map { (paneId, record) =>
          val moving = movable && renderPlan.columnSnapshotsFor(paneId).isEmpty
          val ghostRows = DropCapLayout.closedOverGlyphs(
            DirtyLineDiff.dilate(
              RendererChapterGhosts.changedRows(previousGhosts.getOrElse(paneId, Vector.empty), record.rowGhosts),
              record.rowRects.length
            ),
            record.snapshot
          )
          val dirty =
            damageDirtyRows(effectiveDamage, record, persistenceKey, context.caches, scrollIsEveryRow = !moving) ++
              ghostRows
          paneId -> RendererScrollReuse.planPane(
            state,
            context,
            renderPlan,
            paneId,
            record,
            persistenceKey,
            dirty,
            moving
          )
        }
        context.caches.frameState.rememberRowGhosts(persistenceKey, panes.view.mapValues(_.rowGhosts).toMap)
        val dirtyRowsByPane = redraws.map((paneId, redraw) => paneId -> redraw.dirtyRows)

        val chrome = RendererChromePlan.plan(
          context,
          renderPlan,
          panes,
          persistenceKey,
          effectiveDamage,
          dirtyRowsByPane,
          geometryChanged
        )
        val preserved = panes.toList.flatMap {
          case (paneId, record) =>
            val dirty = dirtyRowsByPane.getOrElse(paneId, record.rowRects.indices.toSet)
            record.rowRects.indices
              .filterNot(dirty.contains)
              .map(record.rowRects)
              .map(RendererChromePlan.withBleed(context))
        } ++ RendererChromePlan.preservedRects(state, context, renderPlan, panes, chrome)

        val blits = redraws.values.flatMap(_.blits).toList
        val keptPanels =
          if Damage.isEverything(effectiveDamage) || blits.nonEmpty || !PanelLayers.canKeep(state, context) ||
              state.runtime.modalStack.nonEmpty
          then Set.empty[SurfaceId]
          else
            val repainted =
              panes.toList
                .flatMap { (paneId, record) =>
                  dirtyRowsByPane.getOrElse(paneId, record.rowRects.indices.toSet).toList.flatMap(record.rowRects.lift)
                }
                .map(RendererChromePlan.withBleed(context)) ++
                Option.when(chrome.paintsAny)(RendererChromePlan.regions(context, renderPlan)).toList.flatten
            val frame =
              PanelReuse.Frame(context.surface.viewportWidth, context.surface.viewportHeight, context.cursorVisible)
            PanelReuse.kept(
              panels,
              PanelLayers.holds(state, context, persistenceKey, frame),
              panelDirtyCheck(damage),
              repainted,
              context.caches.frameState.previousFloatingSurfaceRectsFor(persistenceKey)
            )

        // The screen shows the previous frame, while the pixels this surface preserves come from the one before that,
        // so the repaint region is measured against damage accumulated since the screen was last published to, and
        // only when every pane is reusable and the published pane set hasn't shifted underneath it.
        val screenDamageSincePublish = context.caches.frameState.drainScreenDamage(output)
        val screenSnapshots          = context.caches.frameState.screenSnapshots.shownOn(output)
        val screenScrollbars         = context.caches.frameState.scrollbarsChangedOnScreen(output, scrollbars)
        val screenVacated = context.caches.frameState.bandsVacatedOnScreen(
          output,
          panes.flatMap((paneId, record) => PixelRect.unionOf(record.rowRects).map(paneId -> _))
        )
        val boundedRepaintEligible =
          !context.caches.frameState.screenPaneIdsChanged(output, paneIds) && allPanesReusable && !geometryChanged &&
            ghostPaneIds.isEmpty && Damage.isBufferRowsOrChromeOnly(screenDamageSincePublish)
        val repaintRegion =
          Option.when(boundedRepaintEligible) {
            RendererRepaintRegion.rects(
              state,
              context,
              renderPlan,
              panes,
              screenDamageSincePublish,
              dirtyRowsFor(screenDamageSincePublish, _, _, screenSnapshots, persistenceKey, context.caches),
              RendererScrollbars.strips(context, renderPlan, screenScrollbars) ++ screenVacated
            )
          }

        FramePlan(
          dirtyRowsByPane = dirtyRowsByPane,
          chrome = chrome,
          blits = blits,
          preserved = preserved ++ keptPanels.toList.flatMap(panels.sites.get).map(_.painted),
          repaintRegion = repaintRegion,
          keptPanels = keptPanels
        )
      }

    if plan.isEmpty then forgetPreservedContent(context.surface, output, context.caches)
    plan

  /** Rows of `record` that `damage` marks dirty, translated from buffer line numbers to this pane's current visual row
    * indices -- every row of a damaged line, but only the rows a [[Damage.BufferCells]] span falls on
    * ([[rowsUnderSpans]]) -- widened by one row on each side and by the rows whose glyphs reach outside the band this
    * pane can preserve, plus any row whose pixel band was under a floating panel's *previous*-frame rect for every
    * `Damage.Surface(id)` fact in `damage` ([[vacatedFloatingSurfaceRows]]). `Damage.Surface` carries no buffer-row
    * detail for `Damage.coarsenToRows` to translate -- a panel's move/resize/close only reports which surface changed,
    * not which pane pixels it used to cover -- so without this, a pane whose own content didn't change preserves rows a
    * panel painted opaque background into last frame and no longer paints into this frame, leaving that background
    * stale under any theme with a transparent pane background. `Damage.Everything` dirties every row, since it carries
    * no per-buffer detail to translate.
    *
    * Unioned with [[DirtyLineDiff.dirtyRows]] comparing this pane's [[TextLayoutSnapshot]] as the screen last showed it
    * (`screenSnapshots`, from [[ScreenSnapshots.shownOn]]) against `record.snapshot`: `Damage`'s buffer-line facts only
    * ever name the paragraph actually edited, so a paragraph whose own content is untouched but whose *screen row*
    * moved -- because an earlier paragraph's edit changed how many rows it wraps into -- is otherwise never marked
    * dirty, leaving stale pixels from the row's old frame in place. `TextVisualLine` carries no cursor/selection state,
    * so this catches reflow shifts precisely without making the `Damage`-based half above redundant: a selection- or
    * cursor-only change can leave every row's `TextVisualLine` equal while still needing a redraw, which only `Damage`
    * reports.
    */
  private def dirtyRowsFor(
    damage: Damage,
    paneId: PaneId,
    record: PaneFrameRecord,
    screenSnapshots: Map[PaneId, TextLayoutSnapshot],
    persistenceKey: SurfaceContentIdentity,
    caches: com.serenity.state.manager.RenderCaches
  ): Set[Int] =
    damageDirtyRows(damage, record, persistenceKey, caches, scrollIsEveryRow = true) ++
      DirtyLineDiff.dirtyRows(screenSnapshots.get(paneId), record.snapshot)

  /** The rows `damage` itself marks dirty -- [[dirtyRowsFor]] without the comparison of snapshots. A scroll
    * ([[Damage.BufferScrolled]]) marks every row unless the caller goes on to find where the rows moved to
    * (`scrollIsEveryRow` false), as the frame being painted does and the screen's repaint region cannot.
    */
  private def damageDirtyRows(
    damage: Damage,
    record: PaneFrameRecord,
    persistenceKey: SurfaceContentIdentity,
    caches: com.serenity.state.manager.RenderCaches,
    scrollIsEveryRow: Boolean
  ): Set[Int] =
    val facts    = Damage.rowFactsFor(record.bufferId, damage)
    val everyRow = facts.everyRow && (scrollIsEveryRow || !facts.scrollOnly)
    if everyRow || Damage.isEverything(damage) then record.rowBufferLines.indices.toSet
    else
      val lineRows = record.rowBufferLines.zipWithIndex.collect {
        case (bufferLine, row) if facts.lines.contains(bufferLine) => row
      }.toSet
      val dirty = lineRows ++ rowsUnderSpans(record, facts.spans)
      DropCapLayout.closedOverGlyphs(
        DirtyLineDiff.dilate(dirty, record.rowBufferLines.length) ++
          record.overflowingRows ++
          vacatedFloatingSurfaceRows(damage, record, persistenceKey, caches),
        record.snapshot
      )

  /** The visual rows each span's columns fall on. A column at a wrap boundary matches both rows it sits between, as the
    * caret lookup does. A span that matches no row of its line (whitespace dropped at a wrap, say) dirties the whole
    * line rather than nothing.
    */
  private def rowsUnderSpans(record: PaneFrameRecord, spans: List[Damage.BufferCells]): Set[Int] =
    spans.toSet.flatMap { span =>
      val lineRows =
        record.snapshot.visualLines.zipWithIndex.filter((visualLine, _) => visualLine.bufferLine == span.row)
      val touched = lineRows.collect {
        case (visualLine, row)
            if span.fromColumn <= visualLine.endColumn && span.toColumn.forall(_ > visualLine.startColumn) =>
          row
      }
      if touched.nonEmpty then touched else lineRows.map(_._2)
    }

  /** Rows of `record` whose pixel band ([[PaneFrameRecord.rowRects]]) intersects the previous frame's rect of any
    * floating surface `damage` reports as changed ([[Damage.surfaceIds]]). A surface absent from this owner's
    * [[RendererFrameState.previousFloatingSurfaceRectsFor]] (never painted as a floating panel, or this is its first
    * frame) contributes nothing -- there is no earlier rect for it to have vacated.
    */
  private def vacatedFloatingSurfaceRows(
    damage: Damage,
    record: PaneFrameRecord,
    persistenceKey: SurfaceContentIdentity,
    caches: com.serenity.state.manager.RenderCaches
  ): Set[Int] =
    val changedSurfaceIds = Damage.surfaceIds(damage)
    if changedSurfaceIds.isEmpty then Set.empty
    else
      val previousRects = caches.frameState.previousFloatingSurfaceRectsFor(persistenceKey)
      val vacatedRects  = changedSurfaceIds.flatMap(previousRects.get)
      if vacatedRects.isEmpty then Set.empty
      else
        record.rowRects.zipWithIndex.collect {
          case (rect, row) if vacatedRects.exists(rect.intersects) => row
        }.toSet

  def prepareScene(
    state: AppState,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    scene: UiSceneSnapshot,
    cursorVisible: Boolean,
    cursorColor: Option[RenderColor],
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    caches: com.serenity.state.manager.RenderCaches = com.serenity.state.manager.RenderCaches.create()
  ): PreparedScene =
    val context = RenderContext(
      surface,
      scene.calculatedLayout,
      cursorVisible,
      cursorColor,
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      caches = caches
    )
    val renderPlan = RendererPaneSetup.prepareEditorPaneRenderPlan(state, context, scene)
    PreparedScene(
      scene,
      renderPlan,
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      viewportSize
    )
