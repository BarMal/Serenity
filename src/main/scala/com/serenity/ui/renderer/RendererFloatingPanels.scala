package com.serenity.ui.renderer

import com.serenity.config.MarkdownViewMode
import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Paints every surface that floats above or is pinned within the editor workspace: cursor-anchored overlays
  * (completion popups, hovers, ...), the modal backdrop + modal surface, and pinned/expanded panels (outline, markdown
  * split-preview). Panel-level layer caching is delegated to
  * [[RendererFramePlanner.paintPanelLayer]]/[[RendererFramePlanner.panelDirtyCheck]], which decide reuse from
  * frame-wide damage this object never sees.
  *
  * [[renderModalLayer]] and [[pinnedAndExpandedSurfaces]] are public for the reverse direction: the frame planner
  * schedules the modal layer in its own z-ordered layer stack, and reconciles cached panel buffers against the set of
  * panels actually on screen, both of which are facts only this object can state.
  */
object RendererFloatingPanels:

  def renderFloatingPanels(
    state: AppState,
    context: RenderContext,
    scene: UiSceneSnapshot,
    damage: Damage
  ): Unit =
    context.surface.text.setFont(FontSpec.fromAwt(context.uiFont))
    val overlays     = OverlayViewModel.fromState(state, scene)
    val panelIsDirty = RendererFramePlanner.panelDirtyCheck(damage)

    def paintOverlay(overlay: TextOverlayView): Unit =
      def paint(layerContext: RenderContext): Unit =
        TextOverlayRenderer.render(
          layerContext.surface,
          overlay,
          state.persisted.theme,
          state.persisted.config,
          layerContext.cursorVisible,
          layerContext.uiFont,
          layerContext.cellMetrics
        )
      overlay.surfaceId match
        case Some(surfaceId) =>
          RendererFramePlanner.paintPanelLayer(state, context, surfaceId, overlay.rect, panelIsDirty(surfaceId))(
            paint
          )
        case None => paint(context)

    overlays.tabBar.foreach(paintOverlay)
    overlays.aboveCursorStack.foreach(paintOverlay)
    val belowOverlays =
      if overlays.belowCursorStack.nonEmpty then overlays.belowCursorStack else overlays.belowCursor.toList
    belowOverlays.foreach(paintOverlay)
    overlays.cornerStack.foreach(paintOverlay)

    // Recorded *after* this frame's panels are painted, so `dirtyRowsFor`'s read of this same state (via `planFrame`,
    // which always runs before this method for a given frame) still sees last frame's rects while planning this one --
    // see `RendererFrameState.previousFloatingSurfaceRects`' doc comment.
    val currentFloatingRects: Map[SurfaceId, PixelRect] =
      (overlays.aboveCursorStack ++ belowOverlays ++ overlays.cornerStack).flatMap { overlay =>
        overlay.surfaceId.map(_ -> floatingPanelPixelRect(overlay.rect, context.cellMetrics))
      }.toMap
    context.caches.frameState.rememberFloatingSurfaceRects(context.surface, currentFloatingRects)

  /** `rect` (in cell/row units, as every [[LayoutRect]] floating panels are placed with is) converted to the pixel
    * rectangle it occupies on screen, using the same `cellMetrics`-based conversion `paneRowRects` uses for pane
    * content rows -- the two need to agree for `vacatedFloatingSurfaceRows`'s intersection test to mean anything.
    */
  private def floatingPanelPixelRect(rect: LayoutRect, cellMetrics: CellMetrics): PixelRect =
    val leftPx = cellMetrics.toPixelX(rect.x)
    val topPx  = cellMetrics.toPixelY(rect.y)
    PixelRect(leftPx, topPx, cellMetrics.toPixelX(rect.right) - leftPx, cellMetrics.toPixelY(rect.bottom) - topPx)

  private val ModalBackdropEffect = LayerEffect(0.4f)

  def renderModalLayer(state: AppState, context: RenderContext, scene: UiSceneSnapshot): Unit =
    scene.modalBackdrop.foreach { backdrop =>
      LayerCompositor.withEffect(context.surface)(ModalBackdropEffect) {
        context.surface.setBackgroundColor(state.persisted.theme.margin)
        context.surface.fillRect(
          backdrop.frameRect.x,
          backdrop.frameRect.y,
          backdrop.frameRect.width,
          backdrop.frameRect.height,
          ' '
        )
      }
    }
    OverlayViewModel.fromState(state, scene).modal.foreach { overlay =>
      TextOverlayRenderer.render(
        context.surface,
        overlay,
        state.persisted.theme,
        state.persisted.config,
        context.cursorVisible,
        context.uiFont,
        context.cellMetrics
      )
    }

  def renderPinnedPanels(
    state: AppState,
    context: RenderContext,
    scene: UiSceneSnapshot,
    damage: Damage
  ): Unit =
    context.surface.text.setFont(FontSpec.fromAwt(context.uiFont))
    val surfaceNodes = scene.workspace.collect {
      case node @ SceneNode(SceneNodeId.Surface(surfaceId), _, _, _, _, _) => surfaceId -> node
    }.toMap
    pinnedAndExpandedSurfaces(state).foreach { surface =>
      surfaceNodes.get(surface.id).foreach { node =>
        val rect = node.frameRect
        def paintContent(layerContext: RenderContext): Unit =
          surface.content match
            case SurfaceContent.MarkdownPreview(bufferId, title) =>
              renderMarkdownPreviewPanel(bufferId, title, rect, node.contentRect, state, layerContext)
            case _ =>
              PinnedPanelRenderer.render(
                layerContext.surface,
                PinnedPanelViewModel
                  .resolve(surface, rect, state, layerContext.caches.markdownPreviewCache)
                  .copy(contentRect = Some(node.contentRect)),
                state.persisted.theme,
                state.persisted.config,
                layerContext.cellMetrics
              )

        val isDirty = RendererFramePlanner.panelDirtyCheck(damage)(surface.id)
        RendererFramePlanner.paintPanelLayer(state, context, surface.id, rect, isDirty)(paintContent)
      }
    }

  /** Every docked panel, expanded or not -- `state.pinnedSurfaces` (issue #817) already includes the currently expanded
    * one (expansion is a `Layout.maximizedWorkspaceNodeId` overlay on an otherwise still-docked surface, not a separate
    * presentation), and the renderer paints both cases identically (an expanded panel is a pinned panel temporarily
    * grown to fill more of the workspace; both read their geometry from the same scene node and the same
    * [[PinnedPanelRenderer]]).
    */
  def pinnedAndExpandedSurfaces(state: AppState): List[UiSurface] =
    state.pinnedSurfaces

  /** The panel shows the buffer's text on the editor's own pipeline: the same layout, glyph runs and Markdown restyling
    * as an editor pane, in read mode so every marker is hidden, scrolled to the editor's top line, and without a caret,
    * selection or word-wrap setting of its own.
    */
  private def renderMarkdownPreviewPanel(
    bufferId: BufferId,
    title: String,
    rect: LayoutRect,
    contentRect: LayoutRect,
    state: AppState,
    context: RenderContext
  ): Unit =
    val shell = TextPanelView(rect = rect, contentRect = Some(contentRect), title = s"Preview: $title", rows = Nil)
    PinnedPanelRenderer.render(
      context.surface,
      shell,
      state.persisted.theme,
      state.persisted.config,
      context.cellMetrics
    )

    state.persisted.buffers.get(bufferId).foreach { buffer =>
      val textRect     = markdownPreviewTextRect(rect, contentRect, context)
      val previewState = readingState(state)
      val previewBuffer =
        buffer.clearSelections.copy(viewport = buffer.viewport.copy(topVisualLine = 0, leftColumn = 0))
      val previewSnapshot = RendererPaneSetup.snapshotForBuffer(previewBuffer, textRect, previewState, context)
      RendererPaneContent.renderReadOnlyText(previewBuffer, textRect, previewState, context, previewSnapshot)
    }

  private def readingState(state: AppState): AppState =
    val config = state.persisted.config
    state.copy(persisted =
      state.persisted.copy(config =
        config
          .withMarkdownViewMode(MarkdownViewMode.Read)
          .withSurfaceConfig(
            config.surfaceConfig
              .copy(wordWrapEnabled = true, columnModeEnabled = false, focusedTextBodyEnabled = false)
          )
      )
    )

  private def markdownPreviewTextRect(
    rect: LayoutRect,
    contentRect: LayoutRect,
    context: RenderContext
  ): LayoutRect =
    val x =
      if rect.x <= 0 then rect.x
      else contentRect.x
    val right =
      if rect.right >= context.surface.viewportWidth then rect.right
      else contentRect.right

    LayoutRect(
      x = x,
      y = contentRect.y,
      width = math.max(1, right - x),
      height = math.max(1, contentRect.height)
    )
