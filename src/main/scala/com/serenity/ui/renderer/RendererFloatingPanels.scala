package com.serenity.ui.renderer

import com.serenity.markdown.MarkdownDocumentPreview
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

    // Recorded *after* this frame's panels are painted, so `dirtyRowsFor`'s read of this same state (via `planFrame`,
    // which always runs before this method for a given frame) still sees last frame's rects while planning this one --
    // see `RendererFrameState.previousFloatingSurfaceRects`' doc comment.
    val currentFloatingRects: Map[SurfaceId, PixelRect] =
      (overlays.aboveCursorStack ++ belowOverlays).flatMap { overlay =>
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

    val imageRect          = markdownPreviewImageRect(rect, contentRect, context)
    val contentWidthCells  = math.max(1, imageRect.width)
    val contentHeightCells = math.max(1, imageRect.height)
    val widthPx = RendererMarkdownLens.scaledImagePixelDimension(
      contentWidthCells * context.cellMetrics.charWidth,
      context.surface.devicePixelScaleX
    )
    val heightPx = RendererMarkdownLens.scaledImagePixelDimension(
      contentHeightCells * context.cellMetrics.lineHeight,
      context.surface.devicePixelScaleY
    )
    val buffer = state.persisted.buffers.get(bufferId)
    val content = buffer
      .map(buffer => markdownSplitPreviewWindow(buffer, contentHeightCells).source)
      .getOrElse("")
    val baseUri =
      buffer.flatMap(_.document.filePath).flatMap(path => Option(path.toAbsolutePath.getParent).map(_.toUri))
    val image = MarkdownDocumentPreview.renderImage(
      source = content,
      title = title,
      widthPx = widthPx,
      heightPx = heightPx,
      theme = state.persisted.theme,
      font = context.textFont,
      cache = context.caches.markdownPreviewCache,
      baseUri = baseUri,
      reuseLastRenderWhileEditing =
        buffer.exists(b => b.markdownPreviewEditGeneration != b.markdownPreviewCommittedGeneration)
    )
    context.surface.pixels.drawImage(
      RenderImage.fromAwt(image),
      imageRect.x,
      imageRect.y,
      contentWidthCells,
      contentHeightCells
    )

  private def markdownPreviewImageRect(
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

  private def markdownSplitPreviewWindow(buffer: Buffer, visibleRows: Int): MarkdownDocumentPreview.PreviewWindow =
    val lineCount = buffer.document.content.lineCount
    if lineCount == 0 then MarkdownDocumentPreview.PreviewWindow(0, 0, "")
    else
      val maxSourceLines = RendererMarkdownLens.markdownPreviewSourceLineLimit(visibleRows).max(1)
      val maxStart       = (lineCount - maxSourceLines).max(0)
      val fallbackStart  = buffer.viewport.topLine.max(0).min(maxStart)
      val anchorLine = buffer.editing.cursorPositions.headOption
        .map(_.line)
        .filter(line => line >= 0 && line < lineCount)
        .getOrElse(buffer.viewport.topLine.max(0).min(lineCount - 1))
      val firstSourceLine =
        if anchorLine < fallbackStart then anchorLine.min(maxStart)
        else if anchorLine >= fallbackStart + maxSourceLines then (anchorLine - maxSourceLines / 2).max(0).min(maxStart)
        else fallbackStart
      MarkdownDocumentPreview.PreviewWindow(
        firstSourceLine,
        firstPreviewRow = 0,
        buffer.document.content.linesFrom(firstSourceLine, maxSourceLines).mkString("\n")
      )
