package com.serenity.ui.renderer

import com.serenity.animation.sprite.{CompanionSpriteAssets, CompanionSpriteFrames}
import com.serenity.config.VisualFlairLevel
import com.serenity.markdown.MarkdownDocumentPreview
import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Paints every surface that floats above or is pinned within the editor workspace: cursor-anchored overlays
  * (completion popups, hovers, ...), the modal backdrop + modal surface, and pinned/expanded panels (outline, markdown
  * split-preview, the companion sprite panel). Panel-level layer caching is delegated to
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
    context.surface.text.setFont(context.uiFont)
    val overlays     = OverlayViewModel.fromState(state, scene)
    val blurRadius   = SurfaceMaterials.effectiveBlurRadius(state.persisted.config)
    val panelIsDirty = RendererFramePlanner.panelDirtyCheck(damage, blurRadius)

    def paintOverlay(overlay: TextOverlayView): Unit =
      def paint(layerContext: RenderContext): Unit =
        if blurRadius > 0f then renderFloatingBackdrop(overlay, blurRadius, state.persisted.config, layerContext)
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
          RendererFramePlanner.paintPanelLayer(context, surfaceId, overlay.rect, panelIsDirty(surfaceId))(paint)
        case None => paint(context)

    overlays.aboveCursor.foreach(paintOverlay)
    val belowOverlays =
      if overlays.belowCursorStack.nonEmpty then overlays.belowCursorStack else overlays.belowCursor.toList
    belowOverlays.foreach(paintOverlay)

    // Recorded *after* this frame's panels are painted, so `dirtyRowsFor`'s read of this same state (via `planFrame`,
    // which always runs before this method for a given frame) still sees last frame's rects while planning this one --
    // see `RendererFrameState.previousFloatingSurfaceRects`' doc comment.
    val currentFloatingRects: Map[SurfaceId, PixelRect] =
      (overlays.aboveCursor.toList ++ belowOverlays).flatMap { overlay =>
        overlay.surfaceId.map(_ -> floatingPanelPixelRect(overlay.rect, context.cellMetrics))
      }.toMap
    RendererFrameState.rememberFloatingSurfaceRects(context.surface, currentFloatingRects)

  /** `rect` (in cell/row units, as every [[LayoutRect]] floating panels are placed with is) converted to the pixel
    * rectangle it occupies on screen, using the same `cellMetrics`-based conversion `paneRowRects` uses for pane
    * content rows -- the two need to agree for `vacatedFloatingSurfaceRows`'s intersection test to mean anything.
    */
  private def floatingPanelPixelRect(rect: LayoutRect, cellMetrics: CellMetrics): PixelRect =
    val leftPx = cellMetrics.toPixelX(rect.x)
    val topPx  = cellMetrics.toPixelY(rect.y)
    PixelRect(leftPx, topPx, cellMetrics.toPixelX(rect.right) - leftPx, cellMetrics.toPixelY(rect.bottom) - topPx)

  private def renderFloatingBackdrop(
    overlay: TextOverlayView,
    blurRadius: Float,
    config: com.serenity.config.AppConfig,
    context: RenderContext
  ): Unit =
    val offsetPx = FloatingSurfaceGeometry.signedRowOffsetPixels(overlay.verticalOffsetRows, context.cellMetrics)
    context.surface.pixels.withPixelTranslation(0.0, offsetPx) {
      withOptionalRoundRectClip(
        context.surface,
        overlay.rect.x,
        overlay.rect.y,
        overlay.rect.width,
        overlay.rect.height,
        config.uiCornerRadiusPx
      ) {
        context.surface.effects.foreach(
          _.blurRegion(
            overlay.rect.x,
            overlay.rect.y,
            overlay.rect.width,
            overlay.rect.height,
            blurRadius
          )
        )
      }
    }

  /** Falls back to running `render` unclipped when the surface doesn't support rounded-rect clipping -- content still
    * draws, just without the corner mask.
    */
  private def withOptionalRoundRectClip(
    surface: RenderSurface,
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    arcPx: Int
  )(render: => Unit): Unit =
    surface.roundedRects match
      case Some(rounded) => rounded.withRoundRectClip(x, y, width, height, arcPx)(render)
      case None          => render

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
    context.surface.text.setFont(context.uiFont)
    val surfaceNodes = scene.workspace.collect {
      case node @ SceneNode(SceneNodeId.Surface(surfaceId), _, _, _, _, _) => surfaceId -> node
    }.toMap
    val blurRadius   = SurfaceMaterials.effectiveBlurRadius(state.persisted.config)
    val panelIsDirty = RendererFramePlanner.panelDirtyCheck(damage, blurRadius)
    pinnedAndExpandedSurfaces(state).foreach { surface =>
      surfaceNodes.get(surface.id).foreach { node =>
        val rect = node.frameRect
        val animationState =
          state.runtime.surfaceAnimations
            .get(surface.id)
            .map(_.animationState)
            .getOrElse(com.serenity.animation.AnimationState.empty)

        def paint(layerContext: RenderContext): Unit =
          if blurRadius > 0f then
            layerContext.surface.effects.foreach(_.blurRegion(rect.x, rect.y, rect.width, rect.height, blurRadius))
          surface.content match
            case SurfaceContent.MarkdownPreview(bufferId, title) =>
              renderMarkdownPreviewPanel(bufferId, title, rect, node.contentRect, state, layerContext, animationState)
            case SurfaceContent.CompanionSprite =>
              renderCompanionSpritePanel(rect, node.contentRect, state, layerContext, animationState)
            case _ =>
              PinnedPanelRenderer.render(
                layerContext.surface,
                PinnedPanelViewModel.resolve(surface, rect, state).copy(contentRect = Some(node.contentRect)),
                state.persisted.theme,
                state.persisted.config,
                animationState
              )

        RendererFramePlanner.paintPanelLayer(context, surface.id, rect, panelIsDirty(surface.id))(paint)
      }
    }

  /** Every pinned panel plus every surface presented as [[SurfacePresentation.Expanded]] -- the two presentation kinds
    * [[renderPinnedPanels]] paints identically (an expanded panel is a pinned panel temporarily grown to fill more of
    * the workspace; both read their geometry from the same scene node and the same [[PinnedPanelRenderer]]).
    */
  def pinnedAndExpandedSurfaces(state: AppState): List[UiSurface] =
    state.pinnedSurfaces ++ state.runtime.uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Expanded(_, _) => true
        case _                                  => false
    }

  private def renderMarkdownPreviewPanel(
    bufferId: BufferId,
    title: String,
    rect: LayoutRect,
    contentRect: LayoutRect,
    state: AppState,
    context: RenderContext,
    animationState: com.serenity.animation.AnimationState
  ): Unit =
    val shell = TextPanelView(rect = rect, contentRect = Some(contentRect), title = s"Preview: $title", rows = Nil)
    PinnedPanelRenderer.render(context.surface, shell, state.persisted.theme, state.persisted.config, animationState)

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
      baseUri = baseUri,
      reuseLastRenderWhileEditing =
        buffer.exists(b => b.markdownPreviewEditGeneration != b.markdownPreviewCommittedGeneration)
    )
    context.surface.pixels.drawImage(image, imageRect.x, imageRect.y, contentWidthCells, contentHeightCells)

  /** Paints the companion sprite pane: the same pinned-panel chrome every other panel gets, then the current sprite
    * frame drawn directly via `surface.pixels.drawImage` -- on the GUI surface a real bitmap blit, on the TUI surface
    * `TerminalRenderSurface`'s half-block conversion -- filling the panel's whole content rect. Gated on
    * `VisualFlairLevel` here as well as by `StateManagerEffectHandlers.syncCompanionSpritePanel` removing the surface
    * entirely at `Off`: a defensive second check, not a second source of truth, so this paint step alone can never draw
    * the sprite once flair is turned all the way off.
    */
  private def renderCompanionSpritePanel(
    rect: LayoutRect,
    contentRect: LayoutRect,
    state: AppState,
    context: RenderContext,
    animationState: com.serenity.animation.AnimationState
  ): Unit =
    val shell = TextPanelView(rect = rect, contentRect = Some(contentRect), title = "Companion", rows = Nil)
    PinnedPanelRenderer.render(context.surface, shell, state.persisted.theme, state.persisted.config, animationState)

    if state.persisted.config.visualFlairLevel != VisualFlairLevel.Off then
      val frames = CompanionSpriteAssets.loadFrames(state.persisted.config.companionSpriteConfig.character)
      CompanionSpriteFrames.currentFrame(frames, state.runtime.companionSprite).foreach { frame =>
        context.surface.pixels.drawImage(frame, contentRect.x, contentRect.y, contentRect.width, contentRect.height)
      }

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
      val anchorLine = buffer.editing.cursors.headOption
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
