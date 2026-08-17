package com.serenity.ui.renderer

import java.awt.{Color, Font}
import java.util.concurrent.atomic.AtomicReference

import com.serenity.animation.ThemeInterpolator
import com.serenity.config.{AppConfig, CursorInfoBarPlacement, MarkdownViewMode, PostProcessingEffect}
import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.{MarkdownBlockLens, MarkdownDocumentPreview}
import com.serenity.state.manager.AuthoritativeUiScene
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.theme.*

final case class RenderContext(
    surface: RenderSurface,
    layout: CalculatedLayout,
    cursorVisible: Boolean = true,
    cursorColorOverride: Option[java.awt.Color] = None,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics
):

  def fontForRole(role: TypographyRole): java.awt.Font =
    role match
      case TypographyRole.Code            => codeFont
      case TypographyRole.Prose           => textFont
      case TypographyRole.MarkdownSource  => textFont
      case TypographyRole.MarkdownPreview => textFont
      case TypographyRole.Ui              => uiFont
      case TypographyRole.Mixed           => textFont

  def fontForBuffer(buffer: Buffer): java.awt.Font =
    fontForRole(buffer.typographyRole)

object Renderer:

  final private case class PreparedScene(
      scene: UiSceneSnapshot,
      renderPlan: EditorPaneRenderPlan,
      codeFont: Font,
      textFont: Font,
      uiFont: Font,
      cellMetrics: CellMetrics,
      uiMetrics: CellMetrics,
      viewportSize: ViewportSize
  ):

    def matches(
      candidateScene: UiSceneSnapshot,
      candidateCodeFont: Font,
      candidateTextFont: Font,
      candidateUiFont: Font,
      candidateCellMetrics: CellMetrics,
      candidateUiMetrics: CellMetrics,
      candidateViewportSize: ViewportSize
    ): Boolean =
      (scene eq candidateScene) &&
        codeFont == candidateCodeFont &&
        textFont == candidateTextFont &&
        uiFont == candidateUiFont &&
        cellMetrics == candidateCellMetrics &&
        uiMetrics == candidateUiMetrics &&
        viewportSize == candidateViewportSize

  private val preparedSceneRef = new AtomicReference[Option[PreparedScene]](None)

  private val MarkdownFenceProbeWindow    = 512
  private val MarkdownFenceProbeMaximum   = 8_192
  private val MarkdownSelectionProbeLimit = 512

  private def markdownBlockForRenderer(buffer: Buffer, line: Int): Range.Inclusive =
    val bounded = MarkdownBlockLens.currentBlock(
      buffer.content.lineCount,
      buffer.content.getLine,
      line,
      fenceProbeWindow = MarkdownFenceProbeWindow
    )
    def resolve(window: Int, range: Range.Inclusive): Range.Inclusive =
      val firstProbeLine = (line - window).max(0)
      val lastProbeLine  = (line + window).min(buffer.content.lineCount - 1)
      val probeBounded =
        (firstProbeLine > 0 && range.start == firstProbeLine) ||
          (lastProbeLine < buffer.content.lineCount - 1 && range.end == lastProbeLine)
      if !probeBounded || window >= buffer.content.lineCount || window >= MarkdownFenceProbeMaximum then range
      else
        val nextWindow = (window * 2).min(buffer.content.lineCount).min(MarkdownFenceProbeMaximum)
        val expanded = MarkdownBlockLens.currentBlock(
          buffer.content.lineCount,
          buffer.content.getLine,
          line,
          fenceProbeWindow = nextWindow
        )
        resolve(nextWindow, expanded)

    resolve(MarkdownFenceProbeWindow, bounded)

  final private case class EditorPaneRenderPlan(
      workspaceLayout: EditorWorkspaceLayout,
      layoutContract: EditorLayoutContract,
      snapshots: Map[PaneId, TextLayoutSnapshot],
      annotations: Map[BufferId, BufferRenderAnnotations]
  ):
    def paneLayouts: Map[PaneId, EditorPaneLayout] = workspaceLayout.paneLayouts

  final private case class BufferRenderAnnotations(
      commentsByLine: Map[Int, List[DocumentComment]],
      diagnosticsByLine: Map[Int, List[com.serenity.lsp.model.Diagnostic]]
  )

  final private case class MarkdownLensFrame(
      firstSourceLine: Int,
      lines: Vector[String],
      previewWindow: MarkdownDocumentPreview.PreviewWindow,
      activeSourceRanges: List[Range.Inclusive],
      previewRows: Vector[MarkdownDocumentPreview.InlinePreviewLine],
      placements: Map[Range.Inclusive, MarkdownLensPlacement]
  )

  final private case class MarkdownLensPreviewWindow(
      window: MarkdownDocumentPreview.PreviewWindow,
      sourceLineCount: Int
  )

  private val MinMarkdownPreviewSourceLines = 32
  private val MarkdownPreviewOverscanFactor = 4

  /** Reference identity for large state objects. Cheaper than structural equality and conservative in the safe
    * direction: a rebuilt but equal object simply forces a redraw.
    */
  final private class ReferenceIdentity(private val value: AnyRef):

    override def equals(other: Any): Boolean =
      other match
        case that: ReferenceIdentity => value eq that.value
        case _                       => false

    override def hashCode(): Int = System.identityHashCode(value)

  /** Everything outside a pane's own visible rows that changes how those rows are drawn. Any difference retires the
    * whole pane rather than being reasoned about per row.
    */
  final private case class PaneContentKey(
      contentRect: LayoutRect,
      theme: Theme,
      config: ReferenceIdentity,
      font: Font,
      cellMetrics: CellMetrics,
      syntaxHighlightingEnabled: Boolean,
      language: Option[LanguageId],
      selections: List[Selection]
  )

  /** Per-row inputs that are not part of the text layout: focused-body dimming, comment highlights, cell animations and
    * any caret this surface draws itself. [[com.serenity.animation.AnimatedCell]] is pure frame-counter state, so value
    * equality implies the same rendered colours.
    */
  final private case class PaneRowKey(
      focusedBody: Boolean,
      comments: List[DocumentComment],
      animations: Map[Int, com.serenity.animation.AnimatedCell],
      caretPixels: List[(Int, Int, Int)]
  )

  final private case class PaneFrameRecord(
      key: PaneContentKey,
      snapshot: TextLayoutSnapshot,
      rowKeys: Vector[PaneRowKey],
      rowRects: Vector[PixelRect],
      overflowingRows: Set[Int]
  )

  /** Everything the frame draws outside editor pane text: spacers, line numbers, gutter, pane headers and the caret
    * visibility that feeds them. Two frames sharing a chrome key paint identical pixels everywhere except pane rows,
    * which is what makes a bounded repaint safe.
    */
  final private case class ChromeKey(
      contract: EditorLayoutContract,
      theme: Theme,
      config: ReferenceIdentity,
      layout: ReferenceIdentity,
      uiSurfaces: ReferenceIdentity,
      focus: Focus,
      cursorVisible: Boolean,
      cursorColor: Option[java.awt.Color],
      viewportSize: ViewportSize,
      codeFont: Font,
      textFont: Font,
      uiFont: Font,
      cellMetrics: CellMetrics,
      uiMetrics: CellMetrics,
      gutterText: String,
      lineNumberRows: Vector[(Int, Int)],
      diagnostics: Map[BufferId, Map[Int, List[com.serenity.lsp.model.Diagnostic]]],
      headers: List[(PaneId, Boolean, Option[String], Boolean, Option[Int])]
  )

  final private case class FrameRecord(panes: Map[PaneId, PaneFrameRecord], chrome: ChromeKey)

  /** What a frame decided to reuse: the rows it still has to draw per pane, the pixel bands it kept, and the record to
    * remember once the frame has been drawn.
    */
  final private case class FramePlan(
      dirtyRowsByPane: Map[PaneId, Set[Int]],
      preserved: List[PixelRect],
      record: FrameRecord,
      repaintRegion: Option[PixelRect],
      boundedRepaint: Boolean
  )

  /** Frame records for surfaces that keep their pixels, keyed by the surface's persistence key.
    *
    * Weak keys: the base image pool recycles a small number of images, and an image the pool drops must not be held
    * alive by this cache. Access is synchronised because the render thread is not guaranteed to be a single thread
    * across the app's lifetime.
    */
  private val frameRecords = new java.util.WeakHashMap[AnyRef, FrameRecord]()

  /** Where a frame goes: the screen it will be shown on, and the region sink that screen's repaint should honour. */
  final private case class FrameOutput(screenToken: AnyRef, repaintRegion: AtomicReference[Option[PixelRect]])

  /** The record of the frame currently on screen, per screen, used to bound repaints. Distinct from [[frameRecords]]
    * because base images alternate: the pixels a surface preserves come from two frames ago, while the screen shows the
    * last one.
    */
  private val publishedRecords = new java.util.WeakHashMap[AnyRef, FrameRecord]()

  private def publishedRecordFor(output: Option[FrameOutput]): Option[FrameRecord] =
    output.flatMap(value => publishedRecords.synchronized(Option(publishedRecords.get(value.screenToken))))

  private def storePublishedRecord(output: Option[FrameOutput], record: Option[FrameRecord]): Unit =
    output.foreach { value =>
      publishedRecords.synchronized {
        record match
          case Some(frame) => val _ = publishedRecords.put(value.screenToken, frame)
          case None        => val _ = publishedRecords.remove(value.screenToken)
      }
    }

  /** Logical pixels map one-to-one onto canvas component pixels: the canvas scales the frame image back to its own
    * logical size when it paints, so a region measured against the frame is already in component coordinates.
    */
  private def toAwtRectangle(rect: PixelRect): java.awt.Rectangle =
    new java.awt.Rectangle(rect.xPx, rect.yPx, rect.widthPx, rect.heightPx)

  private def recordFor(key: AnyRef): Option[FrameRecord] =
    frameRecords.synchronized(Option(frameRecords.get(key)))

  private def storeRecord(key: AnyRef, record: Option[FrameRecord]): Unit =
    frameRecords.synchronized {
      record match
        case Some(value) => val _ = frameRecords.put(key, value)
        case None        => val _ = frameRecords.remove(key)
    }

  private def withEffectiveTheme(state: AppState): AppState =
    state.themeTransition match
      case None => state
      case Some(t) =>
        state.copy(theme = ThemeInterpolator.blend(t.previousTheme, state.theme, t.progress))

  private def startPageOnly(state: AppState): Option[StartupPage] =
    state.uiSurfaces match
      case List(UiSurface(_, SurfaceContent.StartPage(page), _, _)) => Some(page)
      case _                                                        => None

  private def renderStartPageFrame(
    state: AppState,
    page: StartupPage,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    output: Option[FrameOutput]
  ): Unit =
    surface.hideCursor()
    surface.clearViewport(state.theme.background)
    forgetPreservedContent(surface, output)
    renderStartPage(page, surface, viewportSize, state.theme, uiFont, cellMetrics, uiMetrics)
    surface.applyPostProcessing(state.config.postProcessingEffect)
    surface.flush()

  private[serenity] def withSceneIfNeeded[A](
    state: AppState,
    buildScene: => UiSceneSnapshot
  )(startup: StartupPage => A)(editor: UiSceneSnapshot => A): A =
    startPageOnly(state).fold(editor(buildScene))(startup)

  def render(
    state: AppState,
    cursorVisible: Boolean,
    swingWin: com.serenity.ui.terminal.SwingWindow,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color],
    repaintOnFlush: Boolean
  ): Unit =
    val state0 = withEffectiveTheme(state)
    // Set while the frame is drawn, read when it is flushed: None asks for a whole-canvas repaint, Some(rect) for a
    // repaint bounded to the pane rows this frame actually changed.
    val repaintRegion = new AtomicReference[Option[PixelRect]](None)
    val output        = Some(FrameOutput(swingWin.canvas, repaintRegion))
    val publishFrame: java.awt.image.BufferedImage => Unit =
      if repaintOnFlush then image => swingWin.onImageReady(image, repaintRegion.get().map(toAwtRectangle))
      else swingWin.onBaseImageReady
    val surface = Java2DRenderSurface.forFrame(
      swingWin.metrics,
      codeFont,
      swingWin.canvas,
      publishFrame,
      swingWin.acquireBaseImage
    )
    val viewportSize = swingWin.viewportSize
    val _ = withSceneIfNeeded(state0, AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont))(page =>
      renderStartPageFrame(state0, page, surface, viewportSize, uiFont, swingWin.metrics, uiMetrics, output)
    ) { scene =>
      renderFrame(
        state0,
        cursorVisible,
        surface,
        viewportSize,
        scene,
        codeFont,
        textFont,
        uiFont,
        swingWin.metrics,
        uiMetrics,
        cursorColor,
        output
      )
    }

  def renderCursorOnly(
    state: AppState,
    cursorVisible: Boolean,
    swingWin: com.serenity.ui.terminal.SwingWindow,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
  ): Boolean =
    val state0       = withEffectiveTheme(state)
    val viewportSize = swingWin.viewportSize
    withSceneIfNeeded(state0, AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont))(_ => false) {
      authoritativeScene =>
        // No base content is redrawn by this call, so the base frame contributes nothing to the repaint bound --
        // only the cursor's own old and new pixel rects can have changed on screen.
        swingWin.onCursorOverlayReady(Some(new java.awt.Rectangle(0, 0, 0, 0))) { image =>
          val surface =
            Java2DRenderSurface.forImage(image, swingWin.metrics, codeFont, swingWin.canvas, _ => ())
          val prepared = preparedSceneRef
            .get()
            .filter(
              _.matches(authoritativeScene, codeFont, textFont, uiFont, swingWin.metrics, uiMetrics, viewportSize)
            )
          val (layout, renderPlan) = prepared match
            case Some(value) => value.scene.calculatedLayout -> value.renderPlan
            case None =>
              val next = prepareScene(
                state0,
                surface,
                viewportSize,
                authoritativeScene,
                cursorVisible,
                cursorColor,
                codeFont,
                textFont,
                uiFont,
                swingWin.metrics,
                uiMetrics
              )
              preparedSceneRef.set(Some(next))
              next.scene.calculatedLayout -> next.renderPlan
          val context =
            RenderContext(
              surface,
              layout,
              cursorVisible,
              cursorColor,
              codeFont,
              textFont,
              uiFont,
              swingWin.metrics,
              uiMetrics
            )
          val cursorRects = renderEditorCursors(state0, context, renderPlan)
          surface.flush()
          cursorRects.map(toAwtRectangle)
        }
    }

  /** Render a base frame and its cursor overlay without recalculating the editor layout. */
  def renderWithCursorOverlay(
    state: AppState,
    swingWin: com.serenity.ui.terminal.SwingWindow,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
  ): Boolean =
    val state0       = withEffectiveTheme(state)
    val viewportSize = swingWin.viewportSize
    // The caret is composited from a separate overlay image, so this base frame is repainted whole every time; the
    // record is still kept up to date so the next frame knows what the screen is showing.
    val repaintRegion = new AtomicReference[Option[PixelRect]](None)
    val output        = Some(FrameOutput(swingWin.canvas, repaintRegion))
    // The pooled acquirer is what lets this frame reuse the pixels of the last frame drawn into the same image; the
    // cursor rides on a separate overlay image, so the base frame here is pure pane content and chrome.
    val surface = Java2DRenderSurface.forFrame(
      swingWin.metrics,
      codeFont,
      swingWin.canvas,
      swingWin.onBaseImageReady,
      swingWin.acquireBaseImage
    )
    withSceneIfNeeded(state0, AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont))(page =>
      renderStartPageFrame(state0, page, surface, viewportSize, uiFont, swingWin.metrics, uiMetrics, output)
      // The base frame published above went through onBaseImageReady, which does not repaint the canvas by
      // itself -- only onCursorOverlayReady does. The editor branch below reaches it naturally via its cursor
      // composite step; the startup page has no cursor to draw, but still needs this call to actually get painted.
      // A start page also has no persisted content to reason about, so this always forces a full repaint.
      swingWin.onCursorOverlayReady(None)(_ => Nil)
    ) { scene =>
      renderFrame(
        state0,
        cursorVisible = false,
        surface,
        viewportSize,
        scene,
        codeFont,
        textFont,
        uiFont,
        swingWin.metrics,
        uiMetrics,
        cursorColor = None,
        output
      ).fold(false) { renderPlan =>
        val baseDirtyRegion = repaintRegion.get().map(toAwtRectangle)
        swingWin.onCursorOverlayReady(baseDirtyRegion) { image =>
          val cursorSurface =
            Java2DRenderSurface.forImage(image, swingWin.metrics, codeFont, swingWin.canvas, _ => ())
          val cursorContext = RenderContext(
            cursorSurface,
            scene.calculatedLayout,
            true,
            cursorColor,
            codeFont,
            textFont,
            uiFont,
            swingWin.metrics,
            uiMetrics
          )
          val cursorRects = renderEditorCursors(state0, cursorContext, renderPlan)
          cursorSurface.flush()
          cursorRects.map(toAwtRectangle)
        }
      }
    }

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    cellMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
  ): Unit =
    val defaultUiFont = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
    render(
      state,
      cursorVisible,
      surface,
      viewportSize,
      codeFont,
      textFont,
      defaultUiFont,
      cellMetrics,
      CellMetrics.fromFont(defaultUiFont),
      cursorColor
    )

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
  ): Unit =
    val _ = renderWithRepaintRegion(
      state,
      cursorVisible,
      surface,
      viewportSize,
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      cursorColor
    )

  /** Render one frame and report which part of the canvas it changed.
    *
    * `None` means the whole canvas has to be repainted. `Some(rect)` means everything outside `rect` is already correct
    * on screen; an empty rect means the frame is pixel-identical to the one on screen.
    */
  private[serenity] def renderWithRepaintRegion(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
  ): Option[PixelRect] =
    val state0        = withEffectiveTheme(state)
    val repaintRegion = new AtomicReference[Option[PixelRect]](None)
    val output        = Some(FrameOutput(surface, repaintRegion))
    val _ = withSceneIfNeeded(state0, AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont))(page =>
      renderStartPageFrame(state0, page, surface, viewportSize, uiFont, cellMetrics, uiMetrics, output)
    ) { scene =>
      renderFrame(
        state0,
        cursorVisible,
        surface,
        viewportSize,
        scene,
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics,
        cursorColor,
        output
      )
    }
    repaintRegion.get()

  /** Report the pixel rects the visible cursors in `state` would be painted at, without needing a real `SwingWindow`.
    * Exposed for testing #963's bounded-repaint region: `onCursorOverlayReady` unions this same geometry (from both the
    * previous and current frame) with the base frame's own dirty region.
    */
  private[serenity] def cursorRepaintRects(
    state: AppState,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
  ): List[PixelRect] =
    val state0 = withEffectiveTheme(state)
    withSceneIfNeeded(state0, AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont))(_ => Nil) {
      scene =>
        val prepared = prepareScene(
          state0,
          surface,
          viewportSize,
          scene,
          cursorVisible = true,
          cursorColor,
          codeFont,
          textFont,
          uiFont,
          cellMetrics,
          uiMetrics
        )
        val context = RenderContext(
          surface,
          prepared.scene.calculatedLayout,
          true,
          cursorColor,
          codeFont,
          textFont,
          uiFont,
          cellMetrics,
          uiMetrics
        )
        renderEditorCursors(state0, context, prepared.renderPlan)
    }

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    cursorColor: Option[java.awt.Color] = None
  ): Unit =
    val defaultFont = Font(Font.MONOSPACED, Font.PLAIN, 12)
    render(
      state,
      cursorVisible,
      surface,
      viewportSize,
      defaultFont,
      defaultFont,
      CellMetrics.fromFont(defaultFont),
      cursorColor
    )

  private def renderFrame(
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
    cursorColor: Option[java.awt.Color],
    output: Option[FrameOutput]
  ): Option[EditorPaneRenderPlan] =
    surface.hideCursor()

    val editorRenderPlan = state.startPageSurface.flatMap {
      _.content match
        case SurfaceContent.StartPage(page) => Some(page)
        case _                              => None
    } match
      case Some(page) =>
        surface.clearViewport(state.theme.background)
        forgetPreservedContent(surface, output)
        renderStartPage(page, surface, viewportSize, state.theme, uiFont, cellMetrics, uiMetrics)
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
            uiMetrics
          )
        renderFloatingPanels(state, floatContext, scene)
        None
      case None =>
        val prepared = preparedSceneRef
          .get()
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
              uiMetrics
            )
            preparedSceneRef.set(Some(next))
            next
          }
        val finalizedScene   = prepared.scene
        val editorRenderPlan = prepared.renderPlan
        val context = RenderContext(
          surface,
          finalizedScene.calculatedLayout,
          cursorVisible,
          cursorColor,
          codeFont,
          textFont,
          uiFont,
          cellMetrics,
          uiMetrics
        )
        val framePlan = planFrame(state, context, finalizedScene, editorRenderPlan, viewportSize, output)
        framePlan match
          case Some(plan) if plan.preserved.nonEmpty =>
            surface.clearViewportExcept(state.theme.background, plan.preserved)
          case _ =>
            surface.clearViewport(state.theme.background)
        renderSpacerColumns(state, context, editorRenderPlan.layoutContract)
        renderLineNumbers(state, context, editorRenderPlan)
        renderGutter(state, context, editorRenderPlan.layoutContract)
        renderEditorPanes(
          state,
          context,
          editorRenderPlan,
          framePlan.map(_.dirtyRowsByPane).getOrElse(Map.empty)
        )
        renderPinnedPanels(state, context, finalizedScene)
        renderFloatingPanels(state, context, finalizedScene)
        renderModalLayer(state, context, finalizedScene)
        commitFramePlan(surface, framePlan, output)
        Some(editorRenderPlan)

    surface.applyPostProcessing(state.config.postProcessingEffect)
    surface.flush()
    editorRenderPlan

  /** Drop every reuse promise attached to this surface and force the next repaint to cover the whole canvas.
    *
    * Used by frames that repaint the canvas from scratch (the start page), where no pane row survives.
    */
  private def forgetPreservedContent(surface: RenderSurface, output: Option[FrameOutput]): Unit =
    surface.persistentContentKey.foreach(key => storeRecord(key, None))
    storePublishedRecord(output, None)
    output.foreach(_.repaintRegion.set(None))

  private def commitFramePlan(surface: RenderSurface, framePlan: Option[FramePlan], output: Option[FrameOutput]): Unit =
    framePlan match
      case Some(plan) =>
        surface.persistentContentKey.foreach(key => storeRecord(key, Some(plan.record)))
        storePublishedRecord(output, Some(plan.record))
        output.foreach(
          _.repaintRegion.set(
            if plan.boundedRepaint then Some(plan.repaintRegion.getOrElse(PixelRect(0, 0, 0, 0))) else None
          )
        )
      case None =>
        forgetPreservedContent(surface, output)

  /** Decide which pane rows this frame still has to draw, and which pixel bands it may keep from an earlier frame.
    *
    * Returns `None` — meaning "draw everything, remember nothing" — whenever the frame cannot be reasoned about safely:
    * a surface that does not preserve pixels, a post-processing pass that would compound over kept pixels, or any
    * floating/pinned/modal layer that may paint across pane content.
    */
  private def planFrame(
    state: AppState,
    context: RenderContext,
    scene: UiSceneSnapshot,
    renderPlan: EditorPaneRenderPlan,
    viewportSize: ViewportSize,
    output: Option[FrameOutput]
  ): Option[FramePlan] =
    context.surface.persistentContentKey
      .filter(_ => state.config.postProcessingEffect == PostProcessingEffect.Off)
      .filter(_ => !overlaysMayCoverPanes(state, scene))
      .map { persistenceKey =>
        val previous  = recordFor(persistenceKey)
        val published = publishedRecordFor(output)
        val chrome    = chromeKeyFor(state, context, renderPlan, viewportSize)
        val panes     = paneRecordsFor(state, context, renderPlan)
        val allPanesReusable =
          renderPlan.paneLayouts.keySet.forall(panes.contains) &&
            state.layout.orderedPaneIds.forall(panes.contains)

        val dirtyRowsByPane = panes.map {
          case (paneId, record) =>
            paneId -> dirtyRowsAgainst(previous.flatMap(_.panes.get(paneId)), record)
        }

        val preserved = panes.toList.flatMap {
          case (paneId, record) =>
            val dirty = dirtyRowsByPane.getOrElse(paneId, record.rowRects.indices.toSet)
            record.rowRects.indices.filterNot(dirty.contains).map(record.rowRects)
        }

        // The screen shows the previous frame, while the pixels this surface preserves come from the one before that,
        // so the repaint region is measured against the published frame and only when every other layer matches it.
        val repaintRows =
          published
            .filter(_.chrome == chrome)
            .filter(_ => allPanesReusable)
            .filter(_.panes.keySet == panes.keySet)
            .map { publishedRecord =>
              panes.toList.flatMap {
                case (paneId, record) =>
                  dirtyRowsAgainst(publishedRecord.panes.get(paneId), record).toList
                    .flatMap(record.rowRects.lift)
              }
            }

        FramePlan(
          dirtyRowsByPane = dirtyRowsByPane,
          preserved = preserved,
          record = FrameRecord(panes, chrome),
          repaintRegion = repaintRows.flatMap(PixelRect.unionOf),
          boundedRepaint = repaintRows.isDefined
        )
      }

  /** Rows of `current` that cannot reuse `previous`, widened by one row on each side and by the rows whose glyphs reach
    * outside the band this pane can preserve.
    */
  private def dirtyRowsAgainst(previous: Option[PaneFrameRecord], current: PaneFrameRecord): Set[Int] =
    val comparable = previous.filter(_.key == current.key)
    val dirty = DirtyLineDiff.dirtyRows(
      comparable.map(_.snapshot),
      current.snapshot,
      comparable.map(_.rowKeys).getOrElse(Vector.empty),
      current.rowKeys
    )
    DirtyLineDiff.dilate(dirty, current.snapshot.visualLines.length) ++ current.overflowingRows

  /** True when any floating, pinned or modal layer is on screen.
    *
    * Those layers draw shadows, blur and translucency that reach outside the rectangles the scene reports, so rather
    * than testing for overlap with each pane the whole optimisation stands down while any of them is visible.
    */
  private def overlaysMayCoverPanes(state: AppState, scene: UiSceneSnapshot): Boolean =
    val overlays = OverlayViewModel.fromState(state, scene)
    overlays.aboveCursor.nonEmpty ||
    overlays.belowCursor.nonEmpty ||
    overlays.belowCursorStack.nonEmpty ||
    overlays.modal.nonEmpty ||
    scene.modalBackdrop.nonEmpty ||
    state.pinnedSurfaces.nonEmpty ||
    state.uiSurfaces.exists {
      _.presentation match
        case SurfacePresentation.Expanded(_, _) => true
        case _                                  => false
    }

  /** Frame records for the panes drawn by [[renderPlainBufferContent]].
    *
    * Panes on any other path — the welcome text, the empty-pane filler, the inline markdown lens, which paint over the
    * whole content rect from inputs this record does not track — are left out, so they always redraw in full.
    */
  private def paneRecordsFor(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan
  ): Map[PaneId, PaneFrameRecord] =
    state.layout.editorPanes.flatMap {
      case (paneId, pane) =>
        for
          paneLayout <- renderPlan.paneLayouts.get(paneId)
          bufferId   <- pane.bufferId
          buffer     <- state.buffers.get(bufferId)
          snapshot   <- renderPlan.snapshots.get(paneId)
          if buffer.content.weight > 0 && !isInlineMarkdownLens(buffer, state)
        yield paneId -> paneFrameRecord(
          buffer,
          paneLayout.contentRect,
          state,
          context,
          snapshot,
          renderPlan.annotations.getOrElse(bufferId, BufferRenderAnnotations(Map.empty, Map.empty))
        )
    }.toMap

  private def paneFrameRecord(
    buffer: Buffer,
    contentRect: LayoutRect,
    state: AppState,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    annotations: BufferRenderAnnotations
  ): PaneFrameRecord =
    val activeBodyLines = focusedTextBodyLines(buffer, state)
    val carets          = caretPixelsByRow(buffer, contentRect, state, context, snapshot)
    val rowKeys = snapshot.visualLines.zipWithIndex.map {
      case (visualLine, row) =>
        PaneRowKey(
          focusedBody = activeBodyLines(visualLine.bufferLine),
          comments = annotations.commentsByLine.getOrElse(visualLine.bufferLine, Nil),
          animations = buffer.animations.getLineAnimations(visualLine.bufferLine),
          caretPixels = carets.getOrElse(row, Nil)
        )
    }
    PaneFrameRecord(
      key = PaneContentKey(
        contentRect = contentRect,
        theme = state.theme,
        config = ReferenceIdentity(state.config),
        font = context.fontForBuffer(buffer),
        cellMetrics = context.cellMetrics,
        syntaxHighlightingEnabled = state.syntaxHighlightingEnabled,
        language = buffer.language,
        selections = buffer.allSelections
      ),
      snapshot = snapshot,
      rowKeys = rowKeys,
      rowRects = paneRowRects(contentRect, context, snapshot),
      overflowingRows = overflowingRows(contentRect, context, snapshot)
    )

  /** Rows whose run reaches past the pane's content rect.
    *
    * Unwrapped layout shapes a couple of columns of overscan and glyphs are clipped to the whole surface rather than to
    * the pane, so such a row paints pixels outside the band that could be preserved for it. Those rows are always
    * redrawn rather than reasoned about.
    */
  private def overflowingRows(
    contentRect: LayoutRect,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Set[Int] =
    val contentWidthPx = (contentRect.right - contentRect.x) * context.cellMetrics.charWidth
    snapshot.visualLines.zipWithIndex.collect {
      case (visualLine, row) if visualLine.xOffsetPx + visualLine.widthPx > contentWidthPx.toFloat => row
    }.toSet

  /** The pixel band each visible row owns, clamped to the pane's content rect. */
  private def paneRowRects(
    contentRect: LayoutRect,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Vector[PixelRect] =
    val rowMetrics = textRowMetrics(contentRect, context, snapshot)
    val leftPx     = context.cellMetrics.toPixelX(contentRect.x)
    val widthPx    = context.cellMetrics.toPixelX(contentRect.right) - leftPx
    val heightPx =
      if snapshot.usesMeasuredLayout then snapshot.lineHeightPx else context.cellMetrics.lineHeight
    val topLimitPx    = context.cellMetrics.toPixelY(contentRect.y)
    val bottomLimitPx = rowMetrics.contentBottomPx
    snapshot.visualLines.indices.toVector.map { row =>
      val topPx    = rowMetrics.lineTopPx(row).max(topLimitPx)
      val bottomPx = (rowMetrics.lineTopPx(row) + heightPx).min(bottomLimitPx)
      PixelRect(leftPx, topPx, widthPx.max(0), (bottomPx - topPx).max(0))
    }

  /** The caret rectangles this surface paints itself, grouped by visual row.
    *
    * Mirrors [[renderCursors]] exactly, so a caret appearing, moving, blinking or changing colour retires the rows it
    * touches. When the caret is drawn on a separate overlay image instead, no rectangle is produced here and the rows
    * stay reusable.
    */
  private def caretPixelsByRow(
    buffer: Buffer,
    contentRect: LayoutRect,
    state: AppState,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Map[Int, List[(Int, Int, Int)]] =
    val cursorVisible = context.cursorVisible || state.hasCommandRunnerDomain
    buffer.cursors.zipWithIndex
      .flatMap {
        case (cursor, cursorIndex) =>
          val isPrimaryCursor = cursorIndex == 0
          val shouldRender    = cursorVisible || (buffer.cursors.size > 1 && !isPrimaryCursor)
          calculateCursorVisualPosition(cursor, snapshot)
            .filter(_ => shouldRender)
            .filter((visualLine, _) => visualLineVisible(contentRect, visualLine, context, snapshot))
            .flatMap {
              case (visualLine, xPx) =>
                val color        = cursorColorFor(state.config, state.theme, context, isPrimaryCursor)
                val caretWidthPx = math.max(2, math.round(context.cellMetrics.charWidth * 0.12f))
                val desiredXPx   = context.cellMetrics.toPixelX(contentRect.x) + math.round(xPx)
                caretWithin(contentRect, context.cellMetrics, desiredXPx, caretWidthPx).map {
                  case (caretXPx, widthPx) => visualLine -> (caretXPx, widthPx, color.getRGB)
                }
            }
      }
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toList)
      .toMap

  private def chromeKeyFor(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    viewportSize: ViewportSize
  ): ChromeKey =
    val activeSnapshot = state.layout.activeEditorPaneId.flatMap(renderPlan.snapshots.get)
    ChromeKey(
      contract = renderPlan.layoutContract,
      theme = state.theme,
      config = ReferenceIdentity(state.config),
      layout = ReferenceIdentity(state.layout),
      uiSurfaces = ReferenceIdentity(state.uiSurfaces),
      focus = state.focus,
      cursorVisible = context.cursorVisible,
      cursorColor = context.cursorColorOverride,
      viewportSize = viewportSize,
      codeFont = context.codeFont,
      textFont = context.textFont,
      uiFont = context.uiFont,
      cellMetrics = context.cellMetrics,
      uiMetrics = context.uiMetrics,
      gutterText = buildGutterContent(state),
      lineNumberRows =
        activeSnapshot.map(_.visualLines.map(line => line.bufferLine -> line.startColumn)).getOrElse(Vector.empty),
      diagnostics = renderPlan.annotations.view.mapValues(_.diagnosticsByLine).toMap,
      headers = state.layout.orderedPaneIds.map { paneId =>
        val buffer = state.layout.editorPanes.get(paneId).flatMap(_.bufferId).flatMap(state.buffers.get)
        (
          paneId,
          state.layout.activeEditorPaneId.contains(paneId),
          buffer.flatMap(_.filePath).flatMap(path => Option(path.getFileName)).map(_.toString),
          buffer.exists(_.isDirty),
          buffer.map(_.id.value)
        )
      }
    )

  private def prepareScene(
    state: AppState,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    scene: UiSceneSnapshot,
    cursorVisible: Boolean,
    cursorColor: Option[java.awt.Color],
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics
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
      uiMetrics
    )
    val renderPlan = prepareEditorPaneRenderPlan(state, context, scene)
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

  private def renderSpacerColumns(state: AppState, context: RenderContext, contract: EditorLayoutContract): Unit =
    val surface = context.surface
    surface.setBackgroundColor(state.theme.margin)
    List(contract.leftSpacerRect, contract.rightSpacerRect)
      .filter(rect => rect.width > 0 && rect.height > 0)
      .foreach(rect => surface.fillRect(rect.x, rect.y, rect.width, rect.height, ' '))

  private def prepareEditorPaneRenderPlan(
    state: AppState,
    context: RenderContext,
    scene: UiSceneSnapshot
  ): EditorPaneRenderPlan =
    val layoutContract  = scene.editorContract
    val workspaceLayout = layoutContract.workspace
    val snapshots =
      state.layout.editorPanes.flatMap {
        case (paneId, pane) =>
          for
            paneLayout <- workspaceLayout.paneLayouts.get(paneId)
            bufferId   <- pane.bufferId
            buffer     <- state.buffers.get(bufferId)
          yield paneId -> scene
            .textSnapshot(paneId)
            .getOrElse(
              snapshotForBuffer(buffer, paneLayout.contentRect, state, context)
            )
      }

    val visibleLinesByBuffer = state.layout.editorPanes.toList
      .flatMap {
        case (paneId, pane) =>
          for
            bufferId <- pane.bufferId
            snapshot <- snapshots.get(paneId)
          yield bufferId -> snapshot.visualLines.map(_.bufferLine).toSet
      }
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.foldLeft(Set.empty[Int])(_ ++ _))
      .toMap

    val annotations = state.layout.editorPanes.values
      .flatMap(_.bufferId)
      .toList
      .distinct
      .flatMap { bufferId =>
        state.buffers.get(bufferId).map { _ =>
          val visibleLines = visibleLinesByBuffer.getOrElse(bufferId, Set.empty)
          val cached =
            state.annotationIndexByBuffer.get(bufferId).map(_()).getOrElse(AnnotationLineIndex(Vector.empty, Map.empty))
          val commentsByLine    = cached.commentsByLine(visibleLines)
          val diagnosticsByLine = visibleAnnotationLines(visibleLines, cached.diagnosticsByLine)
          bufferId -> BufferRenderAnnotations(commentsByLine, diagnosticsByLine)
        }
      }
      .toMap

    EditorPaneRenderPlan(workspaceLayout, layoutContract, snapshots, annotations)

  private[serenity] def visibleAnnotationLines[A](
    visibleLines: Set[Int],
    indexed: Map[Int, List[A]]
  ): Map[Int, List[A]] =
    visibleLines.iterator.flatMap(line => indexed.get(line).map(line -> _)).toMap

  private def snapshotForBuffer(
    buffer: Buffer,
    contentRect: LayoutRect,
    state: AppState,
    context: RenderContext
  ): TextLayoutSnapshot =
    val bufferFont    = context.fontForBuffer(buffer)
    val panelWidthPx  = contentRect.width * context.cellMetrics.charWidth
    val panelHeightPx = contentRect.height * context.cellMetrics.lineHeight
    val bufferMetrics = CellMetrics.fromFont(bufferFont)
    val baseViewport  = LayoutEngine.updateBufferViewportDimensions(buffer, contentRect, state.config.wordWrapEnabled)
    val fontRenderContext =
      context.surface.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
    val visibleColumns =
      if bufferFont == context.codeFont then baseViewport.visibleColumns
      else visibleColumnsFor(bufferFont, fontRenderContext, panelWidthPx, baseViewport.visibleColumns)
    val visibleLines = math.max(1, panelHeightPx / math.max(1, bufferMetrics.lineHeight))
    val sizedViewport = baseViewport.copy(
      visibleColumns = visibleColumns,
      visibleLines = visibleLines,
      topVisualLine = baseViewport.topVisualLine.min(math.max(0, visibleLines - 1))
    )
    val scrollViewport = baseViewport.copy(
      visibleLines = visibleLines,
      topVisualLine = baseViewport.topVisualLine.min(math.max(0, visibleLines - 1))
    )
    val leftColumn =
      if visibleColumns == baseViewport.visibleColumns then baseViewport.leftColumn
      else renderedLeftColumn(buffer, scrollViewport, state.config.wordWrapEnabled)
    val renderedViewport = sizedViewport.copy(
      leftColumn = leftColumn
    )
    val renderBuffer = buffer.copy(
      viewport = renderedViewport
    )
    context.surface.setFont(bufferFont)
    TextLayoutSnapshot.fromBuffer(
      renderBuffer,
      panelWidthPx,
      bufferFont,
      fontRenderContext,
      wordWrapEnabled = state.config.wordWrapEnabled
    )

  private def visibleColumnsFor(
    font: Font,
    fontRenderContext: java.awt.font.FontRenderContext,
    panelWidthPx: Int,
    gridVisibleColumns: Int
  ): Int =
    val sample          = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    val measuredAdvance = TextLayoutSnapshot.caretXsForText(sample, font, fontRenderContext).lastOption.getOrElse(0.0f)
    val averageAdvance  = math.max(1.0f, measuredAdvance / sample.length.toFloat)
    val measuredColumns = math.ceil(panelWidthPx.toDouble / averageAdvance.toDouble).toInt + 32
    val gridOverscan    = gridVisibleColumns + 64

    math.max(gridOverscan, measuredColumns).max(1)

  private def renderedLeftColumn(buffer: Buffer, viewport: Viewport, wordWrapEnabled: Boolean): Int =
    if wordWrapEnabled then 0
    else
      val visibleColumns = math.max(1, viewport.visibleColumns)
      val cursor         = buffer.cursors.headOption.getOrElse(CursorPosition(viewport.topLine, 0))
      val cursorColumn   = cursor.column.max(0)
      val lineLength     = buffer.content.getLine(cursor.line).map(_.length).getOrElse(cursorColumn)
      val maxForCursor   = math.max(0, cursorColumn - visibleColumns + 1)
      val maxForLine     = math.max(0, lineLength - visibleColumns)

      viewport.leftColumn.max(0).min(maxForCursor).min(maxForLine)

  private def renderEditorPanes(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    dirtyRowsByPane: Map[PaneId, Set[Int]]
  ): Unit =
    val activePaneId = state.layout.activeEditorPaneId
    val orderedPanes =
      state.layout.orderedPaneIds
        .flatMap(paneId => state.layout.editorPanes.get(paneId).map(paneId -> _))
        .sortBy((paneId, _) => if activePaneId.contains(paneId) then 1 else 0)

    orderedPanes.foreach { (paneId, pane) =>
      renderPlan.paneLayouts.get(paneId) match
        case Some(paneLayout) =>
          renderEditorPane(
            pane,
            paneLayout,
            state,
            context,
            renderPlan.snapshots.get(paneId),
            renderPlan.layoutContract,
            pane.bufferId.flatMap(renderPlan.annotations.get),
            dirtyRowsByPane.get(paneId)
          )
        case None => ()
    }

  /** Paint every pane's visible cursors and report the union of pixel rects painted across all of them. */
  private def renderEditorCursors(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan
  ): List[PixelRect] =
    val activePaneId = state.layout.activeEditorPaneId
    val orderedPanes =
      activePaneId.toList.flatMap(id => state.layout.editorPanes.get(id).map(id -> _)) ++
        state.layout.editorPanes.toList.filterNot((id, _) => activePaneId.contains(id)).sortBy(_._1.value)

    orderedPanes.flatMap {
      case (paneId, pane) =>
        (for
          paneLayout <- renderPlan.paneLayouts.get(paneId)
          bufferId   <- pane.bufferId
          buffer     <- state.buffers.get(bufferId)
          snapshot   <- renderPlan.snapshots.get(paneId)
        yield renderCursors(buffer, paneLayout.contentRect, state.theme, state.config, context, snapshot))
          .getOrElse(Nil)
    }

  private def renderEditorPane(
    pane: EditorPane,
    paneLayout: EditorPaneLayout,
    state: AppState,
    context: RenderContext,
    preparedSnapshot: Option[TextLayoutSnapshot],
    contract: EditorLayoutContract,
    annotations: Option[BufferRenderAnnotations],
    dirtyRows: Option[Set[Int]]
  ): Unit =
    val buffer = pane.bufferId.flatMap(state.buffers.get)

    renderBufferHeader(pane, buffer, paneLayout, state, context, contract)
    renderEditorPaneVerticalSpacers(paneLayout, state, context)

    val contentRect = paneLayout.contentRect

    val bufferSnapshot = preparedSnapshot.orElse(buffer.map(snapshotForBuffer(_, contentRect, state, context)))
    val markdownLensFrame =
      buffer.collect {
        case buf if isInlineMarkdownLens(buf, state) => markdownLensFrameFor(buf, bufferSnapshot.get)
      }

    buffer match
      case Some(buf) if buf.content.weight == 0 && buf.isNewEmpty =>
        renderWelcomeText(contentRect, state.theme, context)
      case Some(buf) if buf.content.weight == 0 =>
        renderEmptyPane(contentRect, state.theme, context)
      case Some(buf) =>
        renderBufferContent(
          buf,
          contentRect,
          state,
          context,
          bufferSnapshot.get,
          markdownLensFrame,
          annotations.getOrElse(BufferRenderAnnotations(Map.empty, Map.empty)),
          dirtyRows
        )
      case None =>
        renderEmptyPane(contentRect, state.theme, context)

    val cursorContext =
      if state.hasCommandRunnerDomain then context.copy(cursorVisible = true)
      else context
    buffer.foreach { buf =>
      if isInlineMarkdownLens(buf, state) then
        renderMarkdownLensCursors(
          buf,
          contentRect,
          state.theme,
          state.config,
          cursorContext,
          bufferSnapshot.get,
          markdownLensFrame.getOrElse(markdownLensFrameFor(buf, bufferSnapshot.get))
        )
      else
        val _ = renderCursors(buf, contentRect, state.theme, state.config, cursorContext, bufferSnapshot.get)
    }

  private def renderBufferHeader(
    pane: EditorPane,
    buffer: Option[Buffer],
    paneLayout: EditorPaneLayout,
    state: AppState,
    context: RenderContext,
    contract: EditorLayoutContract
  ): Unit =
    val surface    = context.surface
    val headerRect = contract.paneHeaderRect(pane.id).getOrElse(paneLayout.headerRect)
    if headerRect.height > 0 then
      context.surface.setFont(context.uiFont)
      val isActive  = state.layout.activeEditorPaneId.contains(pane.id)
      val titleRect = contract.paneTitleRect(pane.id).getOrElse(paneLayout.titleRect)

      if isActive then
        surface.setBackgroundColor(state.theme.highlighted.background)
        surface.setForegroundColor(state.theme.highlighted.foreground)
      else
        surface.setBackgroundColor(state.theme.panel.background)
        surface.setForegroundColor(state.theme.panel.foreground)

      val bufferTitle = buffer match
        case Some(buf) =>
          buf.filePath match
            case Some(path) =>
              val filename = path.getFileName.toString
              if buf.isDirty then s"$filename - unsaved" else filename
            case None =>
              if buf.isDirty then s"Buffer ${buf.id.value} - unsaved" else s"Buffer ${buf.id.value}"
        case None =>
          "No Buffer"

      val maxTitleWidth = math.max(1, titleRect.width - 2)
      val displayTitle =
        if bufferTitle.length > maxTitleWidth then bufferTitle.take(maxTitleWidth - 3) + "..."
        else bufferTitle

      surface.putString(headerRect.x, headerRect.y, " " * headerRect.width)

      val titlePlacement = TextAlignment.placeLine(
        displayTitle,
        TextAreaPx(
          xPx = context.cellMetrics.toPixelX(titleRect.x).toFloat,
          yPx = context.cellMetrics.toPixelY(titleRect.y),
          widthPx = titleRect.width * context.cellMetrics.charWidth.toFloat,
          heightPx = context.cellMetrics.lineHeight
        ),
        context.uiFont,
        context.cellMetrics.lineHeight,
        context.cellMetrics.ascent,
        TextHorizontalAlignment.Center,
        TextVerticalAlignment.Top,
        surface.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
      )
      surface.drawRunPx(
        titlePlacement.xPx,
        titlePlacement.yPx,
        titlePlacement.widthPx,
        titlePlacement.lineHeightPx,
        titlePlacement.ascentPx,
        displayTitle
      )

    surface.setBackgroundColor(state.theme.background)
    surface.setForegroundColor(state.theme.foreground)

  private def renderEditorPaneVerticalSpacers(
    paneLayout: EditorPaneLayout,
    state: AppState,
    context: RenderContext
  ): Unit =
    context.surface.setBackgroundColor(state.theme.margin)
    List(paneLayout.topSpacerRect, paneLayout.bottomSpacerRect)
      .filter(rect => rect.width > 0 && rect.height > 0)
      .foreach(rect => context.surface.fillRect(rect.x, rect.y, rect.width, rect.height, ' '))
    context.surface.setBackgroundColor(state.theme.background)
    context.surface.setForegroundColor(state.theme.foreground)

  private def renderBufferContent(
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    markdownLensFrame: Option[MarkdownLensFrame],
    annotations: BufferRenderAnnotations,
    dirtyRows: Option[Set[Int]]
  ): Unit =
    context.surface.setFont(context.fontForBuffer(buffer))
    if isInlineMarkdownLens(buffer, state) then
      val frame = markdownLensFrame.getOrElse(markdownLensFrameFor(buffer, snapshot))
      renderInlineMarkdownPreview(buffer, rect, state, context, frame)
      renderMarkdownRawLenses(buffer, rect, state, context, snapshot, frame)
    else renderPlainBufferContent(buffer, rect, state, context, snapshot, annotations, dirtyRows)

  /** Draw the pane's visible rows.
    *
    * `dirtyRows` is the dirty-region contract: `None` draws every row, `Some(rows)` draws only those rows because the
    * surface still holds correct pixels for the rest (see [[planFrame]]).
    */
  private def renderPlainBufferContent(
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    annotations: BufferRenderAnnotations,
    dirtyRows: Option[Set[Int]]
  ): Unit =
    val visualLines     = snapshot.visualLines
    val xOriginPx       = context.cellMetrics.toPixelX(rect.x).toFloat
    val contentRightXPx = context.cellMetrics.toPixelX(rect.right).toFloat
    val activeBodyLines = focusedTextBodyLines(buffer, state)

    visualLines.zipWithIndex.foreach {
      case (visualLine, screenLineIndex) =>
        if dirtyRows.forall(_.contains(screenLineIndex)) &&
            visualLineFits(rect, screenLineIndex, context, snapshot)
        then
          val screenY   = rect.y + screenLineIndex
          val lineTopPx = visualLineTopPx(rect, screenLineIndex, context, snapshot)
          val screenX   = rect.x + visualLineCellOffset(visualLine, context)

          context.surface.setForegroundColor(state.theme.foreground)

          if visualLineVisible(rect, screenLineIndex, context, snapshot) &&
              screenY >= 0 &&
              screenX < context.surface.viewportWidth &&
              screenX >= 0 &&
              screenX < rect.right
          then
            val lineTheme      = state.theme
            val styledSegments = visualLineStyledSegments(visualLine, lineTheme, snapshot, activeBodyLines)
            if snapshot.usesMeasuredLayout then
              CharacterRenderer.renderMeasuredLineWithAnimation(
                context.surface,
                xOriginPx,
                lineTopPx,
                snapshot.lineHeightPx,
                snapshot.ascentPx,
                visualLine,
                lineTheme,
                buffer.animations,
                state.syntaxHighlightingEnabled,
                buffer.language,
                styledSegments,
                clipRightXPx = Some(contentRightXPx)
              )
            else
              CharacterRenderer.renderStringWithAnimation(
                context.surface,
                screenX,
                screenY,
                visualLine.text,
                lineTheme,
                buffer.animations,
                state.syntaxHighlightingEnabled,
                buffer.language,
                bufferLine = visualLine.bufferLine,
                bufferStartColumn = visualLine.startColumn,
                styledSegments = styledSegments
              )

            renderDocumentCommentHighlights(
              context.surface,
              annotations.commentsByLine.getOrElse(visualLine.bufferLine, Nil),
              visualLine,
              rect,
              screenY,
              lineTopPx,
              state.theme,
              context,
              snapshot
            )

            renderSelectionHighlights(
              context.surface,
              buffer,
              visualLine,
              rect,
              screenY,
              lineTopPx,
              state.theme,
              context,
              snapshot
            )

            val stringEnd = visualLine.startColumn + visualLine.text.length
            val lineAnims = buffer.animations.getLineAnimations(visualLine.bufferLine)
            lineAnims
              .filter((col, cell) => col >= stringEnd && cell.currentBackground.isDefined)
              .foreach { (col, cell) =>
                val bgScreenX = rect.x + visualLineCellOffset(visualLine, context) + (col - visualLine.startColumn)
                if bgScreenX >= 0 && bgScreenX < rect.right then
                  context.surface.setForegroundColor(state.theme.foreground)
                  context.surface.setBackgroundColor(cell.currentBackground.get)
                  context.surface.putString(bgScreenX, screenY, " ")
              }
    }

  private def visualLineFits(
    rect: LayoutRect,
    screenLineIndex: Int,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Boolean =
    textRowMetrics(rect, context, snapshot).lineFits(screenLineIndex)

  private def visualLineVisible(
    rect: LayoutRect,
    screenLineIndex: Int,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Boolean =
    textRowMetrics(rect, context, snapshot).lineVisible(screenLineIndex, context.surface.viewportHeight)

  private def visualLineTopPx(
    rect: LayoutRect,
    screenLineIndex: Int,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Int =
    textRowMetrics(rect, context, snapshot).lineTopPx(screenLineIndex)

  private def textRowMetrics(
    rect: LayoutRect,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): TextRowMetrics =
    TextRowMetrics(
      contentRect = rect,
      gridMetrics = context.cellMetrics,
      rowLineHeightPx = snapshot.lineHeightPx,
      usesMeasuredLayout = snapshot.usesMeasuredLayout
    )

  private def visualLineCellOffset(visualLine: TextVisualLine, context: RenderContext): Int =
    if visualLine.xOffsetPx <= 0.0f then 0
    else math.round(visualLine.xOffsetPx / context.cellMetrics.charWidth.toFloat).max(0)

  private def richTextStyledSegments(
    visualLine: TextVisualLine,
    theme: Theme,
    snapshot: TextLayoutSnapshot
  ): Option[List[StyledText]] =
    snapshot.richTextDocument
      .map { document =>
        RichTextStyling.styledLine(
          document,
          visualLine.bufferLine,
          visualLine.startColumn,
          visualLine.endColumn,
          theme
        )
      }
      .filter(segments => segments.map(_.content).mkString == visualLine.text)

  private def visualLineStyledSegments(
    visualLine: TextVisualLine,
    theme: Theme,
    snapshot: TextLayoutSnapshot,
    activeBodyLine: Int => Boolean
  ): Option[List[StyledText]] =
    val richSegments = richTextStyledSegments(visualLine, theme, snapshot)
    if activeBodyLine(visualLine.bufferLine) then richSegments
    else
      val baseSegments =
        richSegments.getOrElse(List(StyledText(visualLine.text, TextStyle.normal, theme.foreground, theme.background)))
      Some(baseSegments.map(segment => segment.copy(foregroundColor = theme.muted, backgroundColor = theme.background)))

  private def focusedTextBodyLines(buffer: Buffer, state: AppState): Int => Boolean =
    if !state.config.focusedTextBodyEnabled then _ => true
    else
      val activeLine = buffer.cursors.headOption.map(_.line)
      if buffer.language.contains(LanguageId.Markdown) then
        activeLine
          .filter(line => line >= 0 && line < buffer.content.lineCount)
          .map(line => markdownBlockForRenderer(buffer, line))
          .map((range: Range.Inclusive) => (line: Int) => range.contains(line))
          .getOrElse((_: Int) => true)
      else
        plainTextBodyRange(buffer, activeLine)
          .map(range => (line: Int) => range.contains(line))
          .getOrElse((_: Int) => true)

  private def plainTextBodyRange(buffer: Buffer, activeLine: Option[Int]): Option[Range.Inclusive] =
    activeLine
      .filter(line => line >= 0 && line < buffer.content.lineCount)
      .map { line =>
        val start = Iterator
          .iterate(line)(_ - 1)
          .takeWhile(index => index >= 0 && buffer.content.getLine(index).exists(_.trim.nonEmpty))
          .foldLeft(line)((_, index) => index)
        val end = Iterator
          .iterate(line + 1)(_ + 1)
          .takeWhile(index => index < buffer.content.lineCount && buffer.content.getLine(index).exists(_.trim.nonEmpty))
          .foldLeft(line)((_, index) => index)
        start to end
      }

  private def renderInlineMarkdownPreview(
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    frame: MarkdownLensFrame
  ): Unit =
    val widthPx =
      scaledImagePixelDimension(rect.width * context.cellMetrics.charWidth, context.surface.devicePixelScaleX)
    val heightPx =
      scaledImagePixelDimension(rect.height * context.cellMetrics.lineHeight, context.surface.devicePixelScaleY)
    val previewFont = MarkdownDocumentPreview.inlineLensFont(
      context.textFont,
      context.cellMetrics.lineHeight,
      context.surface.devicePixelScaleY
    )
    val title = buffer.filePath.flatMap(path => Option(path.getFileName).map(_.toString)).getOrElse("Untitled")
    val image = MarkdownDocumentPreview.renderInlineRowsImage(
      rows = frame.previewRows,
      sourceLines = frame.lines,
      title = title,
      widthPx = widthPx,
      heightPx = heightPx,
      theme = state.theme,
      font = previewFont,
      inlineLineHeightPx = MarkdownDocumentPreview.lineHeightForDeviceScale(
        context.cellMetrics.lineHeight,
        context.surface.devicePixelScaleY
      ),
      reuseLastRenderWhileEditing = buffer.markdownPreviewEditGeneration != buffer.markdownPreviewCommittedGeneration
    )
    context.surface.drawImage(image, rect.x, rect.y, rect.width, rect.height)

  private def scaledImagePixelDimension(logicalPx: Int, scale: Double): Int =
    math.ceil(logicalPx.max(1).toDouble * scale.max(1.0)).toInt.max(1)

  private def renderSelectionHighlights(
    surface: RenderSurface,
    buffer: Buffer,
    visualLine: TextVisualLine,
    rect: LayoutRect,
    screenY: Int,
    lineTopPx: Int,
    theme: Theme,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Unit =
    buffer.allSelections.foreach { selection =>
      columnsForRange(selection.start, selection.end, visualLine, markPoint = false).foreach {
        case (selectionStart, selectionEnd) =>
          if snapshot.usesMeasuredLayout then
            val localStart = selectionStart - visualLine.startColumn
            val localEnd   = selectionEnd - visualLine.startColumn
            if localStart >= 0 && localStart < localEnd && localEnd <= visualLine.text.length then
              val selectedText = visualLine.text.substring(localStart, localEnd)
              val lineOriginPx = context.cellMetrics.toPixelX(rect.x).toFloat
              val startXPx     = lineOriginPx + visualLine.xForColumn(selectionStart).getOrElse(visualLine.widthPx)
              val endXPx       = lineOriginPx + visualLine.xForColumn(selectionEnd).getOrElse(visualLine.widthPx)
              measuredRunWidthWithin(rect, context, startXPx, endXPx).foreach { widthPx =>
                surface.setForegroundColor(theme.highlighted.foreground)
                surface.setBackgroundColor(theme.highlighted.background)
                surface.drawRunPx(
                  startXPx,
                  lineTopPx,
                  widthPx,
                  snapshot.lineHeightPx,
                  snapshot.ascentPx,
                  selectedText,
                  clipGlyphToRun = true
                )
              }
          else
            (selectionStart until selectionEnd).foreach { bufferColumn =>
              val relativeColumn = bufferColumn - visualLine.startColumn
              val screenX        = rect.x + visualLineCellOffset(visualLine, context) + relativeColumn
              if screenX >= rect.x && screenX < rect.right then
                val charIndex = bufferColumn - visualLine.startColumn
                val charToRender =
                  if charIndex >= 0 && charIndex < visualLine.text.length then visualLine.text.charAt(charIndex)
                  else ' '
                surface.setForegroundColor(theme.highlighted.foreground)
                surface.setBackgroundColor(theme.highlighted.background)
                CharacterRenderer.renderChar(surface, screenX, screenY, charToRender)
            }
      }
    }

  private def renderDocumentCommentHighlights(
    surface: RenderSurface,
    comments: List[DocumentComment],
    visualLine: TextVisualLine,
    rect: LayoutRect,
    screenY: Int,
    lineTopPx: Int,
    theme: Theme,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Unit =
    comments.foreach { comment =>
      columnsForRange(comment.start, comment.end, visualLine, markPoint = true).foreach {
        case (commentStart, commentEnd) =>
          val foreground = theme.foreground
          renderTextRangeBackground(
            surface,
            visualLine,
            rect,
            screenY,
            lineTopPx,
            foreground,
            commentHighlightBackground(theme),
            context,
            snapshot,
            commentStart,
            commentEnd
          )
      }
    }

  private[serenity] def commentHighlightBackground(theme: Theme): Color =
    blend(theme.warning.background, theme.background, warningWeight = 0.45)

  private def blend(foreground: Color, background: Color, warningWeight: Double): Color =
    val clampedWeight    = math.max(0.0, math.min(1.0, warningWeight))
    val backgroundWeight = 1.0 - clampedWeight
    def blendChannel(channel: Color => Int): Int =
      math.round(channel(foreground) * clampedWeight + channel(background) * backgroundWeight).toInt

    Color(blendChannel(_.getRed), blendChannel(_.getGreen), blendChannel(_.getBlue))

  private def renderTextRangeBackground(
    surface: RenderSurface,
    visualLine: TextVisualLine,
    rect: LayoutRect,
    screenY: Int,
    lineTopPx: Int,
    foreground: java.awt.Color,
    background: java.awt.Color,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    rangeStart: Int,
    rangeEnd: Int
  ): Unit =
    if snapshot.usesMeasuredLayout then
      val localStart = rangeStart - visualLine.startColumn
      val localEnd   = rangeEnd - visualLine.startColumn
      if localStart >= 0 && localStart < localEnd then
        val rangeText =
          if localStart < visualLine.text.length then
            visualLine.text.substring(localStart, math.min(localEnd, visualLine.text.length))
          else " "
        val lineOriginPx = context.cellMetrics.toPixelX(rect.x).toFloat
        val startXPx     = lineOriginPx + visualLine.xForColumn(rangeStart).getOrElse(visualLine.widthPx)
        val endXPx =
          if rangeStart == rangeEnd - 1 && rangeStart >= visualLine.endColumn then
            startXPx + context.cellMetrics.charWidth
          else lineOriginPx + visualLine.xForColumn(rangeEnd).getOrElse(visualLine.widthPx)
        val desiredWidthPx = math.max(context.cellMetrics.charWidth.toFloat, endXPx - startXPx)
        measuredRunWidthWithin(rect, context, startXPx, startXPx + desiredWidthPx).foreach { widthPx =>
          surface.setForegroundColor(foreground)
          surface.setBackgroundColor(background)
          surface.drawRunPx(
            startXPx,
            lineTopPx,
            widthPx,
            snapshot.lineHeightPx,
            snapshot.ascentPx,
            rangeText,
            clipGlyphToRun = true
          )
        }
    else
      (rangeStart until rangeEnd).foreach { bufferColumn =>
        val relativeColumn = bufferColumn - visualLine.startColumn
        val screenX        = rect.x + visualLineCellOffset(visualLine, context) + relativeColumn
        if screenX >= rect.x && screenX < rect.right then
          val charIndex = bufferColumn - visualLine.startColumn
          val charToRender =
            if charIndex >= 0 && charIndex < visualLine.text.length then visualLine.text.charAt(charIndex)
            else ' '
          surface.setForegroundColor(foreground)
          surface.setBackgroundColor(background)
          CharacterRenderer.renderChar(surface, screenX, screenY, charToRender)
      }

  private def columnsForRange(
    start: CursorPosition,
    end: CursorPosition,
    visualLine: TextVisualLine,
    markPoint: Boolean
  ): Option[(Int, Int)] =
    if visualLine.bufferLine < start.line || visualLine.bufferLine > end.line then None
    else if markPoint && start == end && visualLine.bufferLine == start.line then
      Option.when(start.column >= visualLine.startColumn && start.column <= visualLine.endColumn)(
        start.column -> (start.column + 1)
      )
    else
      val rangeStart =
        if visualLine.bufferLine == start.line then start.column else visualLine.startColumn
      val rangeEnd =
        if visualLine.bufferLine == end.line then end.column else visualLine.endColumn
      val clippedStart = math.max(rangeStart, visualLine.startColumn)
      val clippedEnd   = math.min(rangeEnd, visualLine.endColumn)
      Option.when(clippedStart < clippedEnd)(clippedStart -> clippedEnd)

  private def isInlineMarkdownLens(buffer: Buffer, state: AppState): Boolean =
    buffer.language.contains(LanguageId.Markdown) && state.config.markdownViewMode == MarkdownViewMode.InlineLens

  private def markdownLensFrameFor(buffer: Buffer, snapshot: TextLayoutSnapshot): MarkdownLensFrame =
    val previewWindow = markdownPreviewWindow(buffer, buffer.viewport.visibleLines)
    val lines = buffer.content.linesFrom(
      previewWindow.window.firstSourceLine,
      previewWindow.sourceLineCount
    )
    val activeRanges = activeMarkdownBlockRanges(buffer)
      .filter(range =>
        range.end >= previewWindow.window.firstSourceLine && range.start < previewWindow.window.firstSourceLine + lines.length
      )
      .map(range =>
        range.start.max(previewWindow.window.firstSourceLine) - previewWindow.window.firstSourceLine to
          range.end.min(previewWindow.window.firstSourceLine + lines.length - 1) - previewWindow.window.firstSourceLine
      )
    val baseRows = MarkdownDocumentPreview.inlinePreviewRows(
      lines,
      firstSourceLine = 0,
      maxSourceLines = lines.length
    )
    val (previewRows, placements) = markdownLensRows(
      baseRows,
      lines,
      activeRanges,
      buffer.cursors.map(_.line - previewWindow.window.firstSourceLine).toSet,
      snapshot,
      previewWindow.window,
      previewWindow.window.firstSourceLine
    )
    MarkdownLensFrame(
      previewWindow.window.firstSourceLine,
      lines,
      previewWindow.window,
      activeRanges,
      previewRows,
      placements
    )

  private def markdownLensRows(
    baseRows: Vector[MarkdownDocumentPreview.InlinePreviewLine],
    lines: Vector[String],
    activeRanges: List[Range.Inclusive],
    activeLines: Set[Int],
    snapshot: TextLayoutSnapshot,
    previewWindow: MarkdownDocumentPreview.PreviewWindow,
    firstSourceLine: Int
  ): (Vector[MarkdownDocumentPreview.InlinePreviewLine], Map[Range.Inclusive, MarkdownLensPlacement]) =
    val (rows, placements, _) = activeRanges.foldLeft(
      (baseRows, Map.empty[Range.Inclusive, MarkdownLensPlacement], 0)
    ) {
      case ((rows, placements, rowDelta), blockRange) =>
        val previewRange = MarkdownDocumentPreview.previewRowsForSourceRange(lines, blockRange).map { range =>
          (range.start - previewWindow.firstPreviewRow + rowDelta) to
            (range.end - previewWindow.firstPreviewRow + rowDelta)
        }
        val visibleActiveLine =
          snapshot.visualLines.exists(line => activeLines.contains(line.bufferLine - firstSourceLine))
        val rawHeight =
          if visibleActiveLine then
            snapshot.visualLines.count(line => blockRange.contains(line.bufferLine - firstSourceLine))
          else 0
        previewRange match
          case Some(range) if rawHeight > 0 && range.end >= 0 && range.start < rows.length =>
            val start        = range.start.max(0).min(rows.length)
            val endExclusive = (range.end + 1).max(start).min(rows.length)
            val replacedRows = endExclusive - start
            val replacement  = Vector.fill(rawHeight)(MarkdownDocumentPreview.InlinePreviewLine(None, ""))
            val nextRows     = rows.take(start) ++ replacement ++ rows.drop(endExclusive)
            (
              nextRows,
              placements + (blockRange -> MarkdownLensPlacement(start, rawHeight)),
              rowDelta + rawHeight - replacedRows
            )
          case _ => (rows, placements, rowDelta)
    }
    rows -> placements

  private def markdownPreviewWindow(buffer: Buffer, visibleRows: Int): MarkdownLensPreviewWindow =
    val lineCount = buffer.content.lineCount
    if lineCount == 0 then MarkdownLensPreviewWindow(MarkdownDocumentPreview.PreviewWindow(0, 0, ""), 0)
    else
      val activeLine = buffer.cursors.headOption
        .map(_.line)
        .filter(line => line >= 0 && line < lineCount)
      val activeBlock     = activeLine.map(line => markdownBlockForRenderer(buffer, line))
      val viewportTopLine = buffer.viewport.topLine.max(0).min(lineCount - 1)
      val windowTopLine = activeLine
        .filter(line => line == viewportTopLine && line > 0 && buffer.content.getLine(line).exists(_.trim.isEmpty))
        .filter(line => buffer.content.getLine(line - 1).exists(_.trim.matches("^#{1,6}\\s+.*")))
        .map(_ - 1)
        .getOrElse(viewportTopLine)
      val baseSourceLineLimit = markdownPreviewSourceLineLimit(visibleRows)
      val firstSourceLine = activeBlock
        .filter(blockRange => blockRange.end - blockRange.start + 1 <= baseSourceLineLimit)
        .filter(blockRange => blockRange.start < windowTopLine && blockRange.end >= windowTopLine)
        .map(_.start)
        .getOrElse(windowTopLine)
      val windowEndLine = firstSourceLine + baseSourceLineLimit - 1
      val maxSourceLines = activeBlock
        .filter(_ => activeLine.exists(_ <= windowEndLine))
        .filter(blockRange => blockRange.end - blockRange.start + 1 <= baseSourceLineLimit)
        .map(blockRange => math.max(baseSourceLineLimit, blockRange.end - firstSourceLine + baseSourceLineLimit))
        .getOrElse(baseSourceLineLimit)
      MarkdownLensPreviewWindow(
        window = MarkdownDocumentPreview.PreviewWindow(
          firstSourceLine = firstSourceLine,
          firstPreviewRow = MarkdownDocumentPreview
            .previewRowForSourceLine(
              buffer.content.linesFrom(firstSourceLine, math.min(maxSourceLines, lineCount - firstSourceLine)),
              0
            )
            .getOrElse(0),
          source = ""
        ),
        sourceLineCount = math.min(maxSourceLines, lineCount - firstSourceLine)
      )

  private def markdownPreviewSourceLineLimit(visibleRows: Int): Int =
    math.max(MinMarkdownPreviewSourceLines, visibleRows.max(1) * MarkdownPreviewOverscanFactor)

  private def activeMarkdownBlockRanges(buffer: Buffer): List[Range.Inclusive] =
    val lineCount = buffer.content.lineCount
    val cursorRanges = buffer.cursors
      .map(_.line)
      .filter(line => line >= 0 && line < lineCount)
      .map(line => markdownBlockForRenderer(buffer, line))
    val selectionRanges = buffer.allSelections.flatMap { selection =>
      if lineCount == 0 then Nil
      else
        val startLine = selection.start.line.max(0).min(lineCount - 1)
        val endLine   = selection.end.line.max(0).min(lineCount - 1)
        val selectedLines =
          if endLine - startLine <= MarkdownSelectionProbeLimit then startLine to endLine
          else List(startLine, endLine)
        selectedLines.map(line => markdownBlockForRenderer(buffer, line)).distinct
    }
    mergeOverlappingMarkdownRanges(cursorRanges ++ selectionRanges)

  private def mergeOverlappingMarkdownRanges(ranges: List[Range.Inclusive]): List[Range.Inclusive] =
    ranges
      .sortBy(range => (range.start, range.end))
      .foldLeft(List.empty[Range.Inclusive]) {
        case (last :: rest, range) if range.start <= last.end =>
          (last.start to last.end.max(range.end)) :: rest
        case (merged, range) =>
          range :: merged
      }
      .reverse

  final private case class MarkdownLensPlacement(top: Int, height: Int)

  private def renderMarkdownRawLenses(
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    frame: MarkdownLensFrame
  ): Unit =
    val lines         = frame.lines
    val previewWindow = frame.previewWindow
    frame.activeSourceRanges.foreach { blockRange =>
      val absoluteBlockRange = (blockRange.start + frame.firstSourceLine) to (blockRange.end + frame.firstSourceLine)
      val blockVisualLines   = snapshot.visualLines.filter(line => absoluteBlockRange.contains(line.bufferLine))
      if blockVisualLines.nonEmpty then
        val placement = frame.placements.getOrElse(
          blockRange,
          markdownLensPlacement(blockRange, blockVisualLines, rect.height, lines, previewWindow)
        )
        val lensY = rect.y + placement.top
        context.surface.setBackgroundColor(state.theme.panel.background)
        context.surface.fillRect(rect.x, lensY, rect.width, placement.height, ' ')
        blockVisualLines.zipWithIndex.foreach {
          case (visualLine, index) =>
            val screenY = lensY + index
            if screenY >= rect.y && screenY < rect.bottom && screenY >= 0 && screenY < context.surface.viewportHeight
            then
              context.surface.setForegroundColor(state.theme.foreground)
              context.surface.setBackgroundColor(state.theme.panel.background)
              if snapshot.usesMeasuredLayout then
                CharacterRenderer.renderMeasuredLineWithAnimation(
                  context.surface,
                  context.cellMetrics.toPixelX(rect.x).toFloat,
                  context.cellMetrics.toPixelY(screenY),
                  snapshot.lineHeightPx,
                  snapshot.ascentPx,
                  visualLine,
                  state.theme.copy(background = state.theme.panel.background),
                  buffer.animations,
                  syntaxHighlightingEnabled = false,
                  language = None,
                  clipRightXPx = Some(context.cellMetrics.toPixelX(rect.right).toFloat)
                )
              else
                CharacterRenderer.renderStringWithAnimation(
                  context.surface,
                  rect.x,
                  screenY,
                  visualLine.text,
                  state.theme.copy(background = state.theme.panel.background),
                  buffer.animations,
                  syntaxHighlightingEnabled = false,
                  language = None,
                  bufferLine = visualLine.bufferLine,
                  bufferStartColumn = visualLine.startColumn
                )
              renderSelectionHighlights(
                context.surface,
                buffer,
                visualLine,
                rect,
                screenY,
                context.cellMetrics.toPixelY(screenY),
                state.theme,
                context,
                snapshot
              )
        }
    }

  private def renderMarkdownLensCursors(
    buffer: Buffer,
    rect: LayoutRect,
    theme: Theme,
    config: AppConfig,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    frame: MarkdownLensFrame
  ): Unit =
    val lines         = frame.lines
    val previewWindow = frame.previewWindow
    frame.activeSourceRanges.foreach { blockRange =>
      val absoluteBlockRange = (blockRange.start + frame.firstSourceLine) to (blockRange.end + frame.firstSourceLine)
      val blockVisualLines   = snapshot.visualLines.filter(line => absoluteBlockRange.contains(line.bufferLine))
      if blockVisualLines.nonEmpty then
        val placement = frame.placements.getOrElse(
          blockRange,
          markdownLensPlacement(blockRange, blockVisualLines, rect.height, lines, previewWindow)
        )
        buffer.cursors.zipWithIndex.foreach { (cursor, cursorIndex) =>
          val isPrimaryCursor = cursorIndex == 0
          val shouldRenderCursor =
            context.cursorVisible || (buffer.cursors.size > 1 && !isPrimaryCursor)
          blockVisualLines.zipWithIndex.collectFirst {
            case (line, visualIndex)
                if line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn =>
              val xPx = line.xForColumn(cursor.column).getOrElse(line.widthPx)
              (visualIndex, xPx)
          } match
            case Some((visualLine, xPx)) if shouldRenderCursor =>
              val screenYCell = rect.y + placement.top + visualLine
              if screenYCell >= rect.y && screenYCell < rect.bottom &&
                  screenYCell >= 0 && screenYCell < context.surface.viewportHeight
              then
                val effectiveCursorColor = cursorColorFor(config, theme, context, isPrimaryCursor)
                val caretWidthPx         = math.max(2, math.round(context.cellMetrics.charWidth * 0.12f))
                val screenXPx            = context.cellMetrics.toPixelX(rect.x) + math.round(xPx)
                val screenYPx = TextRowMetrics(
                  contentRect = rect.copy(y = rect.y + placement.top, height = placement.height),
                  gridMetrics = context.cellMetrics,
                  rowLineHeightPx = context.cellMetrics.lineHeight,
                  usesMeasuredLayout = false
                ).cursorTopPx(visualLine)
                caretWithin(rect, context.cellMetrics, screenXPx, caretWidthPx).foreach { (caretXPx, widthPx) =>
                  context.surface.fillPixelRect(
                    caretXPx,
                    screenYPx,
                    widthPx,
                    context.cellMetrics.lineHeight,
                    effectiveCursorColor
                  )
                }
            case _ => ()
        }
    }

  private def markdownLensPlacement(
    blockRange: Range.Inclusive,
    blockVisualLines: Vector[TextVisualLine],
    visibleHeight: Int,
    markdownLines: Vector[String],
    previewWindow: MarkdownDocumentPreview.PreviewWindow
  ): MarkdownLensPlacement =
    val previewRange = MarkdownDocumentPreview.previewRowsForSourceRange(markdownLines, blockRange)
    val lensHeight = math.max(
      blockVisualLines.length,
      previewRange.map(range => range.end - range.start + 1).getOrElse(0)
    )
    val desiredTop = previewRange
      .map(_.start - previewWindow.firstPreviewRow)
      .getOrElse(blockRange.start - previewWindow.firstSourceLine)
    val visibleLensHeight = lensHeight.max(1).min(visibleHeight.max(1))
    MarkdownLensPlacement(
      top = desiredTop.max(0).min(math.max(0, visibleHeight - visibleLensHeight)),
      height = visibleLensHeight
    )

  private def renderEmptyPane(rect: LayoutRect, theme: Theme, context: RenderContext): Unit =
    val textMetrics  = CellMetrics.fromFont(context.textFont)
    val lineHeightPx = math.max(context.cellMetrics.lineHeight, textMetrics.lineHeight)
    val yPx          = centeredBlockTopPx(rect, context.cellMetrics, 1, lineHeightPx)
    context.surface.setFont(context.textFont)
    context.surface.setForegroundColor(theme.foreground)
    context.surface.setBackgroundColor(theme.background)
    renderAlignedTextLine(
      surface = context.surface,
      line = "Empty document — start typing",
      rect = rect,
      yPx = yPx,
      font = context.textFont,
      cellMetrics = context.cellMetrics,
      textMetrics = textMetrics
    )

  private def renderWelcomeText(rect: LayoutRect, theme: Theme, context: RenderContext): Unit =
    val lines = List(
      "Welcome to Serenity!",
      "",
      "Start typing to edit text.",
      "",
      "Press Ctrl+P for command palette"
    )

    val textMetrics  = CellMetrics.fromFont(context.textFont)
    val lineHeightPx = math.max(context.cellMetrics.lineHeight, textMetrics.lineHeight)
    val startYPx     = centeredBlockTopPx(rect, context.cellMetrics, lines.length, lineHeightPx)

    context.surface.setFont(context.textFont)
    context.surface.setForegroundColor(theme.muted)
    context.surface.setBackgroundColor(theme.background)

    lines.zipWithIndex.foreach {
      case (line, index) =>
        renderAlignedTextLine(
          surface = context.surface,
          line = line,
          rect = rect,
          yPx = startYPx + (index * lineHeightPx),
          font = context.textFont,
          cellMetrics = context.cellMetrics,
          textMetrics = textMetrics
        )
    }

  private def renderAlignedTextLine(
    surface: RenderSurface,
    line: String,
    rect: LayoutRect,
    yPx: Int,
    font: java.awt.Font,
    cellMetrics: CellMetrics,
    textMetrics: CellMetrics
  ): Unit =
    val lineHeightPx     = math.max(cellMetrics.lineHeight, textMetrics.lineHeight)
    val viewportHeightPx = surface.viewportHeight * cellMetrics.lineHeight
    if yPx + lineHeightPx > 0 && yPx < viewportHeightPx then
      surface.fontRenderContext match
        case Some(frc) =>
          val placement = TextAlignment.placeLine(
            text = line,
            area = TextAreaPx(
              xPx = cellMetrics.toPixelX(rect.x).toFloat,
              yPx = yPx,
              widthPx = rect.width * cellMetrics.charWidth,
              heightPx = lineHeightPx
            ),
            font = font,
            lineHeightPx = lineHeightPx,
            ascentPx = textMetrics.ascent,
            horizontal = TextHorizontalAlignment.Center,
            vertical = TextVerticalAlignment.Top,
            fontRenderContext = frc
          )
          surface.drawRunPx(
            placement.xPx,
            placement.yPx,
            placement.widthPx,
            placement.lineHeightPx,
            placement.ascentPx,
            line
          )
        case None =>
          val y       = cellMetrics.toRow(yPx)
          val centerX = rect.x + (rect.width - line.length) / 2
          if centerX >= 0 then CharacterRenderer.renderString(surface, centerX, y, line)

  private def renderStartPage(
    page: StartupPage,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    theme: Theme,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics
  ): Unit =
    surface.setFont(uiFont)
    val lines         = page.renderLines
    val lineHeightPx  = math.max(cellMetrics.lineHeight, uiMetrics.lineHeight)
    val totalHeightPx = lines.size * lineHeightPx
    val startYPx      = math.max(0, ((viewportSize.height * cellMetrics.lineHeight) - totalHeightPx) / 2)

    val actionLineToIndex = page.actionLineIndices.zipWithIndex.toMap
    val actionBounds =
      page.actionBounds(viewportSize, cellMetrics, uiMetrics).map(bounds => bounds.index -> bounds).toMap

    lines.zipWithIndex.foreach {
      case (line, lineIndex) =>
        val yPx = startYPx + (lineIndex * lineHeightPx)

        if yPx + lineHeightPx > 0 && yPx < viewportSize.height * cellMetrics.lineHeight then
          val optionIndex = actionLineToIndex.get(lineIndex)
          val isOption    = optionIndex.nonEmpty
          val isSelected  = optionIndex.contains(page.selectedIndex)

          if isSelected then
            surface.setForegroundColor(theme.highlighted.foreground)
            surface.setBackgroundColor(theme.highlighted.background)
            surface.enableStyle(theme.focusStyle)
            optionIndex.flatMap(actionBounds.get).foreach { bounds =>
              surface.fillPixelRect(
                xPx = bounds.xPx,
                yPx = bounds.yPx,
                widthPx = bounds.widthPx,
                heightPx = bounds.heightPx,
                color = theme.highlighted.background
              )
            }
            renderCenteredStartPageLine(surface, line, yPx, viewportSize, uiFont, cellMetrics, uiMetrics)
            surface.disableStyle(theme.focusStyle)
          else
            val foreground =
              if lineIndex == 0 || isOption then theme.foreground
              else theme.muted
            surface.setForegroundColor(foreground)
            surface.setBackgroundColor(theme.background)
            renderCenteredStartPageLine(surface, line, yPx, viewportSize, uiFont, cellMetrics, uiMetrics)
    }

  private def renderCenteredStartPageLine(
    surface: RenderSurface,
    line: String,
    yPx: Int,
    viewportSize: ViewportSize,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics
  ): Unit =
    surface.fontRenderContext match
      case Some(frc) =>
        val lineHeightPx = math.max(cellMetrics.lineHeight, uiMetrics.lineHeight)
        val placement = TextAlignment.placeLine(
          text = line,
          area = TextAreaPx(
            xPx = 0.0f,
            yPx = yPx,
            widthPx = viewportSize.width * cellMetrics.charWidth,
            heightPx = lineHeightPx
          ),
          font = uiFont,
          lineHeightPx = lineHeightPx,
          ascentPx = uiMetrics.ascent,
          horizontal = TextHorizontalAlignment.Center,
          vertical = TextVerticalAlignment.Top,
          fontRenderContext = frc
        )
        surface.drawRunPx(
          xPx = placement.xPx,
          yPx = placement.yPx,
          bgWidthPx = placement.widthPx,
          lineHeightPx = placement.lineHeightPx,
          ascentPx = placement.ascentPx,
          s = line
        )
      case None =>
        val y = cellMetrics.toRow(yPx)
        val x = math.max(0, (viewportSize.width - line.length) / 2)
        CharacterRenderer.renderString(surface, x, y, line)

  private def centeredBlockTopPx(
    rect: LayoutRect,
    cellMetrics: CellMetrics,
    lineCount: Int,
    lineHeightPx: Int
  ): Int =
    val contentTopPx    = cellMetrics.toPixelY(rect.y)
    val contentHeightPx = rect.height * cellMetrics.lineHeight
    contentTopPx + math.max(0, (contentHeightPx - (lineCount * lineHeightPx)) / 2)

  /** Paint every visible cursor in `buffer` and report the pixel rect each one occupies, so a caller can bound a
    * repaint to cover them.
    */
  private def renderCursors(
    buffer: Buffer,
    rect: LayoutRect,
    theme: Theme,
    config: AppConfig,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): List[PixelRect] =

    buffer.cursors.zipWithIndex.flatMap { (cursor, cursorIndex) =>
      val isPrimaryCursor = cursorIndex == 0
      val shouldRenderCursor =
        context.cursorVisible || (buffer.cursors.size > 1 && !isPrimaryCursor)
      calculateCursorVisualPosition(cursor, snapshot) match
        case Some((visualLine, xPx)) if shouldRenderCursor =>
          if visualLineVisible(rect, visualLine, context, snapshot)
          then
            val effectiveCursorColor = cursorColorFor(config, theme, context, isPrimaryCursor)
            val caretWidthPx         = math.max(2, math.round(context.cellMetrics.charWidth * 0.12f))
            val screenXPx            = context.cellMetrics.toPixelX(rect.x) + math.round(xPx)
            val screenYPx =
              textRowMetrics(rect, context, snapshot).cursorTopPx(visualLine)
            caretWithin(rect, context.cellMetrics, screenXPx, caretWidthPx) match
              case Some((caretXPx, widthPx)) =>
                context.surface.fillPixelRect(
                  caretXPx,
                  screenYPx,
                  widthPx,
                  snapshot.lineHeightPx,
                  effectiveCursorColor
                )
                List(PixelRect(caretXPx, screenYPx, widthPx, snapshot.lineHeightPx))
              case None => Nil
          else Nil
        case _ => Nil
    }

  private def calculateCursorVisualPosition(
    cursor: CursorPosition,
    snapshot: TextLayoutSnapshot
  ): Option[(Int, Float)] =
    snapshot.visualLines.zipWithIndex.collectFirst {
      case (line, visualIndex)
          if line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn =>
        val xPx = line.xForColumn(cursor.column).getOrElse(line.widthPx)
        (visualIndex, xPx)
    }

  private def measuredRunWidthWithin(
    rect: LayoutRect,
    context: RenderContext,
    startXPx: Float,
    endXPx: Float
  ): Option[Float] =
    val rightXPx = context.cellMetrics.toPixelX(rect.right).toFloat
    Option.when(startXPx < rightXPx)(math.max(0.0f, math.min(endXPx, rightXPx) - startXPx)).filter(_ > 0.0f)

  private def caretWithin(
    rect: LayoutRect,
    cellMetrics: CellMetrics,
    desiredXPx: Int,
    desiredWidthPx: Int
  ): Option[(Int, Int)] =
    val leftXPx  = cellMetrics.toPixelX(rect.x)
    val rightXPx = cellMetrics.toPixelX(rect.right)
    val widthPx  = math.min(math.max(1, desiredWidthPx), rightXPx - leftXPx)
    Option.when(widthPx > 0)(desiredXPx.max(leftXPx).min(rightXPx - widthPx) -> widthPx)

  private def cursorColorFor(
    config: AppConfig,
    theme: Theme,
    context: RenderContext,
    isPrimaryCursor: Boolean
  ): java.awt.Color =
    val activeColor = context.cursorColorOverride.getOrElse(config.cursorColors.activeOr(theme.cursor))
    if isPrimaryCursor then activeColor
    else config.cursorColors.inactiveOr(activeColor)

  private def renderFloatingPanels(state: AppState, context: RenderContext, scene: UiSceneSnapshot): Unit =
    context.surface.setFont(context.uiFont)
    val overlays = OverlayViewModel.fromState(state, scene)

    overlays.aboveCursor.foreach { overlay =>
      val blurRadius = SurfaceMaterials.effectiveBlurRadius(state.config)
      if blurRadius > 0f then renderFloatingBackdrop(overlay, blurRadius, state.config, context)
      TextOverlayRenderer.render(
        context.surface,
        overlay,
        state.theme,
        state.config,
        context.cursorVisible,
        context.uiFont,
        context.cellMetrics
      )
    }
    val belowOverlays =
      if overlays.belowCursorStack.nonEmpty then overlays.belowCursorStack else overlays.belowCursor.toList
    belowOverlays.foreach { overlay =>
      val blurRadius = SurfaceMaterials.effectiveBlurRadius(state.config)
      if blurRadius > 0f then renderFloatingBackdrop(overlay, blurRadius, state.config, context)
      TextOverlayRenderer.render(
        context.surface,
        overlay,
        state.theme,
        state.config,
        context.cursorVisible,
        context.uiFont,
        context.cellMetrics
      )
    }

  private def renderFloatingBackdrop(
    overlay: TextOverlayView,
    blurRadius: Float,
    config: AppConfig,
    context: RenderContext
  ): Unit =
    val offsetPx = FloatingSurfaceGeometry.signedRowOffsetPixels(overlay.verticalOffsetRows, context.cellMetrics)
    context.surface.withPixelTranslation(0.0, offsetPx) {
      context.surface.withRoundRectClip(
        overlay.rect.x,
        overlay.rect.y,
        overlay.rect.width,
        overlay.rect.height,
        config.uiCornerRadiusPx
      ) {
        context.surface.blurRegion(
          overlay.rect.x,
          overlay.rect.y,
          overlay.rect.width,
          overlay.rect.height,
          blurRadius
        )
      }
    }

  private def renderModalLayer(state: AppState, context: RenderContext, scene: UiSceneSnapshot): Unit =
    scene.modalBackdrop.foreach { backdrop =>
      context.surface.setAlpha(0.4f)
      context.surface.setBackgroundColor(state.theme.margin)
      context.surface.fillRect(
        backdrop.frameRect.x,
        backdrop.frameRect.y,
        backdrop.frameRect.width,
        backdrop.frameRect.height,
        ' '
      )
      context.surface.setAlpha(1.0f)
    }
    OverlayViewModel.fromState(state, scene).modal.foreach { overlay =>
      TextOverlayRenderer.render(
        context.surface,
        overlay,
        state.theme,
        state.config,
        context.cursorVisible,
        context.uiFont,
        context.cellMetrics
      )
    }

  private def renderPinnedPanels(
    state: AppState,
    context: RenderContext,
    scene: UiSceneSnapshot
  ): Unit =
    context.surface.setFont(context.uiFont)
    val surfaceNodes = scene.workspace.collect {
      case node @ SceneNode(SceneNodeId.Surface(surfaceId), _, _, _, _, _) => surfaceId -> node
    }.toMap
    (state.pinnedSurfaces ++ state.uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Expanded(_, _) => true
        case _                                  => false
    }).foreach {
      case surface @ UiSurface(_, content, SurfacePresentation.Pinned(position, _), _) =>
        surfaceNodes.get(surface.id).foreach { node =>
          val rect = node.frameRect
          val animationState =
            state.surfaceAnimations
              .get(surface.id)
              .map(_.animationState)
              .getOrElse(com.serenity.animation.AnimationState.empty)
          val blurRadius = SurfaceMaterials.effectiveBlurRadius(state.config)
          if blurRadius > 0f then
            context.surface.blurRegion(
              rect.x,
              rect.y,
              rect.width,
              rect.height,
              blurRadius
            )
          content match
            case SurfaceContent.MarkdownPreview(bufferId, title) =>
              renderMarkdownPreviewPanel(
                bufferId,
                title,
                rect,
                node.contentRect,
                state,
                context,
                animationState
              )
            case _ =>
              PinnedPanelRenderer.render(
                context.surface,
                PinnedPanelViewModel.resolve(surface, rect, state).copy(contentRect = Some(node.contentRect)),
                state.theme,
                state.config,
                animationState
              )
        }
      case surface @ UiSurface(_, content, SurfacePresentation.Expanded(_, _), _) =>
        surfaceNodes.get(surface.id).foreach { node =>
          val rect = node.frameRect
          val animationState =
            state.surfaceAnimations
              .get(surface.id)
              .map(_.animationState)
              .getOrElse(com.serenity.animation.AnimationState.empty)
          val blurRadius = SurfaceMaterials.effectiveBlurRadius(state.config)
          if blurRadius > 0f then
            context.surface.blurRegion(
              rect.x,
              rect.y,
              rect.width,
              rect.height,
              blurRadius
            )
          content match
            case SurfaceContent.MarkdownPreview(bufferId, title) =>
              renderMarkdownPreviewPanel(
                bufferId,
                title,
                rect,
                node.contentRect,
                state,
                context,
                animationState
              )
            case _ =>
              PinnedPanelRenderer.render(
                context.surface,
                PinnedPanelViewModel.resolve(surface, rect, state).copy(contentRect = Some(node.contentRect)),
                state.theme,
                state.config,
                animationState
              )
        }
      case _ => ()
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
    PinnedPanelRenderer.render(context.surface, shell, state.theme, state.config, animationState)

    val imageRect          = markdownPreviewImageRect(rect, contentRect, context)
    val contentWidthCells  = math.max(1, imageRect.width)
    val contentHeightCells = math.max(1, imageRect.height)
    val widthPx =
      scaledImagePixelDimension(contentWidthCells * context.cellMetrics.charWidth, context.surface.devicePixelScaleX)
    val heightPx =
      scaledImagePixelDimension(contentHeightCells * context.cellMetrics.lineHeight, context.surface.devicePixelScaleY)
    val buffer = state.buffers.get(bufferId)
    val content = buffer
      .map(buffer => markdownSplitPreviewWindow(buffer, contentHeightCells).source)
      .getOrElse("")
    val baseUri =
      buffer.flatMap(_.filePath).flatMap(path => Option(path.toAbsolutePath.getParent).map(_.toUri))
    val image = MarkdownDocumentPreview.renderImage(
      source = content,
      title = title,
      widthPx = widthPx,
      heightPx = heightPx,
      theme = state.theme,
      font = context.textFont,
      baseUri = baseUri,
      reuseLastRenderWhileEditing =
        buffer.exists(b => b.markdownPreviewEditGeneration != b.markdownPreviewCommittedGeneration)
    )
    context.surface.drawImage(image, imageRect.x, imageRect.y, contentWidthCells, contentHeightCells)

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
    val lineCount = buffer.content.lineCount
    if lineCount == 0 then MarkdownDocumentPreview.PreviewWindow(0, 0, "")
    else
      val maxSourceLines = markdownPreviewSourceLineLimit(visibleRows).max(1)
      val maxStart       = (lineCount - maxSourceLines).max(0)
      val fallbackStart  = buffer.viewport.topLine.max(0).min(maxStart)
      val anchorLine = buffer.cursors.headOption
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
        buffer.content.linesFrom(firstSourceLine, maxSourceLines).mkString("\n")
      )

  private def renderLineNumbers(state: AppState, context: RenderContext, renderPlan: EditorPaneRenderPlan): Unit =
    if state.config.showLineNumbers then
      context.surface.setFont(context.uiFont)
      renderPlan.layoutContract.lineNumberRect foreach { lineRect =>
        val surface = context.surface

        surface.setBackgroundColor(state.theme.panel.background)
        surface.setForegroundColor(state.theme.muted)

        surface.fillRect(lineRect.x, lineRect.y, lineRect.width, lineRect.height, ' ')
        state.layout.activeEditorPaneId
          .flatMap(state.layout.editorPanes.get)
          .foreach { pane =>
            val buffer = pane.bufferId.flatMap(state.buffers.get)
            val snapshot =
              state.layout.activeEditorPaneId
                .flatMap(renderPlan.snapshots.get)
                .orElse {
                  for
                    paneLayout <- state.layout.activeEditorPaneId.flatMap(renderPlan.paneLayouts.get)
                    buf        <- buffer
                  yield snapshotForBuffer(buf, paneLayout.contentRect, state, context)
                }
            snapshot.foreach { snapshot =>
              renderPlan.layoutContract.lineNumberRowSlots(snapshot.visualLines.length).foreach {
                case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), rowY)
                    if visualLineFits(lineRect, index, context, snapshot) =>
                  snapshot.visualLines.lift(index).foreach { visualLine =>
                    val lineTopPx = visualLineTopPx(lineRect, index, context, snapshot)
                    val rendersLineNumber =
                      shouldRenderLineNumberForVisualLine(visualLine, state.config.wordWrapEnabled)
                    val lineNumberText =
                      if rendersLineNumber then
                        val numberWidth = math.max(1, lineRect.width - 1)
                        (visualLine.bufferLine + 1).toString.reverse.padTo(numberWidth, ' ').reverse + " "
                      else continuationIndicatorText(lineRect.width)
                    val measuredLineNumberFont = buffer.filter(useMeasuredLineNumberFont(_, context))
                    if snapshot.usesMeasuredLayout && measuredLineNumberFont.nonEmpty then
                      measuredLineNumberFont.foreach(buf => surface.setFont(context.fontForBuffer(buf)))
                      surface.drawRunPx(
                        context.cellMetrics.toPixelX(lineRect.x).toFloat,
                        lineTopPx,
                        lineRect.width * context.cellMetrics.charWidth.toFloat,
                        snapshot.lineHeightPx,
                        snapshot.ascentPx,
                        lineNumberText
                      )
                      surface.setFont(context.uiFont)
                    else surface.putString(lineRect.x, rowY, lineNumberText)
                    if rendersLineNumber then
                      for
                        bufferId   <- pane.bufferId
                        annotation <- renderPlan.annotations.get(bufferId)
                      do
                        renderDiagnosticIndicator(
                          surface,
                          lineRect,
                          rowY,
                          annotation.diagnosticsByLine.getOrElse(visualLine.bufferLine, Nil),
                          state
                        )
                  }
                case _ => ()
              }
            }
          }
      }

  private def useMeasuredLineNumberFont(buffer: Buffer, context: RenderContext): Boolean =
    buffer.typographyRole != TypographyRole.Code && context.fontForBuffer(buffer) != context.codeFont

  private def shouldRenderLineNumberForVisualLine(visualLine: TextVisualLine, wordWrapEnabled: Boolean): Boolean =
    !wordWrapEnabled || visualLine.startColumn == 0

  private def continuationIndicatorText(width: Int): String =
    val safeWidth = math.max(1, width)
    val leftWidth = (safeWidth - 1) / 2
    " " * leftWidth + "│" + " " * (safeWidth - leftWidth - 1)

  private def renderDiagnosticIndicator(
    surface: RenderSurface,
    lineRect: LayoutRect,
    screenY: Int,
    lineDiags: List[com.serenity.lsp.model.Diagnostic],
    state: AppState
  ): Unit =
    if lineDiags.nonEmpty then
      val worstCode = lineDiags.flatMap(_.severity).map(_.code).minOption
      val color = worstCode match
        case Some(1) => state.theme.error.foreground
        case Some(2) => state.theme.warning.foreground
        case _       => state.theme.muted
      surface.setForegroundColor(color)
      surface.setBackgroundColor(state.theme.panel.background)
      surface.putString(lineRect.x + lineRect.width - 1, screenY, "!")

  private def renderGutter(state: AppState, context: RenderContext, contract: EditorLayoutContract): Unit =
    contract.gutterRect.foreach { gutterRect =>
      context.surface.setFont(context.uiFont)
      val surface = context.surface

      surface.setBackgroundColor(state.theme.panel.background)
      surface.setForegroundColor(state.theme.panel.foreground)

      surface.fillRect(gutterRect.x, gutterRect.y, gutterRect.width, gutterRect.height, ' ')

      val gutterContent = buildGutterContent(state)
      val displayContent =
        if gutterContent.length > gutterRect.width then gutterContent.take(gutterRect.width - 3) + "..."
        else gutterContent

      drawUiTextInCellRect(surface, context, gutterRect, displayContent)
    }

  private def drawUiTextInCellRect(
    surface: RenderSurface,
    context: RenderContext,
    rect: LayoutRect,
    text: String
  ): Unit =
    val rowHeightPx  = math.max(1, rect.height * context.cellMetrics.lineHeight)
    val lineHeightPx = math.max(1, math.min(context.uiMetrics.lineHeight, rowHeightPx - 2))
    val ascentPx     = math.max(1, math.min(context.uiMetrics.ascent, lineHeightPx))
    val placement = TextAlignment.placeLine(
      text = text,
      area = TextAreaPx(
        xPx = context.cellMetrics.toPixelX(rect.x).toFloat,
        yPx = context.cellMetrics.toPixelY(rect.y),
        widthPx = rect.width * context.cellMetrics.charWidth.toFloat,
        heightPx = rowHeightPx
      ),
      font = context.uiFont,
      lineHeightPx = lineHeightPx,
      ascentPx = ascentPx,
      horizontal = TextHorizontalAlignment.Left,
      vertical = TextVerticalAlignment.Middle,
      fontRenderContext = surface.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
    )

    surface.drawRunPx(
      xPx = placement.xPx,
      yPx = placement.yPx,
      bgWidthPx = placement.widthPx,
      lineHeightPx = placement.lineHeightPx,
      ascentPx = placement.ascentPx,
      s = text
    )

  private def buildGutterContent(state: AppState): String =
    if state.config.cursorInfoBarPlacement == CursorInfoBarPlacement.PinnedBottom then
      state.cursorInfoBarText.map(text => s" $text ").getOrElse(legacyGutterContent(state))
    else legacyGutterContent(state)

  private def legacyGutterContent(state: AppState): String =
    state.layout.activeEditorPaneId.flatMap(state.layout.editorPanes.get) match
      case Some(pane) =>
        pane.bufferId.flatMap(state.buffers.get) match
          case Some(buffer) =>
            val cursor   = buffer.cursors.headOption.getOrElse(CursorPosition(0, 0))
            val position = s"Line ${cursor.line + 1}, Col ${cursor.column + 1}"
            val language = buffer.language.fold("Plain Text")(_.displayName)

            val filePath = buffer.filePath match
              case Some(path) => s" | ${path.getFileName}"
              case None       => " | Not saved to file yet"

            s" $position | Language: $language$filePath "
          case None => " No active buffer "
      case None => " No active editor pane "
