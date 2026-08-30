package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.{Color, Font}
import java.util.concurrent.atomic.AtomicReference

import scala.jdk.CollectionConverters.*

import com.serenity.animation.ThemeInterpolator
import com.serenity.config.{AppConfig, CursorInfoBarPlacement, CursorMode, MarkdownViewMode, PostProcessingEffect}
import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.MarkdownDocumentPreview
import com.serenity.state.manager.{AuthoritativeUiScene, FocusedTextBody}
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
    uiMetrics: CellMetrics,
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState] = Map.empty
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

  private val MarkdownSelectionProbeLimit = 512

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

  /** The render parameters that shape a frame but are not part of `AppState`, so `DamageProducer` -- which only diffs
    * `AppState` -- has no way to see them change: the window's pixel size, the three fonts, and the metrics/overrides a
    * caller derives from them. Tracked per persistence key so a mismatch against the last frame drawn with that key can
    * force a full redraw the same blunt way `Renderer`'s retired `ChromeKey` did by including these fields directly in
    * its own structural comparison.
    */
  final private case class RenderInputs(
      viewportSize: ViewportSize,
      codeFont: Font,
      textFont: Font,
      uiFont: Font,
      cellMetrics: CellMetrics,
      uiMetrics: CellMetrics,
      cursorVisible: Boolean,
      cursorColor: Option[java.awt.Color]
  )

  private def renderInputsFor(context: RenderContext, viewportSize: ViewportSize): RenderInputs =
    RenderInputs(
      viewportSize,
      context.codeFont,
      context.textFont,
      context.uiFont,
      context.cellMetrics,
      context.uiMetrics,
      context.cursorVisible,
      context.cursorColorOverride
    )

  /** Per-pane geometry for this frame: which buffer line each visual row shows, to translate `Damage`'s buffer-line
    * facts into row indices, and the pixel band each row owns, to know what a preserved row's pixels actually cover.
    */
  final private case class PaneFrameRecord(
      bufferId: BufferId,
      rowBufferLines: Vector[Int],
      rowRects: Vector[PixelRect],
      overflowingRows: Set[Int]
  )

  /** What a frame decided to reuse: the rows it still has to draw per pane and the pixel bands it kept. */
  final private case class FramePlan(
      dirtyRowsByPane: Map[PaneId, Set[Int]],
      preserved: List[PixelRect],
      repaintRegion: Option[PixelRect],
      boundedRepaint: Boolean
  )

  /** Identity of the screen a frame publishes to -- a newtype over the backing canvas/surface's own reference identity,
    * for the same reason [[SurfaceContentIdentity]] wraps a pixel buffer's.
    */
  opaque type ScreenIdentity = AnyRef

  private object ScreenIdentity:
    def apply(value: AnyRef): ScreenIdentity = value

  /** Where a frame goes: the screen it will be shown on, and the region sink that screen's repaint should honour. */
  final private case class FrameOutput(screenToken: ScreenIdentity, repaintRegion: AtomicReference[Option[PixelRect]])

  /** Per-persistence-key bookkeeping this module remembers across frames, replacing the retired `ChromeKey`/
    * `FrameRecord`/`PaneContentKey`/`PaneRowKey` reconstruct-and-diff machinery. Nothing here holds a structural
    * snapshot of a frame to diff against -- staleness is `Damage`, reported by the code that made a change, accumulated
    * across however many frames a pixel buffer or the screen sits unvisited via [[DamageAccumulator]]. What is kept is
    * only enough to know when that accumulation cannot be trusted on its own: the set of panes actually drawn (a pane
    * can appear or disappear -- e.g. an empty buffer gaining content -- without the layout reference changing, which is
    * `paneChromeDamage`'s usual signal for that), and the non-`AppState` [[RenderInputs]] above.
    *
    * Weak keys: the base image pool recycles a small number of images, and an image the pool drops must not be held
    * alive by this cache. Access is synchronised because the render thread is not guaranteed to be a single thread
    * across the app's lifetime.
    */
  final private case class DrawState(paneIds: Set[PaneId], inputs: RenderInputs)

  private val bufferDamage    = new java.util.WeakHashMap[SurfaceContentIdentity, Damage]()
  private val bufferDrawState = new java.util.WeakHashMap[SurfaceContentIdentity, DrawState]()

  /** Which screen each tracked buffer identity was last drawn for, so [[accumulateBufferDamage]] only folds a frame's
    * damage into buffers that actually belong to the same screen -- e.g. the same `SwingWindow`'s own two pooled
    * images. Without this, every buffer identity ever tracked shares one process-wide map with no notion of which ones
    * are actually part of the same image pool: harmless in production (there is only ever one real window, so "every
    * tracked identity" and "every identity in this window's pool" are the same set), but any other concurrently running
    * render session -- another window, or another independent `render` call in the same process -- would otherwise have
    * its damage silently mixed into this one's, and vice versa.
    */
  private val bufferScreen = new java.util.WeakHashMap[SurfaceContentIdentity, ScreenIdentity]()

  /** Distinct from [[bufferDamage]] because base images alternate: the pixels a surface preserves come from two frames
    * ago, while the screen shows the last one published.
    */
  private val screenDamage  = new java.util.WeakHashMap[ScreenIdentity, Damage]()
  private val screenPaneIds = new java.util.WeakHashMap[ScreenIdentity, Set[PaneId]]()

  /** What [[paintModalLayer]] cached the last time it painted the modal layer into its own buffer (#1100 stage 2): the
    * flushed image, and the frame shape/cursor-blink state it was painted for. Reused only when a later frame's modal
    * layer is clean (see [[DamageProducer]]'s `modalOnlyContentChange`) *and* still matches both fields -- a viewport
    * resize or a blink toggle invalidates the cache even though neither is `AppState`, so a stale buffer is never
    * composited over a frame it no longer matches. One slot per surface, not keyed by frame-buffer identity, because
    * exactly one modal is ever open at a time on a given surface and the cached image is just a source for `drawImage`,
    * independent of which of the (possibly pooled) frame buffers it gets composited onto.
    */
  final private case class CachedModalLayer(
      image: BufferedImage,
      viewportWidth: Int,
      viewportHeight: Int,
      cursorVisible: Boolean
  )

  /** Keyed by the [[RenderSurface]] a frame was painted onto, exactly like [[bufferScreen]] above and for the same
    * reason: a single JVM-wide slot would let one surface's cached modal image leak into another surface's frame --
    * harmless in production (there is only ever one real window) but a real hazard for any other concurrently running
    * render session in the same process, tests included, since `Renderer` is a singleton every suite shares. A
    * `WeakHashMap` (rather than the `AtomicReference` this used to be) lets a surface's entry disappear once nothing
    * else references it, instead of pinning every `RenderSurface` a process ever rendered to for its whole lifetime.
    */
  private val modalLayerBuffers = new java.util.WeakHashMap[RenderSurface, CachedModalLayer]()

  /** What [[paintPanelLayer]] cached the last time it painted a given pinned/expanded/floating panel into its own
    * buffer (#1100 stage 3): the [[CachedModalLayer]] pattern generalised to every panel kind that reads pixels back
    * off the frame surface via `blurRegion`, keyed by [[SurfaceId]] rather than held as a single slot, since -- unlike
    * the modal -- more than one of these panels can be on screen at once. `frameRect` is remembered alongside viewport
    * shape and cursor-blink state because a panel's own rect can shift (another panel appearing/disappearing reflows
    * pinned layout) without the panel's own `UiSurface` fields changing, which [[Damage.Surface]] narrowing alone would
    * not catch.
    */
  final private case class CachedPanelLayer(
      image: BufferedImage,
      viewportWidth: Int,
      viewportHeight: Int,
      cursorVisible: Boolean,
      frameRect: LayoutRect
  )

  /** Keyed by [[RenderSurface]] first and [[SurfaceId]] second, for the same cross-surface-leak reason as
    * [[modalLayerBuffers]] -- a bare `Map[SurfaceId, CachedPanelLayer]` shared process-wide let any two independently
    * rendered surfaces that happen to reuse the same `SurfaceId` (unremarkable: tests across many specs all use
    * `SurfaceId("outline")`) stomp on each other's cached panel image.
    */
  private val panelLayerBuffers = new java.util.WeakHashMap[RenderSurface, Map[SurfaceId, CachedPanelLayer]]()

  /** Folds `damage` into every buffer identity already being tracked for `output`'s screen (every identity, if there is
    * no output to scope by), via [[DamageAccumulator.accumulateBuffers]] -- called unconditionally on every frame,
    * regardless of which identity (if any) this specific frame draws into, so a pixel buffer sitting idle through
    * several frames still accrues what each of them changed.
    */
  private def accumulateBufferDamage(output: Option[FrameOutput], damage: Damage): Unit =
    bufferDamage.synchronized {
      val tracked = mapAsScala(bufferDamage)
      val scoped = output match
        case None => tracked
        case Some(value) =>
          tracked.filter { case (key, _) => Option(bufferScreen.get(key)).forall(_ == value.screenToken) }
      val updated = DamageAccumulator.accumulateBuffers(scoped, damage)
      updated.foreach { case (key, value) => val _ = bufferDamage.put(key, value) }
    }

  /** Reports and resets what has accumulated for `persistenceKey` since it was last drawn into, via
    * [[DamageAccumulator.observeBufferDraw]] -- `Damage.Everything` if this is the first time this identity has been
    * seen, since an untracked identity's pixels cannot be trusted at all, not merely assumed unchanged. Also records
    * `output`'s screen as this identity's owner, so future [[accumulateBufferDamage]] calls scope correctly.
    */
  private def drainBufferDamage(output: Option[FrameOutput], persistenceKey: SurfaceContentIdentity): Damage =
    bufferDamage.synchronized {
      output.foreach(value => bufferScreen.put(persistenceKey, value.screenToken))
      val wasTracked          = bufferDamage.containsKey(persistenceKey)
      val (observed, updated) = DamageAccumulator.observeBufferDraw(mapAsScala(bufferDamage), persistenceKey)
      updated.foreach { case (key, value) => val _ = bufferDamage.put(key, value) }
      if wasTracked then observed else Damage.Everything
    }

  /** Records `paneIds`/`inputs` as this identity's current draw state and reports whether either differs from what was
    * remembered -- a pane appearing or disappearing without a layout-reference change (an empty buffer gaining
    * content), or a render parameter `DamageProducer` cannot see because it isn't part of `AppState` (a window resize,
    * a font swap), both mean the accumulated buffer damage alone cannot be trusted for this frame.
    */
  private def drawStateChanged(
    persistenceKey: SurfaceContentIdentity,
    paneIds: Set[PaneId],
    inputs: RenderInputs
  ): Boolean =
    bufferDrawState.synchronized {
      val next     = DrawState(paneIds, inputs)
      val previous = Option(bufferDrawState.get(persistenceKey))
      val _        = bufferDrawState.put(persistenceKey, next)
      !previous.contains(next)
    }

  /** Folds `damage` into the screen's own accumulated total for `output`'s screen, via
    * [[DamageAccumulator.accumulateScreen]]. Distinct from [[accumulateBufferDamage]] because exactly one buffer is
    * ever on screen at a time, so there is only ever one running total to fold into, not one per tracked identity.
    */
  private def accumulateScreenDamage(output: Option[FrameOutput], damage: Damage): Unit =
    output.foreach { value =>
      screenDamage.synchronized {
        val current = Option(screenDamage.get(value.screenToken)).getOrElse(Damage.Nothing)
        val _       = screenDamage.put(value.screenToken, DamageAccumulator.accumulateScreen(current, damage))
      }
    }

  /** Reports and resets what has accumulated for the screen since it was last published to, via
    * [[DamageAccumulator.observeScreenPublish]] -- `Damage.Everything` when there is no output to bound a repaint
    * against, or this is the first publish this screen has ever been tracked for.
    */
  private def drainScreenDamage(output: Option[FrameOutput]): Damage =
    output match
      case None => Damage.Everything
      case Some(value) =>
        screenDamage.synchronized {
          val wasTracked        = screenDamage.containsKey(value.screenToken)
          val current           = Option(screenDamage.get(value.screenToken)).getOrElse(Damage.Nothing)
          val (observed, reset) = DamageAccumulator.observeScreenPublish(current)
          val _                 = screenDamage.put(value.screenToken, reset)
          if wasTracked then observed else Damage.Everything
        }

  /** As [[drawStateChanged]], but for the screen's own published pane set -- distinct because the screen shows the
    * previous frame while a surface's own preserved pixels come from the one before that (see [[screenDamage]]).
    */
  private def screenPaneIdsChanged(output: Option[FrameOutput], paneIds: Set[PaneId]): Boolean =
    output match
      case None => true
      case Some(value) =>
        screenPaneIds.synchronized {
          val previous = Option(screenPaneIds.get(value.screenToken))
          val _        = screenPaneIds.put(value.screenToken, paneIds)
          !previous.contains(paneIds)
        }

  private def mapAsScala(map: java.util.Map[SurfaceContentIdentity, Damage]): Map[SurfaceContentIdentity, Damage] =
    map.asScala.toMap

  /** Logical pixels map one-to-one onto canvas component pixels: the canvas scales the frame image back to its own
    * logical size when it paints, so a region measured against the frame is already in component coordinates.
    */
  private def toAwtRectangle(rect: PixelRect): java.awt.Rectangle =
    new java.awt.Rectangle(rect.xPx, rect.yPx, rect.widthPx, rect.heightPx)

  private def withEffectiveTheme(state: AppState): AppState =
    state.runtime.themeTransition match
      case None => state
      case Some(t) =>
        state.copy(persisted =
          state.persisted.copy(theme = ThemeInterpolator.blend(t.previousTheme, state.persisted.theme, t.progress))
        )

  private def startPageOnly(state: AppState): Option[StartupPage] =
    state.runtime.uiSurfaces match
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
    surface.clearViewport(state.persisted.theme.background)
    forgetPreservedContent(surface, output)
    renderStartPage(page, surface, viewportSize, state.persisted.theme, uiFont, cellMetrics, uiMetrics)
    surface.effects.foreach(_.applyPostProcessing(state.persisted.config.surfaceConfig.postProcessingEffect))
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
    repaintOnFlush: Boolean,
    damage: Damage = Damage.Everything,
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState] = Map.empty
  ): Unit =
    val state0 = withEffectiveTheme(state)
    // Set while the frame is drawn, read when it is flushed: None asks for a whole-canvas repaint, Some(rect) for a
    // repaint bounded to the pane rows this frame actually changed.
    val repaintRegion = new AtomicReference[Option[PixelRect]](None)
    val output        = Some(FrameOutput(ScreenIdentity(swingWin.canvas), repaintRegion))
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
    // A Swing/Java2D surface always has a real FontRenderContext to measure against, so this scene keeps its
    // long-standing behaviour of deriving cell metrics from `codeFont` and letting `TextLayoutSnapshot` auto-detect
    // measured vs. cell layout per buffer font -- no override needed (#1215's cellMetrics override is for a surface
    // with no real font rendering at all, i.e. TUI's `TerminalRenderSurface`, reached only through the
    // surface-generic entry points below).
    val _ = withSceneIfNeeded(
      state0,
      AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont)
    )(page => renderStartPageFrame(state0, page, surface, viewportSize, uiFont, swingWin.metrics, uiMetrics, output)) {
      scene =>
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
          output,
          damage,
          bufferAnimations
        )
    }

  /** The render plan a cursor-only redraw needs: the cached one from the last full layout, if it is still valid for
    * these fonts/metrics/viewport, or a freshly prepared one otherwise. Shared by every cursor-only entry point (Swing
    * and surface-generic alike) so the cache is consulted exactly the same way regardless of shell.
    */
  private def resolveCursorRenderPlan(
    state0: AppState,
    authoritativeScene: UiSceneSnapshot,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    cursorVisible: Boolean,
    cursorColor: Option[java.awt.Color],
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics
  ): (CalculatedLayout, EditorPaneRenderPlan) =
    preparedSceneRef
      .get()
      .filter(_.matches(authoritativeScene, codeFont, textFont, uiFont, cellMetrics, uiMetrics, viewportSize))
      .map(value => value.scene.calculatedLayout -> value.renderPlan)
      .getOrElse {
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
          cellMetrics,
          uiMetrics
        )
        preparedSceneRef.set(Some(next))
        next.scene.calculatedLayout -> next.renderPlan
      }

  /** Draw just the cursor glyphs for `renderPlan` into `surface` and flush. Shared tail of every cursor-only /
    * cursor-overlay entry point: the only thing that differs between them is which surface the cursor lands on and how
    * that surface's owning shell was told a fresh layout was needed.
    */
  private def paintCursorsOnly(
    state0: AppState,
    surface: RenderSurface,
    layout: CalculatedLayout,
    renderPlan: EditorPaneRenderPlan,
    cursorVisible: Boolean,
    cursorColor: Option[java.awt.Color],
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState] = Map.empty
  ): List[PixelRect] =
    val context = RenderContext(
      surface,
      layout,
      cursorVisible,
      cursorColor,
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      bufferAnimations
    )
    val cursorRects = renderEditorCursors(state0, context, renderPlan)
    presentHardwareCursor(surface, state0, cursorVisible, cursorRects, cellMetrics)
    surface.flush()
    cursorRects

  /** #1170: on a surface that can delegate the caret to a real hardware/terminal cursor, position and style it from the
    * primary caret's rect instead of leaving it to be painted as surface content -- except in breathe mode, which
    * animates color/opacity over time (something a DECSCUSR style can't represent) and so stays the documented,
    * app-painted exception. A surface with no hardware cursor to delegate to (`hardwareCursor` is `None`, i.e. every
    * GUI canvas) is entirely unaffected: this is a no-op there.
    */
  private def presentHardwareCursor(
    surface: RenderSurface,
    state0: AppState,
    cursorVisible: Boolean,
    cursorRects: List[PixelRect],
    cellMetrics: CellMetrics
  ): Unit =
    surface.hardwareCursor.foreach { hardwareCursor =>
      (state0.persisted.config.cursorMode, cursorRects.headOption) match
        case (CursorMode.Blink, Some(rect)) if cursorVisible =>
          hardwareCursor.present(
            cellMetrics.toCol(rect.xPx),
            cellMetrics.toRow(rect.yPx),
            HardwareCursorStyle(HardwareCursorShape.Block, blinking = true)
          )
        case _ =>
          hardwareCursor.hide()
    }

  /** Surface-generic form of [[renderCursorOnly]]: redraws just the cursor glyphs into `surface`, reusing the cached
    * render plan from the last full layout when it is still valid. `false` only for a start-page-only state, which has
    * no cursor to draw.
    */
  def renderCursorOnly(
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
  ): Boolean =
    renderCursorOnly(
      state,
      cursorVisible,
      surface,
      viewportSize,
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      cursorColor,
      Map.empty
    )

  def renderCursorOnly(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color],
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState]
  ): Boolean =
    val state0 = withEffectiveTheme(state)
    // #1105/#1215: a surface reporting no FontRenderContext (a terminal) has no real font rendering to measure
    // against at all -- its own declared cellMetrics must reach this scene's cell-based layout, and must force it,
    // rather than being silently discarded by the font's own measured-vs-cell auto-detection. A surface that does
    // report one (every GUI canvas, and any other surface-generic caller with real font rendering) keeps this scene's
    // long-standing auto-detected behaviour untouched.
    val cellMetricsOverride = Option.when(surface.text.fontRenderContext.isEmpty)(cellMetrics)
    withSceneIfNeeded(
      state0,
      AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont, cellMetrics = cellMetricsOverride)
    )(_ => false) { authoritativeScene =>
      val (layout, renderPlan) = resolveCursorRenderPlan(
        state0,
        authoritativeScene,
        surface,
        viewportSize,
        cursorVisible,
        cursorColor,
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics
      )
      val _ = paintCursorsOnly(
        state0,
        surface,
        layout,
        renderPlan,
        cursorVisible,
        cursorColor,
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics,
        bufferAnimations
      )
      true
    }

  def renderCursorOnly(
    state: AppState,
    cursorVisible: Boolean,
    swingWin: com.serenity.ui.terminal.SwingWindow,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color],
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState] = Map.empty
  ): Boolean =
    val state0       = withEffectiveTheme(state)
    val viewportSize = swingWin.viewportSize
    // Swing/Java2D always has a real FontRenderContext -- no cellMetrics override needed, see the render() entry
    // point above.
    withSceneIfNeeded(
      state0,
      AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont)
    )(_ => false) { authoritativeScene =>
      // No base content is redrawn by this call, so the base frame contributes nothing to the repaint bound --
      // only the cursor's own old and new pixel rects can have changed on screen.
      swingWin.onCursorOverlayReady(Some(new java.awt.Rectangle(0, 0, 0, 0))) { image =>
        val surface =
          Java2DRenderSurface.forImage(image, swingWin.metrics, codeFont, swingWin.canvas, _ => ())
        val (layout, renderPlan) = resolveCursorRenderPlan(
          state0,
          authoritativeScene,
          surface,
          viewportSize,
          cursorVisible,
          cursorColor,
          codeFont,
          textFont,
          uiFont,
          swingWin.metrics,
          uiMetrics
        )
        paintCursorsOnly(
          state0,
          surface,
          layout,
          renderPlan,
          cursorVisible,
          cursorColor,
          codeFont,
          textFont,
          uiFont,
          swingWin.metrics,
          uiMetrics,
          bufferAnimations
        ).map(toAwtRectangle)
      }
    }

  /** Surface-generic form of [[renderWithCursorOverlay]]: renders a base frame with the cursor left out, then draws the
    * cursor directly on top of the same surface and flushes again. Unlike the Swing form, which composites the cursor
    * into a separate overlay image so blinking never has to redraw the base, this issues two flushes -- fine for any
    * `RenderSurface`, and inexpensive on a damage-diffed one, where the second flush only ever touches the handful of
    * cells the cursor occupies.
    */
  def renderWithCursorOverlay(
    state: AppState,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
  ): Boolean =
    renderWithCursorOverlay(
      state,
      surface,
      viewportSize,
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      cursorColor,
      Damage.Everything
    )

  def renderWithCursorOverlay(
    state: AppState,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color],
    damage: Damage
  ): Boolean =
    val state0        = withEffectiveTheme(state)
    val repaintRegion = new AtomicReference[Option[PixelRect]](None)
    val output        = Some(FrameOutput(ScreenIdentity(surface), repaintRegion))
    // #1105/#1215: see the surface-generic renderCursorOnly above for why this is scoped to a surface with no real
    // FontRenderContext.
    val cellMetricsOverride = Option.when(surface.text.fontRenderContext.isEmpty)(cellMetrics)
    withSceneIfNeeded(
      state0,
      AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont, cellMetrics = cellMetricsOverride)
    ) { page =>
      renderStartPageFrame(state0, page, surface, viewportSize, uiFont, cellMetrics, uiMetrics, output)
      true
    } { scene =>
      renderFrame(
        state0,
        cursorVisible = false,
        surface,
        viewportSize,
        scene,
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics,
        cursorColor = None,
        output,
        damage
      ).fold(false) { renderPlan =>
        val _ = paintCursorsOnly(
          state0,
          surface,
          scene.calculatedLayout,
          renderPlan,
          true,
          cursorColor,
          codeFont,
          textFont,
          uiFont,
          cellMetrics,
          uiMetrics
        )
        true
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
    cursorColor: Option[java.awt.Color],
    damage: Damage = Damage.Everything,
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState] = Map.empty
  ): Boolean =
    val state0       = withEffectiveTheme(state)
    val viewportSize = swingWin.viewportSize
    // The caret is composited from a separate overlay image, so this base frame is repainted whole every time; the
    // record is still kept up to date so the next frame knows what the screen is showing.
    val repaintRegion = new AtomicReference[Option[PixelRect]](None)
    val output        = Some(FrameOutput(ScreenIdentity(swingWin.canvas), repaintRegion))
    // The pooled acquirer is what lets this frame reuse the pixels of the last frame drawn into the same image; the
    // cursor rides on a separate overlay image, so the base frame here is pure pane content and chrome.
    val surface = Java2DRenderSurface.forFrame(
      swingWin.metrics,
      codeFont,
      swingWin.canvas,
      swingWin.onBaseImageReady,
      swingWin.acquireBaseImage
    )
    // Swing/Java2D always has a real FontRenderContext -- no cellMetrics override needed, see the render() entry
    // point above.
    withSceneIfNeeded(
      state0,
      AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont)
    ) { page =>
      renderStartPageFrame(state0, page, surface, viewportSize, uiFont, swingWin.metrics, uiMetrics, output)
      // The base frame published above went through onBaseImageReady, which does not repaint the canvas by
      // itself -- only onCursorOverlayReady does. The editor branch below reaches it naturally via its cursor
      // composite step; the startup page has no cursor to draw, but still needs this call to actually get painted.
      // A start page also has no persisted content to reason about, so this always forces a full repaint.
      swingWin.onCursorOverlayReady(None)(_ => Nil)
    } { scene =>
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
        output,
        damage,
        bufferAnimations
      ).fold(false) { renderPlan =>
        val baseDirtyRegion = repaintRegion.get().map(toAwtRectangle)
        swingWin.onCursorOverlayReady(baseDirtyRegion) { image =>
          val cursorSurface =
            Java2DRenderSurface.forImage(image, swingWin.metrics, codeFont, swingWin.canvas, _ => ())
          paintCursorsOnly(
            state0,
            cursorSurface,
            scene.calculatedLayout,
            renderPlan,
            true,
            cursorColor,
            codeFont,
            textFont,
            uiFont,
            swingWin.metrics,
            uiMetrics,
            bufferAnimations
          ).map(toAwtRectangle)
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
    render(state, cursorVisible, surface, viewportSize, codeFont, textFont, cellMetrics, cursorColor, Damage.Everything)

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    cellMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color],
    damage: Damage
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
      cursorColor,
      damage
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
    render(
      state,
      cursorVisible,
      surface,
      viewportSize,
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      cursorColor,
      Damage.Everything
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
    cursorColor: Option[java.awt.Color],
    damage: Damage
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
      cursorColor,
      damage
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
    cursorColor: Option[java.awt.Color],
    damage: Damage = Damage.Everything
  ): Option[PixelRect] =
    val state0        = withEffectiveTheme(state)
    val repaintRegion = new AtomicReference[Option[PixelRect]](None)
    val output        = Some(FrameOutput(ScreenIdentity(surface), repaintRegion))
    // #1105/#1215: see the surface-generic renderCursorOnly above for why this is scoped to a surface with no real
    // FontRenderContext.
    val cellMetricsOverride = Option.when(surface.text.fontRenderContext.isEmpty)(cellMetrics)
    val _ = withSceneIfNeeded(
      state0,
      AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont, cellMetrics = cellMetricsOverride)
    )(page => renderStartPageFrame(state0, page, surface, viewportSize, uiFont, cellMetrics, uiMetrics, output)) {
      scene =>
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
          output,
          damage
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
    // #1105/#1215: see the surface-generic renderCursorOnly above for why this is scoped to a surface with no real
    // FontRenderContext.
    val cellMetricsOverride = Option.when(surface.text.fontRenderContext.isEmpty)(cellMetrics)
    withSceneIfNeeded(
      state0,
      AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont, cellMetrics = cellMetricsOverride)
    )(_ => Nil) { scene =>
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
    viewportSize: ViewportSize
  ): Unit =
    render(state, cursorVisible, surface, viewportSize, None, Damage.Everything)

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    cursorColor: Option[java.awt.Color]
  ): Unit =
    render(state, cursorVisible, surface, viewportSize, cursorColor, Damage.Everything)

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    cursorColor: Option[java.awt.Color],
    damage: Damage
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
      cursorColor,
      damage
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
    output: Option[FrameOutput],
    damage: Damage,
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState] = Map.empty
  ): Option[EditorPaneRenderPlan] =
    surface.hideCursor()

    val editorRenderPlan = state.startPageSurface.flatMap {
      _.content match
        case SurfaceContent.StartPage(page) => Some(page)
        case _                              => None
    } match
      case Some(page) =>
        surface.clearViewport(state.persisted.theme.background)
        forgetPreservedContent(surface, output)
        renderStartPage(page, surface, viewportSize, state.persisted.theme, uiFont, cellMetrics, uiMetrics)
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
        renderFloatingPanels(state, floatContext, scene, Damage.Everything)
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
          uiMetrics,
          bufferAnimations
        )
        val framePlan = planFrame(state, context, editorRenderPlan, viewportSize, output, damage)
        framePlan match
          case Some(plan) if plan.preserved.nonEmpty =>
            surface.clearViewportExcept(state.persisted.theme.background, plan.preserved)
          case _ =>
            surface.clearViewport(state.persisted.theme.background)
        paintFrameLayers(state, context, editorRenderPlan, finalizedScene, framePlan, damage)
        commitFramePlan(framePlan, output)
        Some(editorRenderPlan)

    surface.effects.foreach(_.applyPostProcessing(state.persisted.config.surfaceConfig.postProcessingEffect))
    surface.flush()
    editorRenderPlan

  private val ChromeLayerId         = LayerId("chrome")
  private val EditorContentLayerId  = LayerId("editor-content")
  private val PinnedPanelsLayerId   = LayerId("pinned-panels")
  private val FloatingPanelsLayerId = LayerId("floating-panels")
  private val ModalLayerId          = LayerId("modal")

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
    damage: Damage
  ): Unit =
    val modalSurfaceId = currentModalSurfaceId(state)
    val modalDamage    = modalSurfaceId.fold(Damage.Nothing: Damage)(id => Damage.narrowToSurface(damage, id))
    val layers = List(
      Layer(ChromeLayerId, zOrder = 0, LayerEffect.identity, damage),
      Layer(EditorContentLayerId, zOrder = 1, LayerEffect.identity, damage),
      Layer(PinnedPanelsLayerId, zOrder = 2, LayerEffect.identity, damage),
      Layer(FloatingPanelsLayerId, zOrder = 3, LayerEffect.identity, damage),
      Layer(ModalLayerId, zOrder = 4, LayerEffect.identity, modalDamage)
    )
    val dirtyLayerIds = LayerCompositor.dirtyLayers(layers).map(_.id).toSet
    forgetStalePanelLayerBuffers(state, scene, context.surface)
    val paintByLayer: Map[LayerId, () => Unit] = Map(
      ChromeLayerId -> { () =>
        renderSpacerColumns(state, context, editorRenderPlan.layoutContract)
        renderLineNumbers(state, context, editorRenderPlan)
        renderGutter(state, context, editorRenderPlan.layoutContract)
      },
      EditorContentLayerId -> { () =>
        renderEditorPanes(
          state,
          context,
          editorRenderPlan,
          framePlan.map(_.dirtyRowsByPane).getOrElse(Map.empty)
        )
      },
      PinnedPanelsLayerId   -> { () => renderPinnedPanels(state, context, scene, damage) },
      FloatingPanelsLayerId -> { () => renderFloatingPanels(state, context, scene, damage) },
      ModalLayerId          -> { () => paintModalLayer(state, context, scene, dirtyLayerIds.contains(ModalLayerId)) }
    )
    LayerCompositor.orderedForComposite(layers).foreach(layer => paintByLayer(layer.id)())

  /** The `SurfaceId` of the surface currently presented as [[SurfacePresentation.Modal]], if any -- at most one is ever
    * open at a time. `None` means the modal layer has nothing to paint this frame, matching `renderModalLayer`'s own
    * `scene.modalBackdrop.foreach` no-op.
    */
  private def currentModalSurfaceId(state: AppState): Option[SurfaceId] =
    state.runtime.uiSurfaces.collectFirst { case UiSurface(id, _, SurfacePresentation.Modal, _) => id }

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
    * precondition (`renderModalLayer` never reads pixels back off the surface it paints onto) that keeps this safe for
    * the modal specifically while pinned/expanded panels (which do, via `blurRegion`) aren't yet covered.
    */
  private def paintModalLayer(
    state: AppState,
    context: RenderContext,
    scene: UiSceneSnapshot,
    isDirty: Boolean
  ): Unit =
    context.surface.layerBuffers match
      case None => renderModalLayer(state, context, scene)
      case Some(support) =>
        scene.modalBackdrop match
          case None =>
            modalLayerBuffers.synchronized { val _ = modalLayerBuffers.remove(context.surface) }
          case Some(_) =>
            val cached = modalLayerBuffers.synchronized(Option(modalLayerBuffers.get(context.surface)))
            val reusable = !isDirty && cached.exists { c =>
              c.viewportWidth == context.surface.viewportWidth &&
              c.viewportHeight == context.surface.viewportHeight &&
              c.cursorVisible == context.cursorVisible
            }
            if reusable then
              cached.foreach(c => context.surface.pixels.drawImage(c.image, 0, 0, c.viewportWidth, c.viewportHeight))
            else
              val capturedRef  = new AtomicReference[Option[BufferedImage]](None)
              val layerSurface = support.newLayerSurface(image => capturedRef.set(Some(image)))
              renderModalLayer(state, context.copy(surface = layerSurface), scene)
              layerSurface.flush()
              capturedRef.get().foreach { image =>
                val newlyCached = CachedModalLayer(
                  image,
                  context.surface.viewportWidth,
                  context.surface.viewportHeight,
                  context.cursorVisible
                )
                modalLayerBuffers.synchronized {
                  val _ = modalLayerBuffers.put(context.surface, newlyCached)
                }
                context.surface.pixels.drawImage(
                  image,
                  0,
                  0,
                  context.surface.viewportWidth,
                  context.surface.viewportHeight
                )
              }

  /** Whether a pinned/expanded/floating panel identified by `surfaceId` must repaint this frame rather than reuse its
    * cached buffer -- the panel generalisation of [[paintModalLayer]]'s `isDirty` flag (#1100 stage 3).
    *
    * `Damage.narrowToSurface(damage, surfaceId)` alone is exactly what makes the modal's own caching safe: it is
    * `Damage.Nothing` whenever this frame's damage doesn't name this surface, even if it names something else entirely
    * (editor content elsewhere, chrome, another surface). That is safe for the modal because `renderModalLayer` never
    * reads pixels back off the frame it paints onto -- but a panel that blurs its own background
    * (`SurfaceMaterials.effectiveBlurRadius > 0`) *does* read pixels back, via `blurRegion`, and those pixels can be
    * exactly the ones some *other* damage (an edit under a translucent panel) just changed. Reusing a cached buffer in
    * that case would composite a blur of stale content back onto a frame whose real content has since moved on --
    * silently wrong output, not just a missed optimisation. So when blur is active, a panel is dirty unless the whole
    * frame's damage is `Damage.Nothing` -- truly nothing changed anywhere, which is the one case a stale blur is
    * provably still correct. When blur is inactive (`blurRadius <= 0f`), no panel reads pixels back, and the narrower,
    * per-surface check applies exactly as it does for the modal: a panel only redraws when its own `Damage.Surface`
    * entry says so.
    */
  private def panelDirtyCheck(damage: Damage, blurRadius: Float)(surfaceId: SurfaceId): Boolean =
    Damage.narrowToSurface(damage, surfaceId) != Damage.Nothing || (blurRadius > 0f && damage != Damage.Nothing)

  /** Paint one pinned/expanded/floating panel's own content into an isolated layer buffer, reusing its last-painted
    * pixels instead of repainting when [[panelDirtyCheck]] says it's safe to -- the panel generalisation of
    * [[paintModalLayer]] (#1100 stage 3).
    *
    * Unlike the modal, a panel's own paint step (`paintPanel`) can read pixels back off the surface it paints onto
    * (`SurfaceMaterials.effectiveBlurRadius`'s `blurRegion` call), so this seeds the layer buffer from a snapshot of
    * this frame's live pixels (`LayerBufferSupport.newSeededLayerSurface`) instead of starting fully transparent --
    * `blurRegion` then reads exactly the same background it would reading the live frame surface directly, because the
    * snapshot *is* that surface's pixels at the moment it was taken. Painting the rest of the panel into the same
    * seeded buffer and compositing the whole buffer back at full opacity is then pixel-identical to painting directly,
    * the same "paint onto a copy, composite back" argument [[Java2DRenderSurface.newLayerSurface]]'s doc comment makes
    * for the transparent case.
    *
    * No `layerBuffers` capability (TUI's `TerminalRenderSurface`) -> falls straight through to `paintPanel`, painting
    * directly into the shared surface every frame exactly as before this stage -- the same TUI exclusion #1100 stage 2
    * documented for the modal.
    */
  private def paintPanelLayer(
    context: RenderContext,
    surfaceId: SurfaceId,
    frameRect: LayoutRect,
    isDirty: Boolean
  )(paintPanel: RenderContext => Unit): Unit =
    context.surface.layerBuffers match
      case None => paintPanel(context)
      case Some(support) =>
        val cached =
          panelLayerBuffers.synchronized(Option(panelLayerBuffers.get(context.surface))).flatMap(_.get(surfaceId))
        val reusable = !isDirty && cached.exists { c =>
          c.viewportWidth == context.surface.viewportWidth &&
          c.viewportHeight == context.surface.viewportHeight &&
          c.cursorVisible == context.cursorVisible &&
          c.frameRect == frameRect
        }
        if reusable then
          cached.foreach(c => context.surface.pixels.drawImage(c.image, 0, 0, c.viewportWidth, c.viewportHeight))
        else
          val capturedRef  = new AtomicReference[Option[BufferedImage]](None)
          val layerSurface = support.newSeededLayerSurface(image => capturedRef.set(Some(image)))
          paintPanel(context.copy(surface = layerSurface))
          layerSurface.flush()
          capturedRef.get().foreach { image =>
            val newlyCached = CachedPanelLayer(
              image,
              context.surface.viewportWidth,
              context.surface.viewportHeight,
              context.cursorVisible,
              frameRect
            )
            panelLayerBuffers.synchronized {
              val current =
                Option(panelLayerBuffers.get(context.surface)).getOrElse(Map.empty[SurfaceId, CachedPanelLayer])
              val _ = panelLayerBuffers.put(context.surface, current.updated(surfaceId, newlyCached))
            }
            context.surface.pixels.drawImage(image, 0, 0, context.surface.viewportWidth, context.surface.viewportHeight)
          }

  /** Drop cached panel buffers for surfaces no longer on screen this frame, scoped to `surface`'s own entry in
    * [[panelLayerBuffers]] -- a dismissed panel's cache would otherwise sit in that inner `Map[SurfaceId, _]` forever
    * (a `WeakHashMap` reclaims the outer, per-surface entry once a `RenderSurface` is no longer referenced, but
    * [[SurfaceId]] is a plain value, not a pixel buffer whose lifetime this module can track that way).
    */
  private def forgetStalePanelLayerBuffers(state: AppState, scene: UiSceneSnapshot, surface: RenderSurface): Unit =
    val pinnedAndExpandedIds = pinnedAndExpandedSurfaces(state).map(_.id).toSet
    val overlays             = OverlayViewModel.fromState(state, scene)
    val floatingIds =
      (overlays.aboveCursor.toList ++ overlays.belowCursorStack).flatMap(_.surfaceId).toSet
    val activeIds = pinnedAndExpandedIds ++ floatingIds
    panelLayerBuffers.synchronized {
      val current = Option(panelLayerBuffers.get(surface)).getOrElse(Map.empty[SurfaceId, CachedPanelLayer])
      val _       = panelLayerBuffers.put(surface, current.filter { case (id, _) => activeIds.contains(id) })
    }

  /** Drop every reuse promise attached to this surface and force the next repaint to cover the whole canvas.
    *
    * Used by frames that repaint the canvas from scratch (the start page) or that stand `planFrame`'s optimisation down
    * entirely, where no pane row survives -- the accumulated damage this identity was tracking is now moot, since
    * everything just got redrawn from nothing this surface's own bookkeeping remembers.
    */
  private def forgetPreservedContent(surface: RenderSurface, output: Option[FrameOutput]): Unit =
    surface.persistentContentKey.foreach { key =>
      bufferDamage.synchronized {
        val _ = bufferDamage.remove(key)
        val _ = bufferScreen.remove(key)
      }
      bufferDrawState.synchronized { val _ = bufferDrawState.remove(key) }
    }
    output.foreach { value =>
      screenDamage.synchronized { val _ = screenDamage.remove(value.screenToken) }
      screenPaneIds.synchronized { val _ = screenPaneIds.remove(value.screenToken) }
      value.repaintRegion.set(None)
    }

  private def commitFramePlan(framePlan: Option[FramePlan], output: Option[FrameOutput]): Unit =
    output.foreach { value =>
      val bounded = framePlan.filter(_.boundedRepaint).map(_.repaintRegion.getOrElse(PixelRect(0, 0, 0, 0)))
      value.repaintRegion.set(bounded)
    }

  /** Decide which pane rows this frame still has to draw, and which pixel bands it may keep from an earlier frame.
    *
    * Returns `None` — meaning "draw everything, remember nothing" — whenever the frame cannot be reasoned about safely:
    * a surface that does not preserve pixels, or a post-processing pass that would compound over kept pixels. A
    * floating/pinned/modal/expanded layer being visible no longer stands the whole optimisation down by itself
    * (`#1000`, retiring the old `overlaysMayCoverPanes` check) -- `DamageProducer.fullRenderDamage` reports
    * `Everything` whenever `uiSurfaces`/`focus` actually change, which still wipes out any stale
    * shadow/blur/translucency bleed the instant an overlay appears, moves, resizes or changes content, while leaving
    * row reuse active on every other frame an overlay merely sits on screen. `damage` is always folded into every
    * tracked identity first, regardless of which branch this frame takes, so a pixel buffer that sits idle through a
    * stood-down frame does not lose the damage that frame reported.
    */
  private def planFrame(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    viewportSize: ViewportSize,
    output: Option[FrameOutput],
    damage: Damage
  ): Option[FramePlan] =
    accumulateBufferDamage(output, damage)
    accumulateScreenDamage(output, damage)

    val plan = context.surface.persistentContentKey
      .filter(_ => state.persisted.config.surfaceConfig.postProcessingEffect == PostProcessingEffect.Off)
      .map { persistenceKey =>
        val panes   = paneRecordsFor(state, context, renderPlan)
        val paneIds = panes.keySet
        val allPanesReusable =
          renderPlan.paneLayouts.keySet.forall(panes.contains) &&
            state.persisted.layout.orderedPaneIds.forall(panes.contains)

        val bufferDamageSinceLastDraw = drainBufferDamage(output, persistenceKey)
        val inputsOrPanesChanged = drawStateChanged(persistenceKey, paneIds, renderInputsFor(context, viewportSize))
        val effectiveDamage      = if inputsOrPanesChanged then Damage.Everything else bufferDamageSinceLastDraw

        val dirtyRowsByPane = panes.map { case (paneId, record) => paneId -> dirtyRowsFor(effectiveDamage, record) }

        val preserved = panes.toList.flatMap {
          case (paneId, record) =>
            val dirty = dirtyRowsByPane.getOrElse(paneId, record.rowRects.indices.toSet)
            record.rowRects.indices.filterNot(dirty.contains).map(record.rowRects)
        }

        // The screen shows the previous frame, while the pixels this surface preserves come from the one before that,
        // so the repaint region is measured against damage accumulated since the screen was last published to, and
        // only when every pane is reusable and the published pane set hasn't shifted underneath it.
        val screenDamageSincePublish = drainScreenDamage(output)
        val boundedRepaintEligible =
          allPanesReusable &&
            !screenPaneIdsChanged(output, paneIds) &&
            Damage.isBufferRowsOnly(screenDamageSincePublish)
        val repaintRows =
          Option.when(boundedRepaintEligible) {
            panes.toList.flatMap {
              case (_, record) =>
                dirtyRowsFor(screenDamageSincePublish, record).toList.flatMap(record.rowRects.lift)
            }
          }

        FramePlan(
          dirtyRowsByPane = dirtyRowsByPane,
          preserved = preserved,
          repaintRegion = repaintRows.flatMap(PixelRect.unionOf),
          boundedRepaint = repaintRows.isDefined
        )
      }

    if plan.isEmpty then forgetPreservedContent(context.surface, output)
    plan

  /** Rows of `record` that `damage` marks dirty, translated from buffer line numbers to this pane's current visual row
    * indices, widened by one row on each side and by the rows whose glyphs reach outside the band this pane can
    * preserve. `Damage.Everything` dirties every row, since it carries no per-buffer detail to translate.
    */
  private def dirtyRowsFor(damage: Damage, record: PaneFrameRecord): Set[Int] =
    if Damage.isEverything(damage) then record.rowBufferLines.indices.toSet
    else
      val bufferLines = Damage.coarsenToRows(record.bufferId, damage)
      val dirty = record.rowBufferLines.zipWithIndex.collect {
        case (bufferLine, row) if bufferLines.contains(bufferLine) => row
      }.toSet
      DirtyLineDiff.dilate(dirty, record.rowBufferLines.length) ++ record.overflowingRows

  /** Geometry for the panes drawn by [[renderPlainBufferContent]]: which buffer line each visual row shows (to
    * translate `Damage`'s buffer-line facts into row indices) and the pixel band each row owns (to know what a
    * preserved row's pixels actually cover).
    *
    * Panes on any other path — the welcome text, the empty-pane filler, the inline markdown lens, which paint over the
    * whole content rect from inputs this record does not track — are left out, so they always redraw in full.
    */
  private def paneRecordsFor(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan
  ): Map[PaneId, PaneFrameRecord] =
    state.persisted.layout.editorPanes.flatMap {
      case (paneId, pane) =>
        for
          paneLayout <- renderPlan.paneLayouts.get(paneId)
          bufferId   <- pane.bufferId
          buffer     <- state.persisted.buffers.get(bufferId)
          snapshot   <- renderPlan.snapshots.get(paneId)
          if buffer.document.content.weight > 0 && !isInlineMarkdownLens(buffer, state)
        yield paneId -> paneFrameRecord(bufferId, paneLayout.contentRect, context, snapshot)
    }.toMap

  private def paneFrameRecord(
    bufferId: BufferId,
    contentRect: LayoutRect,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): PaneFrameRecord =
    PaneFrameRecord(
      bufferId = bufferId,
      rowBufferLines = snapshot.visualLines.map(_.bufferLine),
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
      if usesMeasuredDrawing(snapshot, context) then snapshot.lineHeightPx else context.cellMetrics.lineHeight
    val topLimitPx    = context.cellMetrics.toPixelY(contentRect.y)
    val bottomLimitPx = rowMetrics.contentBottomPx
    snapshot.visualLines.indices.toVector.map { row =>
      val topPx    = rowMetrics.lineTopPx(row).max(topLimitPx)
      val bottomPx = (rowMetrics.lineTopPx(row) + heightPx).min(bottomLimitPx)
      PixelRect(leftPx, topPx, widthPx.max(0), (bottomPx - topPx).max(0))
    }

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
    surface.setBackgroundColor(state.persisted.theme.margin)
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
      state.persisted.layout.editorPanes.flatMap {
        case (paneId, pane) =>
          for
            paneLayout <- workspaceLayout.paneLayouts.get(paneId)
            bufferId   <- pane.bufferId
            buffer     <- state.persisted.buffers.get(bufferId)
          yield paneId -> scene
            .textSnapshot(paneId)
            .getOrElse(
              snapshotForBuffer(buffer, paneLayout.contentRect, state, context)
            )
      }

    val visibleLinesByBuffer = state.persisted.layout.editorPanes.toList
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

    val annotations = state.persisted.layout.editorPanes.values
      .flatMap(_.bufferId)
      .toList
      .distinct
      .flatMap { bufferId =>
        state.persisted.buffers.get(bufferId).map { _ =>
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

  /** Whether buffer text should actually be painted through the measured pixel-run path.
    *
    * `snapshot.usesMeasuredLayout` is a font-only judgement (proportional advances, ligatures, ...) computed once when
    * the pane's [[TextLayoutSnapshot]] is built -- including by [[com.serenity.state.manager.AuthoritativeUiScene]],
    * which has no surface to ask and always uses it as a scene-wide, surface-agnostic cache shared with mouse
    * targeting. `drawRunPx` is a no-op on a surface reporting no `FontRenderContext` (a terminal), so painting must
    * additionally require the surface's own capability -- #1105.
    */
  private def usesMeasuredDrawing(snapshot: TextLayoutSnapshot, context: RenderContext): Boolean =
    snapshot.usesMeasuredLayout && context.surface.text.fontRenderContext.nonEmpty

  private def snapshotForBuffer(
    buffer: Buffer,
    contentRect: LayoutRect,
    state: AppState,
    context: RenderContext
  ): TextLayoutSnapshot =
    // #1105: a surface reporting no FontRenderContext (a terminal) can't draw a measured pixel run at all -- drawRunPx
    // is a no-op there -- so a buffer that would otherwise pick a proportional text/ui font is forced onto the code
    // font instead, and the resulting snapshot is forced onto the cell path below, regardless of what the font itself
    // would have decided (ligatures, non-monospace advances, ...).
    val hasFontRenderContext = context.surface.text.fontRenderContext.nonEmpty
    val bufferFont           = if hasFontRenderContext then context.fontForBuffer(buffer) else context.codeFont
    val panelWidthPx         = contentRect.width * context.cellMetrics.charWidth
    val panelHeightPx        = contentRect.height * context.cellMetrics.lineHeight
    val bufferMetrics        = CellMetrics.fromFont(bufferFont)
    val baseViewport =
      LayoutEngine.updateBufferViewportDimensions(
        buffer,
        contentRect,
        state.persisted.config.surfaceConfig.wordWrapEnabled
      )
    val fontRenderContext =
      context.surface.text.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
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
      else renderedLeftColumn(buffer, scrollViewport, state.persisted.config.surfaceConfig.wordWrapEnabled)
    val renderedViewport = sizedViewport.copy(
      leftColumn = leftColumn
    )
    val renderBuffer = buffer.copy(
      viewport = renderedViewport
    )
    context.surface.text.setFont(bufferFont)
    // A surface with a real FontRenderContext keeps deriving cell-based advances from the buffer's own font, same as
    // always. A surface with none (a terminal, #1105) has no real font metrics to derive from at all -- its own
    // declared `context.cellMetrics` (TUI's CellMetricsOne, 1 pixel == 1 terminal cell) is the only legitimate scale,
    // so this is what must reach the cell-based layout path instead of `TextLayoutSnapshot` re-deriving
    // `CellMetrics.fromFont(bufferFont)` (a real AWT measurement of a font this surface never actually draws) (#1215).
    val snapshot = TextLayoutSnapshot.fromBuffer(
      renderBuffer,
      panelWidthPx,
      bufferFont,
      fontRenderContext,
      wordWrapEnabled = state.persisted.config.surfaceConfig.wordWrapEnabled,
      cellMetricsOverride = if hasFontRenderContext then Some(bufferMetrics) else Some(context.cellMetrics),
      // #1215: forces the cell path during construction (not just the flag below) -- otherwise a "monospaced" font
      // whose measured-vs-cell auto-detection trips on this environment's own font-rendering quirks would still bake
      // real (and here, meaningless -- #1105) measured advances into the snapshot, discarding `context.cellMetrics`.
      forceCellLayout = !hasFontRenderContext
    )
    if hasFontRenderContext then snapshot else snapshot.copy(usesMeasuredLayout = false)

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
      val cursor         = buffer.editing.cursors.headOption.getOrElse(CursorPosition(viewport.topLine, 0))
      val cursorColumn   = cursor.column.max(0)
      val lineLength     = buffer.document.content.getLine(cursor.line).map(_.length).getOrElse(cursorColumn)
      val maxForCursor   = math.max(0, cursorColumn - visibleColumns + 1)
      val maxForLine     = math.max(0, lineLength - visibleColumns)

      viewport.leftColumn.max(0).min(maxForCursor).min(maxForLine)

  private def renderEditorPanes(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    dirtyRowsByPane: Map[PaneId, Set[Int]]
  ): Unit =
    val activePaneId = state.persisted.layout.activeEditorPaneId
    val orderedPanes =
      state.persisted.layout.orderedPaneIds
        .flatMap(paneId => state.persisted.layout.editorPanes.get(paneId).map(paneId -> _))
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
    val activePaneId = state.persisted.layout.activeEditorPaneId
    val orderedPanes =
      activePaneId.toList.flatMap(id => state.persisted.layout.editorPanes.get(id).map(id -> _)) ++
        state.persisted.layout.editorPanes.toList.filterNot((id, _) => activePaneId.contains(id)).sortBy(_._1.value)

    orderedPanes.flatMap {
      case (paneId, pane) =>
        (for
          paneLayout <- renderPlan.paneLayouts.get(paneId)
          bufferId   <- pane.bufferId
          buffer     <- state.persisted.buffers.get(bufferId)
          snapshot   <- renderPlan.snapshots.get(paneId)
        yield renderCursors(
          buffer,
          paneLayout.contentRect,
          state.persisted.theme,
          state.persisted.config,
          context,
          snapshot
        ))
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
    val buffer = pane.bufferId.flatMap(state.persisted.buffers.get)

    renderBufferHeader(pane, buffer, paneLayout, state, context, contract)
    renderEditorPaneVerticalSpacers(paneLayout, state, context)

    val contentRect = paneLayout.contentRect

    val bufferSnapshot = preparedSnapshot.orElse(buffer.map(snapshotForBuffer(_, contentRect, state, context)))
    val markdownLensFrame =
      for
        buf  <- buffer
        snap <- bufferSnapshot
        if isInlineMarkdownLens(buf, state)
      yield markdownLensFrameFor(buf, snap)

    buffer match
      case Some(buf) if buf.document.content.weight == 0 && buf.document.isNewEmpty =>
        renderWelcomeText(contentRect, state.persisted.theme, context)
      case Some(buf) if buf.document.content.weight == 0 =>
        renderEmptyPane(contentRect, state.persisted.theme, context)
      case Some(buf) =>
        bufferSnapshot.fold(renderEmptyPane(contentRect, state.persisted.theme, context)) { snap =>
          renderBufferContent(
            buf,
            contentRect,
            state,
            context,
            snap,
            markdownLensFrame,
            annotations.getOrElse(BufferRenderAnnotations(Map.empty, Map.empty)),
            dirtyRows
          )
        }
      case None =>
        renderEmptyPane(contentRect, state.persisted.theme, context)

    val cursorContext =
      if state.hasCommandRunnerDomain then context.copy(cursorVisible = true)
      else context
    (buffer, bufferSnapshot) match
      case (Some(buf), Some(snap)) =>
        if isInlineMarkdownLens(buf, state) then
          renderMarkdownLensCursors(
            buf,
            contentRect,
            state.persisted.theme,
            state.persisted.config,
            cursorContext,
            snap,
            markdownLensFrame.getOrElse(markdownLensFrameFor(buf, snap))
          )
        else
          val _ = renderCursors(
            buf,
            contentRect,
            state.persisted.theme,
            state.persisted.config,
            cursorContext,
            snap
          )
      case _ => ()

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
      context.surface.text.setFont(context.uiFont)
      val isActive  = state.persisted.layout.activeEditorPaneId.contains(pane.id)
      val titleRect = contract.paneTitleRect(pane.id).getOrElse(paneLayout.titleRect)

      if isActive then
        surface.setBackgroundColor(state.persisted.theme.highlighted.background)
        surface.setForegroundColor(state.persisted.theme.highlighted.foreground)
      else
        surface.setBackgroundColor(state.persisted.theme.panel.background)
        surface.setForegroundColor(state.persisted.theme.panel.foreground)

      val bufferTitle = buffer match
        case Some(buf) =>
          buf.document.filePath match
            case Some(path) =>
              val filename = path.getFileName.toString
              if buf.document.isDirty then s"$filename - unsaved" else filename
            case None =>
              if buf.document.isDirty then s"Buffer ${buf.id.value} - unsaved" else s"Buffer ${buf.id.value}"
        case None =>
          "No Buffer"

      val maxTitleWidth = math.max(1, titleRect.width - 2)
      val displayTitle =
        if bufferTitle.length > maxTitleWidth then bufferTitle.take(maxTitleWidth - 3) + "..."
        else bufferTitle

      surface.putString(headerRect.x, headerRect.y, " " * headerRect.width)

      surface.text.fontRenderContext match
        case Some(frc) =>
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
            frc
          )
          surface.text.drawRunPx(
            titlePlacement.xPx,
            titlePlacement.yPx,
            titlePlacement.widthPx,
            titlePlacement.lineHeightPx,
            titlePlacement.ascentPx,
            displayTitle
          )
        case None =>
          val centerX = titleRect.x + math.max(0, (titleRect.width - displayTitle.length) / 2)
          CharacterRenderer.renderString(surface, centerX, titleRect.y, displayTitle)

    surface.setBackgroundColor(state.persisted.theme.background)
    surface.setForegroundColor(state.persisted.theme.foreground)

  private def renderEditorPaneVerticalSpacers(
    paneLayout: EditorPaneLayout,
    state: AppState,
    context: RenderContext
  ): Unit =
    context.surface.setBackgroundColor(state.persisted.theme.margin)
    List(paneLayout.topSpacerRect, paneLayout.bottomSpacerRect)
      .filter(rect => rect.width > 0 && rect.height > 0)
      .foreach(rect => context.surface.fillRect(rect.x, rect.y, rect.width, rect.height, ' '))
    context.surface.setBackgroundColor(state.persisted.theme.background)
    context.surface.setForegroundColor(state.persisted.theme.foreground)

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
    context.surface.text.setFont(context.fontForBuffer(buffer))
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
    val lexStartStates  = bufferLexStartStates(buffer)

    visualLines.zipWithIndex.foreach {
      case (visualLine, screenLineIndex) =>
        if dirtyRows.forall(_.contains(screenLineIndex)) &&
            visualLineFits(rect, screenLineIndex, context, snapshot)
        then
          val screenY   = rect.y + screenLineIndex
          val lineTopPx = visualLineTopPx(rect, screenLineIndex, context, snapshot)
          val screenX   = rect.x + visualLineCellOffset(visualLine, context)

          context.surface.setForegroundColor(state.persisted.theme.foreground)

          if visualLineVisible(rect, screenLineIndex, context, snapshot) &&
              screenY >= 0 &&
              screenX < context.surface.viewportWidth &&
              screenX >= 0 &&
              screenX < rect.right
          then
            val lineTheme      = state.persisted.theme
            val styledSegments = visualLineStyledSegments(visualLine, lineTheme, snapshot, activeBodyLines)
            val lexStartState =
              lexStartStates.lift(visualLine.bufferLine).getOrElse(com.serenity.ui.theme.LexState.Default)
            if usesMeasuredDrawing(snapshot, context) then
              CharacterRenderer.renderMeasuredLineWithAnimation(
                context.surface,
                xOriginPx,
                lineTopPx,
                snapshot.lineHeightPx,
                snapshot.ascentPx,
                visualLine,
                lineTheme,
                context.bufferAnimations.getOrElse(buffer.id, com.serenity.animation.AnimationState.empty),
                state.syntaxHighlightingEnabled,
                buffer.document.language,
                styledSegments,
                clipRightXPx = Some(contentRightXPx),
                lexStartState = lexStartState
              )
            else
              CharacterRenderer.renderStringWithAnimation(
                context.surface,
                screenX,
                screenY,
                visualLine.text,
                lineTheme,
                context.bufferAnimations.getOrElse(buffer.id, com.serenity.animation.AnimationState.empty),
                state.syntaxHighlightingEnabled,
                buffer.document.language,
                bufferLine = visualLine.bufferLine,
                bufferStartColumn = visualLine.startColumn,
                styledSegments = styledSegments,
                lexStartState = lexStartState,
                maxColumn = Some(rect.right)
              )

            renderDocumentCommentHighlights(
              context.surface,
              annotations.commentsByLine.getOrElse(visualLine.bufferLine, Nil),
              visualLine,
              rect,
              screenY,
              lineTopPx,
              state.persisted.theme,
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
              state.persisted.theme,
              context,
              snapshot
            )

            val stringEnd = visualLine.startColumn + visualLine.text.length
            val lineAnims = context.bufferAnimations
              .getOrElse(buffer.id, com.serenity.animation.AnimationState.empty)
              .getLineAnimations(visualLine.bufferLine)
            lineAnims.foreach { (col, cell) =>
              cell.currentBackground.foreach { bg =>
                if col >= stringEnd then
                  val bgScreenX = rect.x + visualLineCellOffset(visualLine, context) + (col - visualLine.startColumn)
                  if bgScreenX >= 0 && bgScreenX < rect.right then
                    context.surface.setForegroundColor(state.persisted.theme.foreground)
                    context.surface.setBackgroundColor(bg)
                    context.surface.putString(bgScreenX, screenY, " ")
              }
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
      usesMeasuredLayout = usesMeasuredDrawing(snapshot, context)
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

  /** Lexical state at the start of each buffer line, needed so an open block comment or triple-quoted string keeps
    * coloring correctly on the lines after it opened. Cheap for languages without token-aware highlighting (a single
    * `Default` per line, no document scan); for Scala it's recomputed incrementally by [[ThemeManager]], keyed by
    * buffer id, so an edit only re-scans from the changed line forward.
    */
  private def bufferLexStartStates(buffer: Buffer): Vector[com.serenity.ui.theme.LexState] =
    if !buffer.document.language.contains(LanguageId.Scala) then Vector.empty
    else
      val rope = buffer.document.content
      ThemeManager.lineStartStates(
        buffer.id.value.toString,
        rope.linesFrom(0, rope.lineCount),
        buffer.document.language
      )

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
    if !state.persisted.config.surfaceConfig.focusedTextBodyEnabled then _ => true
    else
      val activeLine = buffer.editing.cursors.headOption.map(_.line)
      FocusedTextBody
        .activeRange(buffer, activeLine)
        .map((range: Range.Inclusive) => (line: Int) => range.contains(line))
        .getOrElse((_: Int) => true)

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
    val title = buffer.document.filePath.flatMap(path => Option(path.getFileName).map(_.toString)).getOrElse("Untitled")
    val image = MarkdownDocumentPreview.renderInlineRowsImage(
      rows = frame.previewRows,
      sourceLines = frame.lines,
      title = title,
      widthPx = widthPx,
      heightPx = heightPx,
      theme = state.persisted.theme,
      font = previewFont,
      inlineLineHeightPx = MarkdownDocumentPreview.lineHeightForDeviceScale(
        context.cellMetrics.lineHeight,
        context.surface.devicePixelScaleY
      ),
      reuseLastRenderWhileEditing = buffer.markdownPreviewEditGeneration != buffer.markdownPreviewCommittedGeneration
    )
    context.surface.pixels.drawImage(image, rect.x, rect.y, rect.width, rect.height)

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
          if usesMeasuredDrawing(snapshot, context) then
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
                surface.text.drawRunPx(
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
    if usesMeasuredDrawing(snapshot, context) then
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
          surface.text.drawRunPx(
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
    buffer.document.language.contains(
      LanguageId.Markdown
    ) && state.persisted.config.markdownViewMode == MarkdownViewMode.InlineLens

  private def markdownLensFrameFor(buffer: Buffer, snapshot: TextLayoutSnapshot): MarkdownLensFrame =
    val previewWindow = markdownPreviewWindow(buffer, buffer.viewport.visibleLines)
    val lines = buffer.document.content.linesFrom(
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
      buffer.editing.cursors.map(_.line - previewWindow.window.firstSourceLine).toSet,
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
    val lineCount = buffer.document.content.lineCount
    if lineCount == 0 then MarkdownLensPreviewWindow(MarkdownDocumentPreview.PreviewWindow(0, 0, ""), 0)
    else
      val activeLine = buffer.editing.cursors.headOption
        .map(_.line)
        .filter(line => line >= 0 && line < lineCount)
      val activeBlock     = activeLine.map(line => FocusedTextBody.markdownBlock(buffer, line))
      val viewportTopLine = buffer.viewport.topLine.max(0).min(lineCount - 1)
      val windowTopLine = activeLine
        .filter(line =>
          line == viewportTopLine && line > 0 && buffer.document.content.getLine(line).exists(_.trim.isEmpty)
        )
        .filter(line => buffer.document.content.getLine(line - 1).exists(_.trim.matches("^#{1,6}\\s+.*")))
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
              buffer.document.content.linesFrom(firstSourceLine, math.min(maxSourceLines, lineCount - firstSourceLine)),
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
    val lineCount = buffer.document.content.lineCount
    val cursorRanges = buffer.editing.cursors
      .map(_.line)
      .filter(line => line >= 0 && line < lineCount)
      .map(line => FocusedTextBody.markdownBlock(buffer, line))
    val selectionRanges = buffer.allSelections.flatMap { selection =>
      if lineCount == 0 then Nil
      else
        val startLine = selection.start.line.max(0).min(lineCount - 1)
        val endLine   = selection.end.line.max(0).min(lineCount - 1)
        val selectedLines =
          if endLine - startLine <= MarkdownSelectionProbeLimit then startLine to endLine
          else List(startLine, endLine)
        selectedLines.map(line => FocusedTextBody.markdownBlock(buffer, line)).distinct
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
        context.surface.setBackgroundColor(state.persisted.theme.panel.background)
        context.surface.fillRect(rect.x, lensY, rect.width, placement.height, ' ')
        blockVisualLines.zipWithIndex.foreach {
          case (visualLine, index) =>
            val screenY = lensY + index
            if screenY >= rect.y && screenY < rect.bottom && screenY >= 0 && screenY < context.surface.viewportHeight
            then
              context.surface.setForegroundColor(state.persisted.theme.foreground)
              context.surface.setBackgroundColor(state.persisted.theme.panel.background)
              if usesMeasuredDrawing(snapshot, context) then
                CharacterRenderer.renderMeasuredLineWithAnimation(
                  context.surface,
                  context.cellMetrics.toPixelX(rect.x).toFloat,
                  context.cellMetrics.toPixelY(screenY),
                  snapshot.lineHeightPx,
                  snapshot.ascentPx,
                  visualLine,
                  state.persisted.theme.copy(background = state.persisted.theme.panel.background),
                  context.bufferAnimations.getOrElse(buffer.id, com.serenity.animation.AnimationState.empty),
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
                  state.persisted.theme.copy(background = state.persisted.theme.panel.background),
                  context.bufferAnimations.getOrElse(buffer.id, com.serenity.animation.AnimationState.empty),
                  syntaxHighlightingEnabled = false,
                  language = None,
                  bufferLine = visualLine.bufferLine,
                  bufferStartColumn = visualLine.startColumn,
                  maxColumn = Some(rect.right)
                )
              renderSelectionHighlights(
                context.surface,
                buffer,
                visualLine,
                rect,
                screenY,
                context.cellMetrics.toPixelY(screenY),
                state.persisted.theme,
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
        buffer.editing.cursors.zipWithIndex.foreach { (cursor, cursorIndex) =>
          val isPrimaryCursor = cursorIndex == 0
          val shouldRenderCursor =
            context.cursorVisible || (buffer.editing.cursors.size > 1 && !isPrimaryCursor)
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
                  context.surface.pixels.fillPixelRect(
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
    context.surface.text.setFont(context.textFont)
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

    context.surface.text.setFont(context.textFont)
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
      surface.text.fontRenderContext match
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
          surface.text.drawRunPx(
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
    surface.text.setFont(uiFont)
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
              surface.pixels.fillPixelRect(
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
    surface.text.fontRenderContext match
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
        surface.text.drawRunPx(
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

    buffer.editing.cursors.zipWithIndex.flatMap { (cursor, cursorIndex) =>
      val isPrimaryCursor = cursorIndex == 0
      val shouldRenderCursor =
        context.cursorVisible || (buffer.editing.cursors.size > 1 && !isPrimaryCursor)
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
                context.surface.pixels.fillPixelRect(
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

  private def renderFloatingPanels(
    state: AppState,
    context: RenderContext,
    scene: UiSceneSnapshot,
    damage: Damage
  ): Unit =
    context.surface.text.setFont(context.uiFont)
    val overlays     = OverlayViewModel.fromState(state, scene)
    val blurRadius   = SurfaceMaterials.effectiveBlurRadius(state.persisted.config)
    val panelIsDirty = panelDirtyCheck(damage, blurRadius)

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
        case Some(surfaceId) => paintPanelLayer(context, surfaceId, overlay.rect, panelIsDirty(surfaceId))(paint)
        case None            => paint(context)

    overlays.aboveCursor.foreach(paintOverlay)
    val belowOverlays =
      if overlays.belowCursorStack.nonEmpty then overlays.belowCursorStack else overlays.belowCursor.toList
    belowOverlays.foreach(paintOverlay)

  private def renderFloatingBackdrop(
    overlay: TextOverlayView,
    blurRadius: Float,
    config: AppConfig,
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

  private def renderModalLayer(state: AppState, context: RenderContext, scene: UiSceneSnapshot): Unit =
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

  private def renderPinnedPanels(
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
    val panelIsDirty = panelDirtyCheck(damage, blurRadius)
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
            case _ =>
              PinnedPanelRenderer.render(
                layerContext.surface,
                PinnedPanelViewModel.resolve(surface, rect, state).copy(contentRect = Some(node.contentRect)),
                state.persisted.theme,
                state.persisted.config,
                animationState
              )

        paintPanelLayer(context, surface.id, rect, panelIsDirty(surface.id))(paint)
      }
    }

  /** Every pinned panel plus every surface presented as [[SurfacePresentation.Expanded]] -- the two presentation kinds
    * [[renderPinnedPanels]] paints identically (an expanded panel is a pinned panel temporarily grown to fill more of
    * the workspace; both read their geometry from the same scene node and the same [[PinnedPanelRenderer]]).
    */
  private def pinnedAndExpandedSurfaces(state: AppState): List[UiSurface] =
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
    val widthPx =
      scaledImagePixelDimension(contentWidthCells * context.cellMetrics.charWidth, context.surface.devicePixelScaleX)
    val heightPx =
      scaledImagePixelDimension(contentHeightCells * context.cellMetrics.lineHeight, context.surface.devicePixelScaleY)
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
      val maxSourceLines = markdownPreviewSourceLineLimit(visibleRows).max(1)
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

  private def renderLineNumbers(state: AppState, context: RenderContext, renderPlan: EditorPaneRenderPlan): Unit =
    if state.persisted.config.surfaceConfig.showLineNumbers then
      context.surface.text.setFont(context.uiFont)
      renderPlan.layoutContract.lineNumberRect foreach { lineRect =>
        val surface = context.surface

        surface.setBackgroundColor(state.persisted.theme.panel.background)
        surface.setForegroundColor(state.persisted.theme.muted)

        surface.fillRect(lineRect.x, lineRect.y, lineRect.width, lineRect.height, ' ')
        state.persisted.layout.activeEditorPaneId
          .flatMap(state.persisted.layout.editorPanes.get)
          .foreach { pane =>
            val buffer = pane.bufferId.flatMap(state.persisted.buffers.get)
            val snapshot =
              state.persisted.layout.activeEditorPaneId
                .flatMap(renderPlan.snapshots.get)
                .orElse {
                  for
                    paneLayout <- state.persisted.layout.activeEditorPaneId.flatMap(renderPlan.paneLayouts.get)
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
                      shouldRenderLineNumberForVisualLine(
                        visualLine,
                        state.persisted.config.surfaceConfig.wordWrapEnabled
                      )
                    val lineNumberText =
                      if rendersLineNumber then
                        val numberWidth = math.max(1, lineRect.width - 1)
                        (visualLine.bufferLine + 1).toString.reverse.padTo(numberWidth, ' ').reverse + " "
                      else continuationIndicatorText(lineRect.width)
                    val measuredLineNumberFont = buffer.filter(useMeasuredLineNumberFont(_, context))
                    if usesMeasuredDrawing(snapshot, context) && measuredLineNumberFont.nonEmpty then
                      measuredLineNumberFont.foreach(buf => surface.text.setFont(context.fontForBuffer(buf)))
                      surface.text.drawRunPx(
                        context.cellMetrics.toPixelX(lineRect.x).toFloat,
                        lineTopPx,
                        lineRect.width * context.cellMetrics.charWidth.toFloat,
                        snapshot.lineHeightPx,
                        snapshot.ascentPx,
                        lineNumberText
                      )
                      surface.text.setFont(context.uiFont)
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
        case Some(1) => state.persisted.theme.error.foreground
        case Some(2) => state.persisted.theme.warning.foreground
        case _       => state.persisted.theme.muted
      surface.setForegroundColor(color)
      surface.setBackgroundColor(state.persisted.theme.panel.background)
      surface.putString(lineRect.x + lineRect.width - 1, screenY, "!")

  private def renderGutter(state: AppState, context: RenderContext, contract: EditorLayoutContract): Unit =
    contract.gutterRect.foreach { gutterRect =>
      context.surface.text.setFont(context.uiFont)
      val surface = context.surface

      surface.setBackgroundColor(state.persisted.theme.panel.background)
      surface.setForegroundColor(state.persisted.theme.panel.foreground)

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
    surface.text.fontRenderContext match
      case Some(frc) =>
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
          fontRenderContext = frc
        )

        surface.text.drawRunPx(
          xPx = placement.xPx,
          yPx = placement.yPx,
          bgWidthPx = placement.widthPx,
          lineHeightPx = placement.lineHeightPx,
          ascentPx = placement.ascentPx,
          s = text
        )
      case None =>
        val middleRow = rect.y + math.max(0, (rect.height - 1) / 2)
        CharacterRenderer.renderString(surface, rect.x, middleRow, text.take(math.max(0, rect.width)))

  private def buildGutterContent(state: AppState): String =
    val base =
      if state.persisted.config.cursorInfoBarPlacement == CursorInfoBarPlacement.PinnedBottom then
        state.cursorInfoBarText.map(text => s" $text ").getOrElse(legacyGutterContent(state))
      else legacyGutterContent(state)
    // Opt-in (`surfaceConfig.showWordCount`, off by default): appended as its own segment rather than folded into
    // `legacyGutterContent`/`cursorInfoBarText`, both of which existing callers assert on as exact strings.
    state.wordCountStatusText.fold(base)(segment => s" ${base.trim} | $segment ")

  private def legacyGutterContent(state: AppState): String =
    state.persisted.layout.activeEditorPaneId.flatMap(state.persisted.layout.editorPanes.get) match
      case Some(pane) =>
        pane.bufferId.flatMap(state.persisted.buffers.get) match
          case Some(buffer) =>
            val cursor   = buffer.editing.cursors.headOption.getOrElse(CursorPosition(0, 0))
            val position = s"Line ${cursor.line + 1}, Col ${cursor.column + 1}"
            val language = buffer.document.language.fold("Plain Text")(_.displayName)

            val filePath = buffer.document.filePath match
              case Some(path) => s" | ${path.getFileName}"
              case None       => " | Not saved to file yet"

            s" $position | Language: $language$filePath "
          case None => " No active buffer "
      case None => " No active editor pane "
