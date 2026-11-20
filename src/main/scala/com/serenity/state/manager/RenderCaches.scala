package com.serenity.state.manager

import com.serenity.diagnostics.{FrameCounters, FramePhaseClock}
import com.serenity.markdown.MarkdownPreviewCache
import com.serenity.state.reducers.CompositionRevisions
import com.serenity.ui.layout.{
  AnchorWrapCache,
  CellMetrics,
  LayoutMemos,
  ScrollRowsReporter,
  TabBarMeasures,
  TextMeasurer,
  WrappedLineCache
}
import com.serenity.ui.renderer.{
  BackendCaches,
  CacheSlots,
  GraphemeSegmentationCache,
  MeasuredTextAdvances,
  RendererFrameState,
  StyledFontCache,
  SurfaceTextCaches
}
import com.serenity.ui.theme.ThemeHighlightCache

/** Every render/mouse-hit-testing cache that issue #1677 found still living as a JVM-wide singleton `object`, bundled
  * into one instance-scoped owner: [[RendererFrameState]]'s per-frame caches, [[ThemeHighlightCache]]'s syntax
  * highlight memoization, [[GraphemeSegmentationCache]]'s grapheme-boundary memoization, [[AuthoritativeUiScene]]'s
  * prepared-scene cache (with its own lock, now scoped to this instance rather than the JVM),
  * [[MarkdownPreviewCache]]'s markdown-preview HTML/image/inline-document caches, and [[WrappedLineCache]]'s wrapped
  * lines, which the scene and the state-update path (through the scene it is handed) both read, together with the
  * per-buffer visual-row indexes it keeps for scrolling and jumps, the [[FrameCounters]] the renderer counts what each
  * frame drew into, and the renderer's text caches: [[StyledFontCache]]'s derived fonts and [[MeasuredTextAdvances]]'
  * overlay advances.
  *
  * One instance is created per render-owning entity -- today, once per [[StateManager]] (see `StateManager.apply`) --
  * and threaded explicitly: down through [[com.serenity.ui.renderer.RenderContext]] to every render entry point and
  * frame-planning call, and via each mouse-hit-testing capability's port to every hit-testing call site. Two
  * independently constructed `StateManager`s therefore share no cache state and contend on no lock, satisfying #1677's
  * acceptance criterion that two `StateManager`s with different cache capacities can run concurrently in one JVM.
  */
final class RenderCaches private (
    val frameState: RendererFrameState,
    val themeHighlightCache: ThemeHighlightCache,
    val graphemeSegmentationCache: GraphemeSegmentationCache,
    val authoritativeScene: AuthoritativeUiScene,
    val markdownPreviewCache: MarkdownPreviewCache,
    val chapterGhosts: ChapterGhostCache,
    val wrappedLines: WrappedLineCache,
    val frameCounters: FrameCounters,
    val styledFonts: StyledFontCache,
    val measuredAdvances: MeasuredTextAdvances,
    val tabBarMeasures: TabBarMeasures,
    val compositionRevisions: CompositionRevisions,
    val backendCaches: BackendCaches,
    val framePhases: FramePhaseClock,
    val scrollRows: ScrollRowsReporter,
    measurerSource: () => Option[TextMeasurer]
):

  /** What layout measures with, made from this owner's backend caches the first time it is asked for, by this or by any
    * of the caches layout measures through: the backend that draws for this owner supplies it, so layout and painting
    * share faces and shaped stretches. None measures nothing: layout is the cell grid.
    */
  def measurer: Option[TextMeasurer] = measurerSource()

  /** The wraps measured while locating a caret, on the measurer this owner draws and lays out with. */
  def anchorWraps: AnchorWrapCache = AnchorWrapCache()(() => measurer)

  /** The grid of `font` that layout measures on: the owner's measurer's, and AWT's where the owner has none. */
  def cellMetricsOf(font: java.awt.Font): CellMetrics =
    measurer.fold(CellMetrics.fromFont(font))(_.cellMetrics(com.serenity.ui.renderer.FontSpec.fromAwt(font)))

  /** What a surface drawing for this owner paints text through. */
  def surfaceText: SurfaceTextCaches = SurfaceTextCaches(styledFonts)

object RenderCaches:

  /** `rendererFrameStateCacheCapacity` mirrors `RendererFrameState.cacheCapacity`'s own default/config source
    * (`AppConfig.surfaceConfig.rendererFrameStateCacheCapacity`) -- callers that build a `RenderCaches` before a config
    * is loaded (or that don't care) get the same 64-entry default that module has always used.
    */
  def create(
    rendererFrameStateCacheCapacity: Int = 64,
    scrollCounting: ScrollCounting = ScrollCounting.Inline,
    styledFonts: StyledFontCache = StyledFontCache(),
    measurerFor: BackendCaches => Option[TextMeasurer] = _ => None,
    sharedBackendCaches: CacheSlots = new CacheSlots
  ): RenderCaches =
    val backend        = new BackendCaches(sharedBackendCaches)
    lazy val measurer  = measurerFor(backend)
    val measurerSource = () => measurer
    val wrappedLines   = WrappedLineCache.sharing(LayoutMemos(measurerSource = measurerSource))
    new RenderCaches(
      RendererFrameState(rendererFrameStateCacheCapacity),
      ThemeHighlightCache(),
      GraphemeSegmentationCache(),
      AuthoritativeUiScene(wrappedLines, scrollCounting = scrollCounting),
      MarkdownPreviewCache(),
      ChapterGhostCache(),
      wrappedLines,
      FrameCounters(),
      styledFonts,
      MeasuredTextAdvances(MeasuredTextAdvances.DefaultCapacity, wrappedLines.layoutMemos),
      new TabBarMeasures(measurerSource),
      new CompositionRevisions,
      backend,
      new FramePhaseClock(),
      ScrollRowsReporter.fromSystemProperty(),
      measurerSource
    )
