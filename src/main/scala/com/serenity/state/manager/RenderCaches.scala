package com.serenity.state.manager

import com.serenity.diagnostics.FrameCounters
import com.serenity.markdown.MarkdownPreviewCache
import com.serenity.ui.layout.WrappedLineCache
import com.serenity.ui.renderer.{
  GraphemeSegmentationCache,
  LabelSprites,
  MeasuredTextAdvances,
  RendererFrameState,
  ShapedTextCache,
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
  * frame drew into, and the renderer's text caches: [[ShapedTextCache]]'s shaped glyphs, [[StyledFontCache]]'s derived
  * fonts, [[LabelSprites]]' rendered labels and [[MeasuredTextAdvances]]' overlay advances.
  *
  * The shaped-text cache is the one layout and painting meet in: `wrappedLines` measures through it and the surface
  * paints through it, so a line is shaped once for both. They must therefore be this one instance, which is why
  * [[create]] hands the same `shapedText` to each.
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
    val shapedText: ShapedTextCache,
    val styledFonts: StyledFontCache,
    val labelSprites: LabelSprites,
    val measuredAdvances: MeasuredTextAdvances
):

  /** What a Java2D surface drawing for this owner paints text through. */
  def surfaceText: SurfaceTextCaches = SurfaceTextCaches(labelSprites, styledFonts, shapedText)

object RenderCaches:

  /** `rendererFrameStateCacheCapacity` mirrors `RendererFrameState.cacheCapacity`'s own default/config source
    * (`AppConfig.surfaceConfig.rendererFrameStateCacheCapacity`) -- callers that build a `RenderCaches` before a config
    * is loaded (or that don't care) get the same 64-entry default that module has always used.
    */
  def create(rendererFrameStateCacheCapacity: Int = 64): RenderCaches =
    val shapedText   = ShapedTextCache()
    val wrappedLines = WrappedLineCache.bounded(shapedText = shapedText)
    new RenderCaches(
      RendererFrameState(rendererFrameStateCacheCapacity),
      ThemeHighlightCache(),
      GraphemeSegmentationCache(),
      AuthoritativeUiScene(wrappedLines),
      MarkdownPreviewCache(),
      ChapterGhostCache(),
      wrappedLines,
      FrameCounters(),
      shapedText,
      StyledFontCache(),
      LabelSprites(LabelSprites.DefaultCapacity),
      MeasuredTextAdvances(MeasuredTextAdvances.DefaultCapacity, shapedText)
    )
