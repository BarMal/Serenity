package com.serenity.state.manager

import com.serenity.markdown.MarkdownPreviewCache
import com.serenity.ui.renderer.{GraphemeSegmentationCache, RendererFrameState}
import com.serenity.ui.theme.ThemeHighlightCache

/** Every render/mouse-hit-testing cache that issue #1677 found still living as a JVM-wide singleton `object`, bundled
  * into one instance-scoped owner: [[RendererFrameState]]'s per-frame caches, [[ThemeHighlightCache]]'s syntax
  * highlight memoization, [[GraphemeSegmentationCache]]'s grapheme-boundary memoization, [[AuthoritativeUiScene]]'s
  * prepared-scene cache (with its own lock, now scoped to this instance rather than the JVM), and
  * [[MarkdownPreviewCache]]'s markdown-preview HTML/image/inline-document caches.
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
    val chapterGhosts: ChapterGhostCache
)

object RenderCaches:

  /** `rendererFrameStateCacheCapacity` mirrors `RendererFrameState.cacheCapacity`'s own default/config source
    * (`AppConfig.surfaceConfig.rendererFrameStateCacheCapacity`) -- callers that build a `RenderCaches` before a config
    * is loaded (or that don't care) get the same 64-entry default that module has always used.
    */
  def create(rendererFrameStateCacheCapacity: Int = 64): RenderCaches =
    new RenderCaches(
      RendererFrameState(rendererFrameStateCacheCapacity),
      ThemeHighlightCache(),
      GraphemeSegmentationCache(),
      AuthoritativeUiScene(),
      MarkdownPreviewCache(),
      ChapterGhostCache()
    )
