package com.serenity.ui.benchmark

import java.util.concurrent.Executor

import com.serenity.state.manager.{RenderCaches, ScrollCountExecutor, ScrollCounting}
import com.serenity.ui.skia.SkiaTextCaches

/** The caches a benchmark window draws with, made the way the app makes its own (#2465, M1): a pane's whole-document
  * row count is measured after the frame that needs it, on an executor, so the frame draws without the scrollbar and
  * does not wait for the count; and a document opened in a running editor finds the font sets already made, because
  * they depend on the fonts and not on the text.
  *
  * One counting serves every set made here, as one serves every document in the app, so a newer frame's count replaces
  * an older frame's instead of the two queueing behind each other.
  */
final class BenchmarkCaches(executor: Executor = ScrollCountExecutor.shared):

  private val counting = ScrollCounting.deferred(executor, _ => ())

  /** The caches of frames that follow one another: typing, scrolling, a caret blinking. */
  def shared(): RenderCaches = RenderCaches.create(scrollCounting = counting)

  /** The caches of a document opened after `shared` has drawn: its font sets, and nothing else. */
  def cold(shared: RenderCaches): RenderCaches =
    val fresh = RenderCaches.create(scrollCounting = counting, styledFonts = shared.styledFonts)
    SkiaTextCaches.keepingFonts(shared.backendCaches, fresh.backendCaches)
    fresh
