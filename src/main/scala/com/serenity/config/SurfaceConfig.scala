package com.serenity.config

import com.serenity.keystroke.Modifier
import com.serenity.state.models.SurfacePlacement

final case class SurfaceConfig(
    showLineNumbers: Boolean = true,
    // Placement and spacing of the line-number counter (independent of interface density). `showLineNumbers` remains the
    // master on/off; this only takes effect while that is on.
    lineNumberLayout: LineNumberLayout = LineNumberLayout(),
    showPaneHeaders: Boolean = true,
    // Opt-in: the click/mouse-hit-testing and comment-lens behaviour is covered by exact-state assertions elsewhere,
    // so this defaults to the smaller, non-disruptive mode and callers opt into margin mode explicitly (#1222).
    commentDisplayMode: CommentDisplayMode = CommentDisplayMode.Floating,
    wordWrapEnabled: Boolean = true,
    // Whether Up/Down under word wrap follow visual rows (the wrapped screen line) rather than jumping straight to
    // the previous/next logical line. Independent of wordWrapEnabled itself: only takes effect while wrap is also on.
    visualLineCursorNavigation: Boolean = true,
    // Off by default (preserves `CursorViewport.adjustForCursor`'s existing behaviour exactly): the cursor's line is
    // recentred on every move, but never past the document's own end, so a viewport near the last line falls back to
    // showing as much real content as fits rather than centring. On, that end clamp is lifted -- the caret's line
    // stays at its centred row even while typing at the very end of the document, padding with blank rows below it
    // the way iA Writer/Ulysses-style typewriter scrolling does (#1204, #1293).
    typewriterScrollingEnabled: Boolean = false,
    // E-reader-style column layout (issue #1338, Phase 1): global toggle, no per-document/per-pane settings
    // infrastructure exists today. Only takes effect while `wordWrapEnabled` is also on -- otherwise a no-op, falling
    // back to ordinary vertical scrolling.
    columnModeEnabled: Boolean = false,
    // Target column width in cells; used only in Auto mode (`columnCount = None`), where the count is however many of
    // this width (plus `columnGap`) fit the pane.
    columnTargetWidthCells: Int = 80,
    columnGap: Int = 2,
    // Multi-column e-reader layout (issue #1338, Phase 2 / slice 4): count-driven mode. `None` is Auto -- the
    // width-driven "as many columns of `columnTargetWidthCells` as fit" behaviour above, unchanged. `Some(n)` pins
    // exactly n columns, each column's width auto-derived to fit. The clamp to the pane-dependent maximum lives in
    // `LayoutEngine.resolvedColumnCount`, not in `normalized` -- `normalized` has no pane width to clamp against.
    columnCount: Option[Int] = None,
    focusedTextBodyEnabled: Boolean = false,
    contextualToolbarEnabled: Boolean = true,
    contextualToolbarDisplayMode: ToolbarDisplayMode = ToolbarDisplayMode.IconAndText,
    commandRunnerVisibleRows: Option[Int] = None,
    // issue #1046: `None` (the default) now falls back to the current interface density's own item spacing
    // (`InterfaceDensityMetrics.itemGapRows`, via `AppConfig.effectiveCommandRunnerItemGapRows`) rather than a flat
    // 0.0 regardless of density -- consistent with `commandRunnerVisibleRows`/`commandRunnerCursorGapRows` above,
    // which already fall back to a density-derived default when unset.
    commandRunnerItemGapRows: Option[Double] = None,
    commandRunnerCursorGapRows: Option[Double] = None,
    // Opt-out (unlike `commentDisplayMode`): the persistent key-hint footer (issue #931, Stage 3) is
    // the discoverability fix the stage exists to deliver, so it ships on by default; callers who want the old
    // dynamic-footer-only behaviour turn it off explicitly.
    commandRunnerShowKeyHints: Boolean = true,
    // Experimental prototype (off by default, unlike `commandRunnerShowKeyHints`): holding or double-tapping a bare
    // modifier peeks/opens the command runner near the cursor line. Ships disabled -- this is a single-panel spike,
    // not the finished feature -- and callers opt in explicitly.
    commandRunnerCursorPeekEnabled: Boolean = false,
    // Configurable per-user given real risk of OS/WM collision with Super/Meta on Linux.
    commandRunnerCursorPeekModifier: Modifier = Modifier.Meta,
    // Hold-vs-double-tap threshold in milliseconds. Defaults to `ModifierTapDetector.WindowMillis` (200L) for
    // consistency with the codebase's existing bare-modifier double-tap window (`ctrl+ctrl`-style hotkeys) rather
    // than introducing a second magic number.
    commandRunnerCursorPeekTapWindowMillis: Long = 200L,
    commandRunnerCursorPeekPlacement: SurfacePlacement = SurfacePlacement.BelowCursor,
    renderFpsTarget: RenderFpsTarget = RenderFpsTarget.Fps60,
    renderDamageGranularity: RenderDamageGranularity = RenderDamageGranularity.Rows,
    textAreaInsets: TextAreaInsets = TextAreaInsets(),
    viewportSizing: ViewportSizing = ViewportSizing(),
    // Per-cache capacity for RendererFrameState's bounded-LRU caches (issue #1433): a Ref-backed Map can't observe
    // GC reachability the way the WeakHashMap it replaced could, so growth is bounded by recency instead. 64 is a
    // conservative default for a single window; tune it up if a session with many concurrently open surfaces (or a
    // busy test run sharing these process-wide caches) sees avoidable extra redraws from eviction churn.
    rendererFrameStateCacheCapacity: Int = 64,
    // Off by default: a cached layer holds a full-window image per modal/panel, and repainting a panel directly costs
    // less than compositing that image back over the whole frame (#1798).
    layerCachingEnabled: Boolean = false,
    frameTimingEnabled: Boolean = false,
    startupWarmUpEnabled: Boolean = true,
    // How strongly a misspelled-word/LSP diagnostic's severity colour shows through its highlight, versus the colour
    // it's painted over -- the current (possibly focus-mode-dimmed) foreground/background, not always the theme's own
    // full-intensity ones (#1530). Was a hardcoded literal in `RendererHighlights` (#1529); 0.45 matches that literal
    // so existing themes render unchanged until a user tunes it.
    diagnosticHighlightBlendWeight: Double = 0.45
):

  def normalized: SurfaceConfig =
    copy(
      rendererFrameStateCacheCapacity = AppConfig.clampRendererFrameStateCacheCapacity(rendererFrameStateCacheCapacity),
      columnGap = columnGap.max(0),
      columnTargetWidthCells = columnTargetWidthCells.max(1),
      commandRunnerVisibleRows = commandRunnerVisibleRows.map(AppConfig.clampCommandRunnerVisibleRows),
      commandRunnerItemGapRows = commandRunnerItemGapRows.map(AppConfig.clampCommandRunnerItemGapRows),
      commandRunnerCursorGapRows = commandRunnerCursorGapRows.map(AppConfig.clampCommandRunnerCursorGapRows),
      commandRunnerCursorPeekTapWindowMillis =
        AppConfig.clampCommandRunnerCursorPeekTapWindowMillis(commandRunnerCursorPeekTapWindowMillis),
      lineNumberLayout = lineNumberLayout.normalized,
      textAreaInsets = textAreaInsets.normalized,
      viewportSizing = viewportSizing.normalized,
      diagnosticHighlightBlendWeight = AppConfig.clampDiagnosticHighlightBlendWeight(diagnosticHighlightBlendWeight)
    )
