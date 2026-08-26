package com.serenity.state.manager

import cats.syntax.all.*
import com.serenity.animation.AnimationState
import com.serenity.config.RenderDamageGranularity
import com.serenity.lsp.model.Diagnostic
import com.serenity.rope.{Balance, RopeDiff}
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*

/** Computes what a transition between two `AppState`s damaged, so the render loop doesn't have to rediscover it by
  * diffing frames -- the wake-up decision this drives is wired in by `#998` (`AppRuntime.inputEventPhase`). `#999` is
  * migrating `Renderer.planFrame`'s own `ChromeKey`/`dirtyRowsAgainst` machinery onto this same signal for the
  * paint-scope decision, which is why this producer also reports dimensions `#998`'s wake-up decision alone never
  * needed -- cursor and selection movement, comment/diagnostic annotations, and language reclassification -- each of
  * those already invalidates pixels today via `Renderer`'s own `PaneRowKey`/`PaneContentKey` structural comparison, so
  * this producer has to cover them before `planFrame` can safely trust `Damage` instead of that comparison. See
  * `CursorViewport.ensureVisibleCursors`'s doc comment for why per-reducer-branch emission was rejected in favour of
  * this boundary-pass pattern.
  *
  * Buffer content damage goes through `RopeDiff`, which finds the changed offset range by walking the rope's persistent
  * tree structure rather than comparing text, so its cost tracks how much of the document an edit actually touched
  * rather than the document's size.
  *
  * Called from two funnel points: `AppRuntime.inputEventPhase` around each input event, and
  * `AppRuntime.fastRenderPhase` (via `advanceAnimationsForCadence`) around each animation tick -- the latter mutates
  * state entirely outside `inputEventPhase`, so it needs its own before/after diff to report
  * `animationDamage`/`fullRenderDamage` at all.
  *
  * `#1000` retires `Renderer.planFrame`'s `overlaysMayCoverPanes` stand-down in favour of `fullRenderDamage`'s
  * `uiSurfaces`/`focus` checks below, rather than reasoning about each overlay's precise pixel footprint (shadow/blur
  * bleed included) -- see that function's doc comment for why a coarse "the whole frame redraws whenever an overlay
  * changes" is both correct and enough to fix the actual reported problem (every frame redrawing while any overlay is
  * merely visible, not just while one is changing).
  */
object DamageProducer:

  def forTransition(
    before: AppState,
    after: AppState,
    beforeAnimations: Map[BufferId, AnimationState] = Map.empty,
    afterAnimations: Map[BufferId, AnimationState] = Map.empty
  )(using Balance): Damage =
    val granularity = after.config.surfaceConfig.renderDamageGranularity
    val bufferDamage = after.buffers.foldLeft(Damage.Nothing: Damage) {
      case (acc, (bufferId, afterBuffer)) =>
        before.buffers.get(bufferId) match
          case None => acc
          case Some(beforeBuffer) =>
            acc |+| bufferDamageFor(
              bufferId,
              before,
              after,
              beforeBuffer,
              afterBuffer,
              granularity,
              beforeAnimations.getOrElse(bufferId, AnimationState.empty),
              afterAnimations.getOrElse(bufferId, AnimationState.empty)
            )
    }
    bufferDamage |+| chromeDamage(before, after) |+| fullRenderDamage(before, after) |+| paneChromeDamage(before, after)

  /** Everything about one buffer's own state that can dirty its visible rows without necessarily touching its rope
    * content -- cursor and selection movement, comment/diagnostic annotation changes, and a language reclassification
    * (which recolors every row via syntax highlighting). Each check is independent and safe to over-report: reporting a
    * row that turned out not to need repainting just costs a redundant draw, matching the bias documented on `RopeDiff`
    * and `DirtyLineDiff`.
    */
  private def bufferDamageFor(
    bufferId: BufferId,
    before: AppState,
    after: AppState,
    beforeBuffer: Buffer,
    afterBuffer: Buffer,
    granularity: RenderDamageGranularity,
    beforeAnimations: AnimationState,
    afterAnimations: AnimationState
  )(using Balance): Damage =
    contentDamage(bufferId, beforeBuffer, afterBuffer, granularity) |+|
      cursorDamage(bufferId, beforeBuffer, afterBuffer) |+|
      selectionDamage(bufferId, beforeBuffer, afterBuffer) |+|
      commentDamage(bufferId, beforeBuffer, afterBuffer) |+|
      diagnosticDamage(bufferId, before, after, beforeBuffer, afterBuffer) |+|
      languageDamage(bufferId, beforeBuffer, afterBuffer) |+|
      viewportDamage(bufferId, beforeBuffer, afterBuffer) |+|
      animationDamage(bufferId, beforeAnimations, afterAnimations) |+|
      focusDimmingDamage(bufferId, after, beforeBuffer, afterBuffer)

  private def contentDamage(
    bufferId: BufferId,
    before: Buffer,
    after: Buffer,
    granularity: RenderDamageGranularity
  )(using Balance): Damage =
    if isSameReference(before.document.content, after.document.content) then Damage.Nothing
    else
      RopeDiff.changedOffsetRange(before.document.content, after.document.content) match
        case None => Damage.Nothing
        case Some((start, end)) =>
          cellDamage(bufferId, after, start, end, granularity)
            .getOrElse(Damage.BufferRows(bufferId, rowsForOffsetRange(after, start, end)))

  /** Column-precise damage for an edit confined to one row of a monospaced buffer under `Cells` granularity. `None`
    * whenever that doesn't hold, so the caller falls back to row-level damage -- multi-row edits (including a merged
    * offset range spanning several multi-cursor edits) lose per-row precision by construction, and a buffer using
    * measured/proportional layout (`Buffer.typographyRole.usesTextFont`) can reshape glyphs across a clipped column
    * boundary, which is the correctness constraint #997 documents for this setting.
    */
  private def cellDamage(
    bufferId: BufferId,
    after: Buffer,
    start: Int,
    end: Int,
    granularity: RenderDamageGranularity
  ): Option[Damage] =
    Option
      .when(granularity == RenderDamageGranularity.Cells && !after.typographyRole.usesTextFont) {
        val weight                   = after.document.content.weight
        val clampedStart             = math.max(0, math.min(start, weight))
        val lastOffset               = math.max(clampedStart, math.min(end, weight) - 1)
        val (startLine, startColumn) = after.document.content.offsetToLineColumn(clampedStart)
        val (endLine, endColumn)     = after.document.content.offsetToLineColumn(lastOffset)
        Option.when(startLine == endLine)(Damage.BufferCells(bufferId, startLine, startColumn, Some(endColumn + 1)))
      }
      .flatten

  /** The lines `[start, end)` (an exclusive offset range in `after`'s content) touches. A pure deletion reports
    * `end == start`, which still damages the one line the deletion landed on, so the last-affected offset is clamped to
    * be at least `start` rather than `end - 1` going negative relative to it.
    */
  private def rowsForOffsetRange(after: Buffer, start: Int, end: Int): Set[Int] =
    val weight         = after.document.content.weight
    val clampedStart   = math.max(0, math.min(start, weight))
    val lastOffset     = math.max(clampedStart, math.min(end, weight) - 1)
    val (startLine, _) = after.document.content.offsetToLineColumn(clampedStart)
    val (endLine, _)   = after.document.content.offsetToLineColumn(lastOffset)
    (startLine to endLine).toSet

  private def cursorDamage(bufferId: BufferId, before: Buffer, after: Buffer): Damage =
    if before.editing.cursors == after.editing.cursors then Damage.Nothing
    else Damage.BufferRows(bufferId, (before.editing.cursors ++ after.editing.cursors).map(_.line).toSet)

  private def selectionDamage(bufferId: BufferId, before: Buffer, after: Buffer): Damage =
    if before.allSelections == after.allSelections then Damage.Nothing
    else Damage.BufferRows(bufferId, selectionLines(before.allSelections) ++ selectionLines(after.allSelections))

  private def selectionLines(selections: List[Selection]): Set[Int] =
    selections.iterator.flatMap(selection => selection.start.line to selection.end.line).toSet

  private def commentDamage(bufferId: BufferId, before: Buffer, after: Buffer): Damage =
    if before.annotations.documentComments == after.annotations.documentComments then Damage.Nothing
    else
      Damage.BufferRows(
        bufferId,
        commentLines(before.annotations.documentComments) ++ commentLines(after.annotations.documentComments)
      )

  private def commentLines(comments: List[DocumentComment]): Set[Int] =
    comments.iterator.flatMap(comment => comment.start.line to comment.end.line).toSet

  /** Diagnostics live in `AppState.diagnostics`, keyed by URI rather than on the buffer itself, so this reads both
    * sides' URIs rather than assuming they match -- a save-as between `before` and `after` changes a buffer's URI, and
    * this must not silently compare the wrong two lists (or worse, the same list against itself) when that happens.
    */
  private def diagnosticDamage(
    bufferId: BufferId,
    before: AppState,
    after: AppState,
    beforeBuffer: Buffer,
    afterBuffer: Buffer
  ): Damage =
    val beforeDiagnostics = before.diagnostics.getOrElse(SpellChecker.diagnosticsUri(beforeBuffer), Nil)
    val afterDiagnostics  = after.diagnostics.getOrElse(SpellChecker.diagnosticsUri(afterBuffer), Nil)
    if beforeDiagnostics == afterDiagnostics then Damage.Nothing
    else Damage.BufferRows(bufferId, diagnosticLines(beforeDiagnostics) ++ diagnosticLines(afterDiagnostics))

  private def diagnosticLines(diagnostics: List[Diagnostic]): Set[Int] =
    diagnostics.iterator.flatMap(diagnostic => diagnostic.range.start.line to diagnostic.range.end.line).toSet

  /** A language reclassification changes every row's syntax highlighting, not just the rows an edit touched, so this
    * reports the buffer's full line extent rather than trying to reason about which rows actually recolor.
    */
  private def languageDamage(bufferId: BufferId, before: Buffer, after: Buffer): Damage =
    if before.document.language == after.document.language then Damage.Nothing
    else Damage.BufferRows(bufferId, (0 until after.document.content.lineCount).toSet)

  /** Scrolling shifts which buffer line each visual row shows, so every visible row's content changes even though
    * nothing about the buffer's own data did -- this producer has no layout knowledge of which rows are actually on
    * screen, so it reports the buffer's full line extent, the same coarse-but-safe bias [[languageDamage]] uses.
    * `Renderer`'s retired row-by-row structural diff already redrew close to every visible row on a scroll in practice
    * (a shifted row rarely matches what the previous frame had at the same row index), so this is not a regression from
    * the pixels it replaces.
    */
  private def viewportDamage(bufferId: BufferId, before: Buffer, after: Buffer): Damage =
    if before.viewport == after.viewport then Damage.Nothing
    else Damage.BufferRows(bufferId, (0 until after.document.content.lineCount).toSet)

  /** Character-reveal (and other per-cell) animation ticks report exactly the rows whose cells changed, read off
    * `AnimationState.animations`'s `CharacterKey`s -- the same map `PaneRowKey.animations` (`Renderer.scala`) reads
    * today to decide row reuse, so this is a direct structural read rather than a coarsening.
    */
  private def animationDamage(bufferId: BufferId, before: AnimationState, after: AnimationState): Damage =
    if before == after then Damage.Nothing
    else Damage.BufferRows(bufferId, changedAnimationLines(before, after))

  /** `Renderer.focusedTextBodyLines` dims every row outside the active paragraph/markdown-block around the cursor.
    * Moving the cursor within the same block changes nothing this needs to report beyond what [[cursorDamage]] already
    * covers, but crossing into a different block flips the dimmed state of every row in the old block that isn't also
    * in the new one (and vice versa) -- a much wider set than just the old and new cursor row, so this has to be
    * computed via [[FocusedTextBody]] (shared with `Renderer` so the two can never disagree) rather than derived from
    * the cursor move alone. Toggling the feature itself is a config change, already caught by [[chromeDamage]]'s
    * blanket `Everything`, so this only has to reason about the range shifting while the feature stays enabled.
    */
  private def focusDimmingDamage(
    bufferId: BufferId,
    after: AppState,
    beforeBuffer: Buffer,
    afterBuffer: Buffer
  ): Damage =
    if !after.config.focusedTextBodyEnabled then Damage.Nothing
    else
      val beforeRange = FocusedTextBody.activeRange(beforeBuffer, beforeBuffer.editing.cursors.headOption.map(_.line))
      val afterRange  = FocusedTextBody.activeRange(afterBuffer, afterBuffer.editing.cursors.headOption.map(_.line))
      if beforeRange == afterRange then Damage.Nothing
      else
        val beforeLines = beforeRange.map(_.toSet).getOrElse((0 until beforeBuffer.document.content.lineCount).toSet)
        val afterLines  = afterRange.map(_.toSet).getOrElse((0 until afterBuffer.document.content.lineCount).toSet)
        Damage.BufferRows(bufferId, beforeLines.diff(afterLines) ++ afterLines.diff(beforeLines))

  private def changedAnimationLines(before: AnimationState, after: AnimationState): Set[Int] =
    (before.animations.keySet ++ after.animations.keySet).iterator
      .filter(key => before.animations.get(key) != after.animations.get(key))
      .map(_.line)
      .toSet

  /** The theme, or *any* config change, forces a full repaint. Config covers far more than the syntax-highlighting
    * toggle this used to check individually -- word wrap, fonts, margins, blur radius, and dozens of other fields this
    * producer has no per-field model for, each of which can reshape or recolor pane content in ways a narrower check
    * would silently miss. `Renderer`'s retired `ChromeKey`/`PaneContentKey` machinery caught all of these the same
    * blunt way, via `ReferenceIdentity(state.config)`; a structural comparison here gives the same safety without
    * having to enumerate what every field does to a rendered frame.
    */
  private def chromeDamage(before: AppState, after: AppState): Damage =
    if before.theme != after.theme || before.config != after.config then Damage.Everything
    else Damage.Nothing

  /** Transitions that touch every visible glyph rather than any one buffer's rows, matching what
    * `AppRuntime.needsFullContentRender` already treats as requiring a full canvas repaint: a theme transition
    * cross-fades every glyph and background colour in flight, and a surface animation composites through the same
    * full-render path as any other overlay (see that function's doc comment for why the window sitter alone is exempt
    * -- it never touches the canvas at all, so it contributes no damage here).
    *
    * `uiSurfaces` changing covers a floating, pinned, modal or expanded surface appearing, moving, resizing or changing
    * content -- `Renderer`'s retired `overlaysMayCoverPanes` stand-down disabled row reuse outright whenever any such
    * surface was merely visible, because those layers draw shadows, blur and translucency that reach outside the
    * rectangles the scene reports. Reporting `Everything` only when this actually changes keeps that same safety (a
    * redraw whenever the overlay itself changes, wiping out any stale bleed) while letting ordinary content edits
    * elsewhere -- typing with the command runner open -- report their own precise row damage instead, which is the
    * whole point of `#1000`. `pinnedSurfaces` is a filtered/reordered projection of `uiSurfaces` (plus `layout`,
    * already covered by `paneChromeDamage`), so it needs no separate check. `focus` changing can retarget which
    * floating surface `OverlayViewModel.preferredFloatingSurface` selects, or its dim/active tint, without `uiSurfaces`
    * itself changing (tabbing between two already-open floating panels).
    */
  private def fullRenderDamage(before: AppState, after: AppState): Damage =
    if before.themeTransition != after.themeTransition ||
        before.surfaceAnimations != after.surfaceAnimations ||
        before.uiSurfaces != after.uiSurfaces ||
        before.focus != after.focus
    then Damage.Everything
    else Damage.Nothing

  /** Pane headers, gutter text and line numbers -- `Renderer.ChromeKey`'s `layout`/`gutterText`/`lineNumberRows`/
    * `headers` fields, keyed by `PaneId` rather than `BufferId`. `state.layout` keeps the same object reference across
    * any transition that doesn't touch pane structure or the active pane (Scala's `.copy` only allocates a new object
    * for the field actually changed), so a reference check here is both cheap and an exact match for what `ChromeKey`'s
    * own `ReferenceIdentity(state.layout)` already invalidates on today -- reported as `Everything` since a changed
    * active pane or pane structure affects the gutter, every pane's header, and line numbers all at once, and figuring
    * out a narrower blast radius from `AppState` alone would just re-derive what `Renderer`'s layout engine computes.
    */
  private def paneChromeDamage(before: AppState, after: AppState): Damage =
    if before.layout ne after.layout then Damage.Everything
    else paneHeaderDamage(before, after) |+| gutterDamage(before, after)

  /** A pane's header shows the active highlight, the buffer's filename and its dirty indicator -- exactly the four
    * inputs `Renderer.chromeKeyFor`'s `headers` field reads. `activeEditorPaneId` can't differ here (the layout
    * reference is unchanged, see [[paneChromeDamage]]), so only title/dirty/buffer-identity ever trip this.
    */
  private def paneHeaderDamage(before: AppState, after: AppState): Damage =
    after.layout.orderedPaneIds.foldLeft(Damage.Nothing: Damage) { (acc, paneId) =>
      if headerInputs(before, paneId) == headerInputs(after, paneId) then acc
      else acc |+| Damage.PaneChrome(paneId)
    }

  private def headerInputs(state: AppState, paneId: PaneId) =
    state.layout.editorPanes.get(paneId).map { pane =>
      val buffer = pane.bufferId.flatMap(state.buffers.get)
      (
        buffer.flatMap(_.document.filePath).flatMap(path => Option(path.getFileName)).map(_.toString),
        buffer.exists(_.document.isDirty),
        buffer.map(_.id.value)
      )
    }

  /** The legacy gutter (`Renderer.legacyGutterContent`) shows the active pane's cursor position, language and filename,
    * and line numbers follow the active pane's own visible lines -- so any of those changing on the active buffer
    * dirties the gutter/line-number chrome, on top of whatever row damage that buffer's own content reports.
    */
  private def gutterDamage(before: AppState, after: AppState): Damage =
    if activeGutterInputs(before) == activeGutterInputs(after) then Damage.Nothing else Damage.Chrome

  private def activeGutterInputs(state: AppState) =
    for
      paneId   <- state.layout.activeEditorPaneId
      pane     <- state.layout.editorPanes.get(paneId)
      bufferId <- pane.bufferId
      buffer   <- state.buffers.get(bufferId)
    yield (buffer.editing.cursors, buffer.document.language, buffer.document.filePath, buffer.viewport)

  private def isSameReference(a: AnyRef, b: AnyRef): Boolean = a eq b
