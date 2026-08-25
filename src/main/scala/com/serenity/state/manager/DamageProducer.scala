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
  * Not yet covered, and out of scope for this pass: pane chrome (headers, gutter, line numbers -- keyed by `PaneId`,
  * not `BufferId`, and largely derived inside `Renderer` rather than stored on `AppState`), focus-dimming's shifting
  * active-body range, and overlay/surface damage (`#1000`).
  */
object DamageProducer:

  def forTransition(before: AppState, after: AppState)(using Balance): Damage =
    val granularity = after.config.surfaceConfig.renderDamageGranularity
    val bufferDamage = after.buffers.foldLeft(Damage.Nothing: Damage) {
      case (acc, (bufferId, afterBuffer)) =>
        before.buffers.get(bufferId) match
          case None => acc
          case Some(beforeBuffer) =>
            acc |+| bufferDamageFor(bufferId, before, after, beforeBuffer, afterBuffer, granularity)
    }
    bufferDamage |+| chromeDamage(before, after) |+| fullRenderDamage(before, after)

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
    granularity: RenderDamageGranularity
  )(using Balance): Damage =
    contentDamage(bufferId, beforeBuffer, afterBuffer, granularity) |+|
      cursorDamage(bufferId, beforeBuffer, afterBuffer) |+|
      selectionDamage(bufferId, beforeBuffer, afterBuffer) |+|
      commentDamage(bufferId, beforeBuffer, afterBuffer) |+|
      diagnosticDamage(bufferId, before, after, beforeBuffer, afterBuffer) |+|
      languageDamage(bufferId, beforeBuffer, afterBuffer) |+|
      animationDamage(bufferId, beforeBuffer, afterBuffer)

  private def contentDamage(
    bufferId: BufferId,
    before: Buffer,
    after: Buffer,
    granularity: RenderDamageGranularity
  )(using Balance): Damage =
    if isSameReference(before.content, after.content) then Damage.Nothing
    else
      RopeDiff.changedOffsetRange(before.content, after.content) match
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
        val weight                   = after.content.weight
        val clampedStart             = math.max(0, math.min(start, weight))
        val lastOffset               = math.max(clampedStart, math.min(end, weight) - 1)
        val (startLine, startColumn) = after.content.offsetToLineColumn(clampedStart)
        val (endLine, endColumn)     = after.content.offsetToLineColumn(lastOffset)
        Option.when(startLine == endLine)(Damage.BufferCells(bufferId, startLine, startColumn, Some(endColumn + 1)))
      }
      .flatten

  /** The lines `[start, end)` (an exclusive offset range in `after`'s content) touches. A pure deletion reports
    * `end == start`, which still damages the one line the deletion landed on, so the last-affected offset is clamped to
    * be at least `start` rather than `end - 1` going negative relative to it.
    */
  private def rowsForOffsetRange(after: Buffer, start: Int, end: Int): Set[Int] =
    val weight         = after.content.weight
    val clampedStart   = math.max(0, math.min(start, weight))
    val lastOffset     = math.max(clampedStart, math.min(end, weight) - 1)
    val (startLine, _) = after.content.offsetToLineColumn(clampedStart)
    val (endLine, _)   = after.content.offsetToLineColumn(lastOffset)
    (startLine to endLine).toSet

  private def cursorDamage(bufferId: BufferId, before: Buffer, after: Buffer): Damage =
    if before.cursors == after.cursors then Damage.Nothing
    else Damage.BufferRows(bufferId, (before.cursors ++ after.cursors).map(_.line).toSet)

  private def selectionDamage(bufferId: BufferId, before: Buffer, after: Buffer): Damage =
    if before.allSelections == after.allSelections then Damage.Nothing
    else Damage.BufferRows(bufferId, selectionLines(before.allSelections) ++ selectionLines(after.allSelections))

  private def selectionLines(selections: List[Selection]): Set[Int] =
    selections.iterator.flatMap(selection => selection.start.line to selection.end.line).toSet

  private def commentDamage(bufferId: BufferId, before: Buffer, after: Buffer): Damage =
    if before.documentComments == after.documentComments then Damage.Nothing
    else Damage.BufferRows(bufferId, commentLines(before.documentComments) ++ commentLines(after.documentComments))

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
    if before.language == after.language then Damage.Nothing
    else Damage.BufferRows(bufferId, (0 until after.content.lineCount).toSet)

  /** Character-reveal (and other per-cell) animation ticks report exactly the rows whose cells changed, read off
    * `AnimationState.animations`'s `CharacterKey`s -- the same map `PaneRowKey.animations` (`Renderer.scala`) reads
    * today to decide row reuse, so this is a direct structural read rather than a coarsening.
    */
  private def animationDamage(bufferId: BufferId, before: Buffer, after: Buffer): Damage =
    if before.animations == after.animations then Damage.Nothing
    else Damage.BufferRows(bufferId, changedAnimationLines(before.animations, after.animations))

  private def changedAnimationLines(before: AnimationState, after: AnimationState): Set[Int] =
    (before.animations.keySet ++ after.animations.keySet).iterator
      .filter(key => before.animations.get(key) != after.animations.get(key))
      .map(_.line)
      .toSet

  /** Global (not per-buffer) chrome-level changes: the theme, or the syntax-highlighting toggle, which recolors every
    * visible buffer at once the same way a theme change does.
    */
  private def chromeDamage(before: AppState, after: AppState): Damage =
    if before.theme != after.theme || before.config.syntaxHighlightingEnabled != after.config.syntaxHighlightingEnabled
    then Damage.Chrome
    else Damage.Nothing

  /** Transitions that touch every visible glyph rather than any one buffer's rows, matching what
    * `AppRuntime.needsFullContentRender` already treats as requiring a full canvas repaint: a theme transition
    * cross-fades every glyph and background colour in flight, and a surface animation composites through the same
    * full-render path as any other overlay (see that function's doc comment for why the window sitter alone is exempt
    * -- it never touches the canvas at all, so it contributes no damage here).
    */
  private def fullRenderDamage(before: AppState, after: AppState): Damage =
    if before.themeTransition != after.themeTransition || before.surfaceAnimations != after.surfaceAnimations then
      Damage.Everything
    else Damage.Nothing

  private def isSameReference(a: AnyRef, b: AnyRef): Boolean = a eq b
