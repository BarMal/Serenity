package com.serenity.ui.accessibility

import cats.effect.{IO, Ref}
import com.serenity.markdown.MarkdownPreviewCache
import com.serenity.state.models.{AppState, Buffer, BufferId, BufferMapChanges, TypingActivity, ViewportPlacement}

/** Memoizes the accessibility snapshot against the `AppState` last synced, so the projection in
  * `AccessibilitySnapshot.from` is paid once per distinct *accessibility-relevant* state instead of on every render
  * frame. The projection holds each document as its `Rope` rather than a copy of its text, so neither it nor the
  * comparisons here read document text.
  *
  * A plain `AppState` reference check only catches the case where nothing at all was dispatched (e.g. a caret-blink
  * cursor-only tick). So a cache hit here is either an exact `AppState` match (cheapest), or a match on a normalized
  * view with known-irrelevant fields blanked out -- verified against `AccessibilityModel.scala` to read only `buffers`
  * (content/filePath/cursors), `focus`, `layout`, `uiSurfaces`, and `config`.
  */
final class AccessibilitySync private (
    ref: Ref[IO, Option[AccessibilitySync.CacheEntry]],
    val previewCache: MarkdownPreviewCache
):
  import AccessibilitySync.{CacheEntry, accessiblyEqual}

  def sync(
    state: AppState
  )(compute: Option[AccessibilitySnapshot] => IO[AccessibilitySnapshot]): IO[AccessibilitySnapshot] =
    ref.get.flatMap {
      case Some(entry) if entry.rawState eq state =>
        IO.pure(entry.snapshot)
      case Some(entry) if accessiblyEqual(entry.rawState, state) =>
        ref.set(Some(entry.copy(rawState = state))).as(entry.snapshot)
      case previous =>
        compute(previous.map(_.snapshot)).flatTap(snapshot => ref.set(Some(CacheEntry(state, snapshot))))
    }

object AccessibilitySync:

  final private[accessibility] case class CacheEntry(
      rawState: AppState,
      snapshot: AccessibilitySnapshot
  )

  /** Whether two states project to the same snapshot: equal once the fields that tick on their own but are never read
    * (`typingActivity`, the markdown preview generations) are set aside. Buffers held as the same object in both are
    * not compared, so the cost does not grow with the number of open buffers.
    */
  private[serenity] def accessiblyEqual(before: AppState, after: AppState): Boolean =
    (before eq after) ||
      (before.runtime.copy(typingActivity = TypingActivity.idle) ==
        after.runtime.copy(typingActivity = TypingActivity.idle) &&
        before.persisted.copy(buffers = Map.empty) == after.persisted.copy(buffers = Map.empty) &&
        sameBuffers(before.persisted.buffers, after.persisted.buffers))

  private def sameBuffers(before: Map[BufferId, Buffer], after: Map[BufferId, Buffer]): Boolean =
    before.size == after.size && !BufferMapChanges.anyChanged(before, after)(
      added = _ => true,
      changed = (previous, buffer) => withoutUnreadFields(previous) != withoutUnreadFields(buffer)
    )

  private def withoutUnreadFields(buffer: Buffer): Buffer =
    buffer.copy(
      markdownPreviewEditGeneration = 0L,
      markdownPreviewCommittedGeneration = 0L,
      viewport = buffer.viewport.copy(placement = ViewportPlacement.Placed)
    )

  def empty: IO[AccessibilitySync] =
    Ref.of[IO, Option[CacheEntry]](None).map(new AccessibilitySync(_, MarkdownPreviewCache()))
