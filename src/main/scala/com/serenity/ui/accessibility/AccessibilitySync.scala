package com.serenity.ui.accessibility

import cats.effect.{IO, Ref}
import com.serenity.state.models.{AppState, Buffer, BufferId, BufferMapChanges, TypingActivity}

/** Memoizes the accessibility snapshot against the `AppState` last synced, so the O(document-size) projection in
  * `AccessibilitySnapshot.from` — including materializing each visible buffer's full content for the document node — is
  * paid once per distinct *accessibility-relevant* state instead of on every render frame.
  *
  * A plain `AppState` reference check only catches the case where nothing at all was dispatched (e.g. a caret-blink
  * cursor-only tick). So a cache hit here is either an exact `AppState` match (cheapest), or a match on a normalized
  * view with known-irrelevant fields blanked out -- verified against `AccessibilityModel.scala` to read only `buffers`
  * (content/filePath/cursors), `focus`, `layout`, `uiSurfaces`, and `config`.
  */
final class AccessibilitySync private (ref: Ref[IO, Option[AccessibilitySync.CacheEntry]]):
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
      changed = (previous, buffer) => withoutPreviewGenerations(previous) != withoutPreviewGenerations(buffer)
    )

  private def withoutPreviewGenerations(buffer: Buffer): Buffer =
    buffer.copy(markdownPreviewEditGeneration = 0L, markdownPreviewCommittedGeneration = 0L)

  def empty: IO[AccessibilitySync] =
    Ref.of[IO, Option[CacheEntry]](None).map(new AccessibilitySync(_))
