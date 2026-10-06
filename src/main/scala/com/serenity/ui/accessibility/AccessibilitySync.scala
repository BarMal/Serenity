package com.serenity.ui.accessibility

import cats.effect.{IO, Ref}
import com.serenity.markdown.MarkdownPreviewCache
import com.serenity.state.models.{AppState, TypingActivity}

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
  import AccessibilitySync.{CacheEntry, normalize}

  def sync(
    state: AppState
  )(compute: Option[AccessibilitySnapshot] => IO[AccessibilitySnapshot]): IO[AccessibilitySnapshot] =
    ref.get.flatMap {
      case Some(entry) if entry.rawState eq state =>
        IO.pure(entry.snapshot)
      case Some(entry) if entry.normalizedState == normalize(state) =>
        ref.set(Some(entry.copy(rawState = state))).as(entry.snapshot)
      case previous =>
        compute(previous.map(_.snapshot)).flatTap { snapshot =>
          ref.set(Some(CacheEntry(state, normalize(state), snapshot)))
        }
    }

object AccessibilitySync:

  final private[accessibility] case class CacheEntry(
      rawState: AppState,
      normalizedState: AppState,
      snapshot: AccessibilitySnapshot
  )

  /** Blanks the fields that tick on their own but are never read when projecting the accessibility snapshot. */
  private[accessibility] def normalize(state: AppState): AppState =
    state.copy(
      persisted = state.persisted.copy(
        buffers = state.persisted.buffers.view
          .mapValues(buffer =>
            buffer.copy(
              markdownPreviewEditGeneration = 0L,
              markdownPreviewCommittedGeneration = 0L
            )
          )
          .toMap
      ),
      runtime = state.runtime.copy(
        typingActivity = TypingActivity.idle
      )
    )

  def empty: IO[AccessibilitySync] =
    Ref.of[IO, Option[CacheEntry]](None).map(new AccessibilitySync(_, MarkdownPreviewCache()))
