package com.serenity.ui.accessibility

import cats.effect.{IO, Ref}
import com.serenity.animation.WindowSitter
import com.serenity.state.models.AppState

/** Memoizes the accessibility snapshot against the `AppState` last synced, so the O(document-size) projection in
  * `AccessibilitySnapshot.from` — including materializing each visible buffer's full content for the document node — is
  * paid once per distinct *accessibility-relevant* state instead of on every render frame.
  *
  * A plain `AppState` reference check only catches the case where nothing at all was dispatched (e.g. a caret-blink
  * cursor-only tick with no pending animation). It's defeated the moment a decorative *state* animation is active
  * (window sitter, theme transition, surface fade), since advancing those always produces a new top-level `AppState`
  * even though none of them are read by `AccessibilitySnapshot.from`. Per-character reveal animations don't have this
  * problem at all -- `AnimationState` lives in a `StateManager`-owned side table (`#1001`), not on `AppState`/`Buffer`,
  * so advancing them never invalidates this cache's `eq` check in the first place. So a cache hit here is either an
  * exact `AppState` match (cheapest), or a match on a normalized view with those known-irrelevant fields blanked out --
  * verified against `AccessibilityModel.scala` to read only `buffers` (content/filePath/cursors), `focus`, `layout`,
  * `uiSurfaces`, and `config`.
  */
final class AccessibilitySync private (ref: Ref[IO, Option[AccessibilitySync.CacheEntry]]):
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

  /** Blanks the fields ticked by decorative animations but never read when projecting the accessibility snapshot. */
  private[accessibility] def normalize(state: AppState): AppState =
    state.copy(
      buffers = state.buffers.view
        .mapValues(buffer =>
          buffer.copy(
            markdownPreviewEditGeneration = 0L,
            markdownPreviewCommittedGeneration = 0L
          )
        )
        .toMap,
      windowSitter = WindowSitter.default,
      themeTransition = None,
      surfaceAnimations = Map.empty
    )

  def empty: IO[AccessibilitySync] =
    Ref.of[IO, Option[CacheEntry]](None).map(new AccessibilitySync(_))
