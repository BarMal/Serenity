package com.serenity.ui.accessibility

import cats.effect.{IO, Ref}
import com.serenity.state.models.AppState

/** Memoizes the accessibility snapshot against the exact `AppState` reference last synced, so the O(document-size)
  * projection in `AccessibilitySnapshot.from` — including materializing each visible buffer's full content for the
  * document node — is paid once per distinct dispatched state instead of on every render frame. Render frames where the
  * state hasn't changed since the last sync (e.g. a caret-blink cursor-only tick between keystrokes) reuse the previous
  * snapshot untouched.
  */
final class AccessibilitySync private (ref: Ref[IO, Option[(AppState, AccessibilitySnapshot)]]):

  def sync(
    state: AppState
  )(compute: Option[AccessibilitySnapshot] => IO[AccessibilitySnapshot]): IO[AccessibilitySnapshot] =
    ref.get.flatMap {
      case Some((previousState, previousSnapshot)) if previousState eq state =>
        IO.pure(previousSnapshot)
      case previous =>
        compute(previous.map(_._2)).flatTap(snapshot => ref.set(Some(state -> snapshot)))
    }

object AccessibilitySync:
  def empty: IO[AccessibilitySync] =
    Ref.of[IO, Option[(AppState, AccessibilitySnapshot)]](None).map(new AccessibilitySync(_))
