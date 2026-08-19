package com.serenity.keystroke.events

/** How one focused surface interprets the shared [[FocusIntent]] vocabulary.
  *
  * Declining is written as `None` rather than left to a catch-all, so a new intent breaks compilation at every surface
  * that has not decided what it means.
  */
trait SurfaceInput[S]:

  def fromIntent(intent: FocusIntent): Option[S]

object SurfaceInput:

  def apply[S](using instance: SurfaceInput[S]): SurfaceInput[S] = instance

  /** Lives here, not in `FocusIntent`'s companion: there the enum's own `DeleteBackward`, `Paste` and friends shadow
    * the identically-named events, so every delete falls through to `None` -- and it compiles.
    */
  def intentOf(event: Event): Option[FocusIntent] =
    event match
      case InsertChar(char)   => Some(FocusIntent.Insert(char))
      case DeleteBackward     => Some(FocusIntent.DeleteBackward)
      case DeleteForward      => Some(FocusIntent.DeleteForward)
      case DeleteWordBackward => Some(FocusIntent.DeleteWordBackward)
      case DeleteWordForward  => Some(FocusIntent.DeleteWordForward)
      case Paste              => Some(FocusIntent.Paste)
      case MoveUp             => Some(FocusIntent.Navigate(Direction.Up))
      case MoveDown           => Some(FocusIntent.Navigate(Direction.Down))
      case MoveLeft           => Some(FocusIntent.Navigate(Direction.Left))
      case MoveRight          => Some(FocusIntent.Navigate(Direction.Right))
      case TabKey             => Some(FocusIntent.NextGroup)
      case ReverseTabKey      => Some(FocusIntent.PreviousGroup)
      case Enter | NewLine    => Some(FocusIntent.Submit)
      case Escape             => Some(FocusIntent.Dismiss)
      case _                  => None

  /** Surfaces whose own events can arrive already translated handle that passthrough before calling this. */
  def translate[S](event: Event)(using instance: SurfaceInput[S]): Option[S] =
    intentOf(event).flatMap(instance.fromIntent)
