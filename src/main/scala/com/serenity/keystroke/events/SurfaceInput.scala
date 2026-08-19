package com.serenity.keystroke.events

/** How one focused surface interprets the shared [[FocusIntent]] vocabulary.
  *
  * Each instance is that surface's input policy stated in one place: the command runner turns `NextGroup` into moving
  * between categories, the modal into moving between fields, the pinned panel into handing focus back to the editor.
  * Declining an intent is part of the policy and is written as `None` rather than left to a catch-all, so `FocusIntent`
  * being sealed makes every instance account for every intent -- adding one to the vocabulary breaks compilation at
  * each surface that has not decided what it means.
  */
trait SurfaceInput[S]:

  /** What this surface does with an intent, or `None` if it does not accept it. */
  def fromIntent(intent: FocusIntent): Option[S]

object SurfaceInput:

  def apply[S](using instance: SurfaceInput[S]): SurfaceInput[S] = instance

  /** Reads the shared vocabulary out of a raw input event, or `None` when the event is not focus input at all.
    *
    * This is the single table that replaced the shared half of five hand-written ones. It lives here rather than on
    * `FocusIntent` deliberately: in that companion the enum's own `DeleteBackward`, `Paste` and friends would shadow
    * the identically-named events, so each pattern would match the intent it was meant to produce and every delete
    * would fall through to `None` -- which compiles cleanly and fails only at runtime.
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

  /** Translates a raw event for surface `S` via the shared vocabulary. Surfaces whose own events can arrive already
    * translated handle that passthrough themselves, ahead of calling this.
    */
  def translate[S](event: Event)(using instance: SurfaceInput[S]): Option[S] =
    intentOf(event).flatMap(instance.fromIntent)
