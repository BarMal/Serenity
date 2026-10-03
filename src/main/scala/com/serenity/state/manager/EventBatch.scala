package com.serenity.state.manager

import cats.effect.IO
import com.serenity.keystroke.events.Event
import com.serenity.state.models.AppState

/** How [[StateEngine.applyEventBatch]] turns its inputs into events.
  *
  * @param decode
  *   reads one input against the state the inputs ahead of it left, so a keystroke reaches whichever translator the
  *   focus they produced selects
  * @param prepare
  *   an update committed just ahead of an event, given the monotonic time in nanoseconds
  * @param isolate
  *   an event the caller must apply on its own; the batch stops before it and hands it back unapplied
  */
final case class EventBatchSteps[A](
    decode: (AppState, A) => Event,
    prepare: (Event, Long) => Option[AppState => AppState],
    isolate: Event => Boolean
)

/** What one [[StateEngine.applyEventBatch]] call applied, the models either side of it, and the isolated event it
  * stopped at together with the inputs after that event.
  */
final case class EventBatch[A](
    applied: List[Event],
    before: Model,
    after: Model,
    isolated: Option[(Event, List[A])]
)

object EventBatch:

  private[serenity] def applying[A](
    inputs: List[A],
    steps: EventBatchSteps[A],
    model: IO[Model],
    commit: (AppState => AppState) => IO[Unit],
    apply: Event => IO[Unit]
  ): IO[EventBatch[A]] =
    def loop(remaining: List[A], applied: Vector[Event]): IO[(Vector[Event], Option[(Event, List[A])])] =
      remaining match
        case Nil => IO.pure((applied, None))
        case input :: rest =>
          model.flatMap { current =>
            val event = steps.decode(current.app, input)
            if steps.isolate(event) then IO.pure((applied, Some((event, rest))))
            else
              IO.monotonic.flatMap(now => steps.prepare(event, now.toNanos).fold(IO.unit)(commit)) >>
                apply(event) >> loop(rest, applied :+ event)
          }
    for
      before              <- model
      (applied, isolated) <- loop(inputs, Vector.empty)
      after               <- model
    yield EventBatch(applied.toList, before, after, isolated)
