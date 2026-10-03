package com.serenity.state.manager

import scala.concurrent.duration.FiniteDuration

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
  * @param slice
  *   how long the batch may keep applying inputs; it always applies at least one, then hands the rest back once this
  *   much time has passed, so the caller can publish what it has before carrying on
  */
final case class EventBatchSteps[A](
    decode: (AppState, A) => Event,
    prepare: (Event, Long) => Option[AppState => AppState],
    isolate: Event => Boolean,
    slice: FiniteDuration
)

/** What one [[StateEngine.applyEventBatch]] call applied and the models either side of it. `remaining` is every input
  * it left unapplied: those after `isolated` when it stopped at an isolated event, otherwise those its time slice had
  * no room for.
  */
final case class EventBatch[A](
    applied: List[Event],
    before: Model,
    after: Model,
    isolated: Option[Event],
    remaining: List[A]
)

object EventBatch:

  private[serenity] def applying[A](
    inputs: List[A],
    steps: EventBatchSteps[A],
    model: IO[Model],
    commit: (AppState => AppState) => IO[Unit],
    apply: Event => IO[Unit]
  ): IO[EventBatch[A]] =
    def loop(
      remaining: List[A],
      applied: Vector[Event],
      deadline: FiniteDuration
    ): IO[(Vector[Event], Option[Event], List[A])] =
      remaining match
        case Nil => IO.pure((applied, None, Nil))
        case input :: rest =>
          IO.monotonic.flatMap { now =>
            if applied.nonEmpty && now >= deadline then IO.pure((applied, None, remaining))
            else
              model.flatMap { current =>
                val event = steps.decode(current.app, input)
                if steps.isolate(event) then IO.pure((applied, Some(event), rest))
                else
                  steps.prepare(event, now.toNanos).fold(IO.unit)(commit) >>
                    apply(event) >> loop(rest, applied :+ event, deadline)
              }
          }
    for
      before                         <- model
      start                          <- IO.monotonic
      (applied, isolated, remaining) <- loop(inputs, Vector.empty, start + steps.slice)
      after                          <- model
    yield EventBatch(applied.toList, before, after, isolated, remaining)
