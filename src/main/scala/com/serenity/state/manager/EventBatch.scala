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
  * @param isolate
  *   an event the caller must apply on its own; the batch stops before it and hands it back unapplied
  * @param slice
  *   how long the batch may keep applying inputs; it always applies at least one, then hands the rest back once this
  *   much time has passed, so the caller can publish what it has before carrying on
  */
final case class EventBatchSteps[A](
    decode: (AppState, A) => Event,
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

/** How a batch folds consecutive typed keys into one commit with the cursor centred once (#1985). `step` is the model a
  * key would commit before centring, or `None` for an event that must be applied on its own; `commit` centres and
  * commits a run from the model it started at. A run ends before any other event, so that event and the undo snapshot
  * it may record see the cursor centred, and after `maxKeys` keys.
  */
final private[serenity] case class TypedRuns(
    step: (Event, Model, Long) => Option[Model],
    commit: (Model, Model) => IO[Unit],
    maxKeys: Int
)

private[serenity] object TypedRuns:

  /** Below `IncrementalWrap.MaxEditChars` (128 characters): centring a longer run would hand the wrap cache one edit
    * too large to re-wrap from the paragraph's previous rows, and it would wrap the paragraph cold.
    */
  val MaxKeys = 96

  val none: TypedRuns = TypedRuns((_, _, _) => None, (_, _) => IO.unit, 1)

object EventBatch:

  final private case class TypedRun(start: Model, typed: Model, keys: Int)

  private[serenity] def applying[A](
    inputs: List[A],
    steps: EventBatchSteps[A],
    model: IO[Model],
    apply: Event => IO[Unit],
    typedRuns: TypedRuns = TypedRuns.none
  ): IO[EventBatch[A]] =
    def settle(run: Option[TypedRun]): IO[Unit] =
      run.fold(IO.unit)(pending => typedRuns.commit(pending.start, pending.typed))
    def extended(run: Option[TypedRun], current: Model, typed: Model): TypedRun =
      run.fold(TypedRun(current, typed, 1))(pending => pending.copy(typed = typed, keys = pending.keys + 1))
    def loop(
      remaining: List[A],
      applied: Vector[Event],
      run: Option[TypedRun],
      deadline: FiniteDuration
    ): IO[(Vector[Event], Option[Event], List[A])] =
      remaining match
        case Nil => settle(run).as((applied, None, Nil))
        case input :: rest =>
          IO.monotonic.flatMap { now =>
            if applied.nonEmpty && now >= deadline then settle(run).as((applied, None, remaining))
            else
              run.fold(model)(pending => IO.pure(pending.typed)).flatMap { current =>
                val event = steps.decode(current.app, input)
                if steps.isolate(event) then settle(run).as((applied, Some(event), rest))
                else
                  typedRuns.step(event, current, now.toNanos) match
                    case Some(typed) =>
                      val next = extended(run, current, typed)
                      if next.keys >= typedRuns.maxKeys then
                        settle(Some(next)) >> loop(rest, applied :+ event, None, deadline)
                      else loop(rest, applied :+ event, Some(next), deadline)
                    case None =>
                      settle(run) >> apply(event) >> loop(rest, applied :+ event, None, deadline)
              }
          }
    for
      before                         <- model
      start                          <- IO.monotonic
      (applied, isolated, remaining) <- loop(inputs, Vector.empty, None, start + steps.slice)
      after                          <- model
    yield EventBatch(applied.toList, before, after, isolated, remaining)
