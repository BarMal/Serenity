package com.serenity.state.manager

import scala.concurrent.duration.*

import cats.effect.{IO, Ref}

/** Folds a burst of requests for the same job into one run of it. The run waits [[WriteCoalescer.Debounce]], so a held
  * key or a slider drag is one run instead of one per repeat, and reads whatever it works on when it starts rather than
  * when it was asked for, so it cannot act on something older than the last request.
  */
final private[manager] class WriteCoalescer private (queued: Ref[IO, Boolean]):

  /** Submits `job` unless a submitted one has not started yet, in which case that one covers this request. */
  def request(submit: IO[Unit] => IO[Unit], job: IO[Unit]): IO[Unit] =
    queued
      .getAndSet(true)
      .flatMap(alreadyQueued =>
        IO.unlessA(alreadyQueued)(
          submit((IO.sleep(WriteCoalescer.Debounce) >> queued.set(false)).onCancel(queued.set(false)) >> job)
        )
      )

private[manager] object WriteCoalescer:

  /** Long enough to fold the repeats of a held key into one run, short enough to be done before anyone notices. */
  val Debounce: FiniteDuration = 100.millis

  def unsafe: WriteCoalescer = new WriteCoalescer(Ref.unsafe[IO, Boolean](false))
