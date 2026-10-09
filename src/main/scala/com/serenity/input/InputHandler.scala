package com.serenity.input

import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.Event
import fs2.{Chunk, Stream}

trait InputHandler[F[_]]:
  def keyStrokeInfoStream: Stream[F, KeyStrokeInfo]
  def eventStream: Stream[F, Event]
  def shutdown: F[Unit]

  /** Everything queued each time the consumer asks, oldest first. This default hands over one already-translated event
    * at a time.
    */
  def inputBatches: Stream[F, Chunk[PendingInput]] =
    eventStream.map(event => Chunk.singleton(PendingInput.Ready(event)))
