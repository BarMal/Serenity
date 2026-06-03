package com.serenity.input

import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.Event
import fs2.Stream

trait InputHandler[F[_]]:
  def keyStrokeInfoStream: Stream[F, KeyStrokeInfo]
  def eventStream: Stream[F, Event]
