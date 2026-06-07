package com.serenity.input

import fs2.Stream

import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.Event

trait InputHandler[F[_]]:
  def keyStrokeInfoStream: Stream[F, KeyStrokeInfo]
  def eventStream: Stream[F, Event]
  def shutdown: F[Unit]
