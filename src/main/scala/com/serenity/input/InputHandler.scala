package com.serenity.input

import cats.effect.Sync
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.googlecode.lanterna.terminal.Terminal
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.Event
import fs2.Stream

trait InputHandler[F[_]]:
  def keyStream: Stream[F, KeyStroke]
  def keyStrokeInfoStream: Stream[F, KeyStrokeInfo]
  def eventStream: Stream[F, Event]

class InputHandlerImpl[F[_] : Sync, E <: Event](
    terminal: Terminal,
    inputRouter: InputRouter[F, E]
) extends InputHandler[F]:

  def keyStream: Stream[F, KeyStroke] =
    keyStreamUntilClosed

  private def keyStreamUntilClosed: Stream[F, KeyStroke] =
    Stream.eval(readKeyStroke).flatMap {
      case Some(keyStroke) if isValidKeyStroke(keyStroke) =>
        Stream.emit(keyStroke) ++ keyStreamUntilClosed
      case Some(_) =>
        keyStreamUntilClosed
      case None =>
        Stream.emit(new KeyStroke(KeyType.EOF))
    }

  def keyStrokeInfoStream: Stream[F, KeyStrokeInfo] =
    keyStream.map(KeyStrokeInfo.fromKeyStroke)

  def eventStream: Stream[F, Event] =
    inputRouter.eventStream(keyStream)

  private def readKeyStroke: F[Option[KeyStroke]] =
    Sync[F].blocking {
      Option(terminal.readInput())
    }

  private def isValidKeyStroke(keyStroke: KeyStroke): Boolean =
    keyStroke != null && keyStroke.getKeyType != null
