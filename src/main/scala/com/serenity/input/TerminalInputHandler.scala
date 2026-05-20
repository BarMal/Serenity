package com.serenity.input

import cats.effect.{Concurrent, Sync}
import com.googlecode.lanterna.input.KeyStroke
import com.googlecode.lanterna.screen.Screen
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.{Event, ResizeEvent}
import com.serenity.ui.layout.TerminalSize
import fs2.Stream

class ScreenInputHandler[F[_] : Sync : Concurrent, E <: Event](
    screen: Screen,
    inputRouter: InputRouter[F, E]
) extends InputHandler[F]:

  def keyStream: Stream[F, KeyStroke] =
    Stream
      .repeatEval(readKeyStroke)
      .unNone
      .filter(isValidKeyStroke)

  def keyStrokeInfoStream: Stream[F, KeyStrokeInfo] =
    keyStream.map(KeyStrokeInfo.fromKeyStroke)

  def eventStream: Stream[F, Event] =
    keyStreamEvents.mergeHaltR(resizeStreamEvents)

  private def keyStreamEvents: Stream[F, Event] =
    inputRouter.eventStream(keyStream)

  private def resizeStreamEvents: Stream[F, Event] =
    Stream
      .repeatEval(checkForResize)
      .unNone
      .map(ResizeEvent.apply)

  private def checkForResize: F[Option[TerminalSize]] =
    Sync[F].blocking {
      Option(screen.doResizeIfNecessary()).map { lanternaSize =>
        TerminalSize(lanternaSize.getColumns, lanternaSize.getRows)
      }
    }

  private def readKeyStroke: F[Option[KeyStroke]] =
    Sync[F].blocking {
      // Use readInput() to block until a key is pressed, avoiding busy-wait
      Option(screen.readInput())
    }

  private def isValidKeyStroke(keyStroke: KeyStroke): Boolean =
    keyStroke != null && keyStroke.getKeyType != null
