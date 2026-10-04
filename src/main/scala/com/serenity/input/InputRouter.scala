package com.serenity.input

import java.util.concurrent.atomic.AtomicBoolean

import cats.FlatMap
import cats.effect.{Ref, Sync}
import cats.syntax.functor.*
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.Event
import com.serenity.keystroke.translators.Translator
import fs2.Stream

trait InputRouter[F[_], E <: Event]:
  def eventStream(infoStream: Stream[F, KeyStrokeInfo]): Stream[F, Event]
  def setActiveTranslator(translator: Translator[E]): F[Unit]
  def getActiveTranslator: F[Translator[E]]

  // Not a `Ref`: the AWT listeners that ask run on the event dispatch thread, outside any effect.
  private val cursorPeekSignals = new AtomicBoolean(false)

  /** Whether the config input is translated against has the cursor-peek prototype on, so its raw gesture signals are
    * worth dispatching (#1845).
    */
  def cursorPeekEnabled: Boolean = cursorPeekSignals.get()

  def setCursorPeekEnabled(enabled: Boolean)(using Sync[F]): F[Unit] = Sync[F].delay(cursorPeekSignals.set(enabled))

class InputRouterImpl[F[_] : FlatMap, E <: Event](
    activeTranslatorRef: Ref[F, Translator[E]]
) extends InputRouter[F, E]:

  def eventStream(infoStream: Stream[F, KeyStrokeInfo]): Stream[F, Event] =
    infoStream.evalMap(translate)

  def setActiveTranslator(translator: Translator[E]): F[Unit] =
    activeTranslatorRef.set(translator)

  def getActiveTranslator: F[Translator[E]] =
    activeTranslatorRef.get

  private def translate(info: KeyStrokeInfo): F[Event] =
    for
      translator <- activeTranslatorRef.get
      event = translator.translate(info)
    yield event

object InputRouter:

  def create[F[_] : Sync, E <: Event](initialTranslator: Translator[E]): F[InputRouter[F, E]] =
    for translatorRef <- Ref.of[F, Translator[E]](initialTranslator)
    yield new InputRouterImpl[F, E](translatorRef)
