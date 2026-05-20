package com.serenity.input

import cats.FlatMap
import cats.effect.{Ref, Sync}
import cats.syntax.functor.*
import com.googlecode.lanterna.input.KeyStroke
import com.serenity.keystroke.events.Event
import com.serenity.keystroke.translators.Translator
import fs2.Stream

trait InputRouter[F[_], E <: Event]:
  def eventStream(keyStream: Stream[F, KeyStroke]): Stream[F, Event]
  def setActiveTranslator(translator: Translator[E]): F[Unit]
  def getActiveTranslator: F[Translator[E]]

class InputRouterImpl[F[_] : FlatMap, E <: Event](
    activeTranslatorRef: Ref[F, Translator[E]]
) extends InputRouter[F, E]:

  def eventStream(keyStream: Stream[F, KeyStroke]): Stream[F, Event] =
    keyStream.evalMap(translateKeyStroke)

  def setActiveTranslator(translator: Translator[E]): F[Unit] =
    activeTranslatorRef.set(translator)

  def getActiveTranslator: F[Translator[E]] =
    activeTranslatorRef.get

  private def translateKeyStroke(keyStroke: KeyStroke): F[Event] =
    for
      translator <- activeTranslatorRef.get
      event = translator.translate(keyStroke)
    yield event

object InputRouter:

  def create[F[_] : Sync, E <: Event](initialTranslator: Translator[E]): F[InputRouter[F, E]] =
    for translatorRef <- Ref.of[F, Translator[E]](initialTranslator)
    yield new InputRouterImpl[F, E](translatorRef)
