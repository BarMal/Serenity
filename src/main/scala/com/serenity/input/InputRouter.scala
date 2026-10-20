package com.serenity.input

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicReference}

import cats.FlatMap
import cats.effect.{Ref, Sync}
import cats.syntax.functor.*
import com.serenity.config.SummonConfig
import com.serenity.keystroke.events.{ChromeSummonToggled, Event, InsertChar, UnhandledEvent}
import com.serenity.keystroke.translators.Translator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
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

  // Not a `Ref` either: the window reads the chord on every key press, on the event dispatch thread.
  private val summonSettings = new AtomicReference[SummonConfig](SummonConfig())

  /** The keys that summon chrome, as the config in use sets them. */
  def summonConfig: SummonConfig = summonSettings.get()

  def setSummonConfig(config: SummonConfig)(using Sync[F]): F[Unit] = Sync[F].delay(summonSettings.set(config))

class InputRouterImpl[F[_] : FlatMap, E <: Event](
    activeTranslatorRef: Ref[F, Translator[E]]
) extends InputRouter[F, E]:

  def eventStream(infoStream: Stream[F, KeyStrokeInfo]): Stream[F, Event] =
    infoStream.evalMap(translate)

  def setActiveTranslator(translator: Translator[E]): F[Unit] =
    activeTranslatorRef.set(translator)

  def getActiveTranslator: F[Translator[E]] =
    activeTranslatorRef.get

  // Not a `Ref`: the swallow window is set and read by the one stream that translates keys, and costs nothing per key.
  private val typedCharacterSwallowedUntil = new AtomicLong(0L)

  private def translate(info: KeyStrokeInfo): F[Event] =
    activeTranslatorRef.get.map { translator =>
      // The window consumes a summon chord itself, so one that arrives here is a terminal's: one keystroke, no
      // release to follow.
      summonConfig.elementFor(info) match
        case Some(element) => ChromeSummonToggled(element)
        case None          => translated(translator, info)
    }

  private def translated(translator: Translator[E], info: KeyStrokeInfo): Event =
    val event = translator.translate(info)
    if InputRouterImpl.isAltShiftChord(info) then
      event match
        case _: InsertChar | _: UnhandledEvent[?] =>
          typedCharacterSwallowedUntil.set(0L)
          UnhandledEvent(info, translator)
        case taken =>
          typedCharacterSwallowedUntil.set(System.nanoTime() + InputRouterImpl.ChordCharacterWindowNanos)
          taken
    else if InputRouterImpl.isTypedCharacter(info) && System.nanoTime() < typedCharacterSwallowedUntil.getAndSet(0L)
    then UnhandledEvent(info, translator)
    else
      typedCharacterSwallowedUntil.set(0L)
      event

/** A Shift+Alt letter chord arrives twice: as the chord when the key goes down and as the character the OS typed for it
  * a moment later. A hotkey that takes the chord also takes that character, and a chord nothing takes is not text, so
  * what the OS typed (a macOS Option+Shift glyph, say) is left to type.
  */
private object InputRouterImpl:

  val ChordCharacterWindowNanos: Long = 200_000_000L

  def isAltShiftChord(info: KeyStrokeInfo): Boolean =
    info.keyType == InputKey.Character && info.modifiers == Set(Modifier.Alt, Modifier.Shift) &&
      info.character.exists(char => char >= 'a' && char <= 'z')

  def isTypedCharacter(info: KeyStrokeInfo): Boolean =
    info.keyType == InputKey.Character && info.modifiers.isEmpty && info.character.isDefined

object InputRouter:

  def create[F[_] : Sync, E <: Event](initialTranslator: Translator[E]): F[InputRouter[F, E]] =
    for translatorRef <- Ref.of[F, Translator[E]](initialTranslator)
    yield new InputRouterImpl[F, E](translatorRef)
