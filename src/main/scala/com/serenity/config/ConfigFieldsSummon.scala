package com.serenity.config

import com.serenity.keystroke.Modifier
import io.circe.{Decoder, Encoder}

/** The keys that summon hidden chrome: the base modifiers, the lock key and one letter per element. */
private[config] object ConfigFieldsSummon:

  import ConfigFieldSyntax.*

  private val modifiers: FieldCodec[Set[Modifier]] =
    FieldCodec(
      SummonConfig.parseModifiers,
      value => HoconValue.string(SummonConfig.renderModifiers(value)),
      Encoder.encodeString.contramap(SummonConfig.renderModifiers),
      Decoder.decodeString.emap(text => SummonConfig.parseModifiers(text).toRight(s"Not a modifier chord: $text"))
    )

  private val lock: FieldCodec[HotkeyTrigger] =
    FieldCodec(
      SummonConfig.parseLock,
      value => HoconValue.string(SummonConfig.renderLock(value)),
      Encoder.encodeString.contramap(SummonConfig.renderLock),
      Decoder.decodeString.emap(text => SummonConfig.parseLock(text).toRight(s"Not a lock key: $text"))
    )

  private val letter: FieldCodec[Option[Char]] =
    FieldCodec(
      SummonConfig.parseLetter,
      value => HoconValue.string(SummonConfig.renderLetter(value)),
      Encoder.encodeString.contramap(SummonConfig.renderLetter),
      Decoder.decodeString.emap(text => SummonConfig.parseLetter(text).toRight(s"Not a letter or off: $text"))
    )

  private def elementLetter(element: ChromeElement): ConfigField[Option[Char]] =
    field(s"ui.summon.${element.configKey}")(letter)(
      _.summon.letterOf(element),
      (config, value) => config.withSummon(config.summon.withLetter(element, value))
    )

  val fields: List[ConfigField[?]] =
    List(
      field("ui.summon.modifiers")(modifiers)(
        _.summon.modifiers,
        (config, value) => config.withSummon(config.summon.copy(modifiers = value))
      ),
      field("ui.summon.lock")(lock)(
        _.summon.lock,
        (config, value) => config.withSummon(config.summon.copy(lock = value))
      )
    ) ++ ChromeElement.values.toList.map(elementLetter)
