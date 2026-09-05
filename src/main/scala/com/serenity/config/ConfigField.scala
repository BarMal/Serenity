package com.serenity.config

import scala.jdk.CollectionConverters.*

import com.typesafe.config.{ConfigList, ConfigValue, ConfigValueType}
import io.circe.{Decoder, Encoder, HCursor, Json}

/** How one setting's value crosses each boundary it has to cross.
  *
  * A setting is written to the config file, read back from it, written to session state and read back from that. Four
  * conversions, and until they were declared together they were four separate pieces of code per setting -- which is
  * how settings came to exist in one direction and not the other.
  */
final case class FieldCodec[A](
    parse: String => Option[A],
    render: A => HoconValue,
    encoder: Encoder[A],
    decoder: Decoder[A],
    readList: Option[List[String] => Option[A]] = None
):
  /** Read straight from the parsed file rather than from a flattened rendering of it.
    *
    * A list is the case that needs it: flattening `["a,b", "c"]` to text loses where one element ends and the next
    * begins, and joining on a comma is what made an element containing a comma unreadable.
    */
  def fromConfigValue(value: ConfigValue): Option[A] =
    (readList, value.valueType) match
      case (Some(read), ConfigValueType.LIST) =>
        value match
          case list: ConfigList => read(list.asScala.toList.map(_.unwrapped.toString))
          case _                => None
      case _ => parse(FieldCodec.flatten(value))

object FieldCodec:

  def of[A](parse: String => Option[A], render: A => HoconValue)(using
    encoder: Encoder[A],
    decoder: Decoder[A]
  ): FieldCodec[A] = FieldCodec(parse, render, encoder, decoder)

  val boolean: FieldCodec[Boolean] = of(parseBoolean, HoconValue.boolean)

  val string: FieldCodec[String] = of(text => Some(text.trim), HoconValue.string)

  val int: FieldCodec[Int] = of(_.trim.toIntOption, HoconValue.number)

  val long: FieldCodec[Long] = of(_.trim.toLongOption, HoconValue.number)

  val double: FieldCodec[Double] = of(_.trim.toDoubleOption, HoconValue.number)

  val float: FieldCodec[Float] = of(_.trim.toFloatOption, HoconValue.number)

  val stringList: FieldCodec[List[String]] =
    of(text => Some(splitList(text)), HoconValue.list).copy(readList = Some(Some.apply))

  /** A list of enum values written as one comma-joined string, with `off` for the empty list. */
  def commaSeparated[A](fromKey: String => Option[A], toKey: A => String): FieldCodec[List[A]] =
    given Encoder[List[A]] = Encoder.encodeList(Encoder.encodeString.contramap(toKey))
    given Decoder[List[A]] =
      Decoder.decodeList(Decoder.decodeString.emap(key => fromKey(key).toRight(s"Unknown value: $key")))
    of(
      text =>
        val trimmed = text.trim.toLowerCase
        if trimmed == "off" || trimmed.isEmpty then Some(Nil)
        else
          val parsed = splitList(text).map(fromKey)
          Option.when(parsed.forall(_.isDefined))(parsed.flatten)
      ,
      values => HoconValue.string(if values.isEmpty then "off" else values.map(toKey).mkString(","))
    )

  /** An enum spelled by its own config key, which is how every enum in this format is written. */
  def enumerated[A](fromKey: String => Option[A], toKey: A => String): FieldCodec[A] =
    given Encoder[A] = Encoder.encodeString.contramap(toKey)
    given Decoder[A] = Decoder.decodeString.emap(key => fromKey(key).toRight(s"Unknown value: $key"))
    of(text => fromKey(text.trim), value => HoconValue.string(toKey(value)))

  /** Refuse a value the config would only clamp or ignore, rather than storing something the file did not say. */
  extension [A](codec: FieldCodec[A])
    def filtered(predicate: A => Boolean): FieldCodec[A] =
      codec.copy(parse = text => codec.parse(text).filter(predicate))

    def mapped[B](to: A => B, from: B => A): FieldCodec[B] =
      FieldCodec(
        parse = text => codec.parse(text).map(to),
        render = value => codec.render(from(value)),
        encoder = codec.encoder.contramap(from),
        decoder = codec.decoder.map(to)
      )

    /** `auto` is how this format spells "no value set" for a setting that has such a state. */
    def orAuto: FieldCodec[Option[A]] =
      given Encoder[Option[A]] = Encoder.encodeOption(using codec.encoder)
      given Decoder[Option[A]] = Decoder.decodeOption(using codec.decoder)
      of(
        text => if isAbsent(text) then Some(None) else codec.parse(text).map(Some.apply),
        value => value.fold(HoconValue.string("auto"))(codec.render)
      )

    /** An empty string is how the settings that predate `auto` spell the same thing. */
    def orEmpty: FieldCodec[Option[A]] =
      given Encoder[Option[A]] = Encoder.encodeOption(using codec.encoder)
      given Decoder[Option[A]] = Decoder.decodeOption(using codec.decoder)
      of(
        text => if text.trim.isEmpty then Some(None) else codec.parse(text).map(Some.apply),
        value => value.fold(HoconValue.string(""))(codec.render)
      )

  /** How a parsed value reads as text, matching what the file's own flattening produces. */
  def flatten(value: ConfigValue): String =
    value match
      case list: ConfigList => list.asScala.map(_.unwrapped.toString).mkString(",")
      case other            => other.unwrapped.toString

  private def isAbsent(text: String): Boolean =
    val trimmed = text.trim.toLowerCase
    trimmed.isEmpty || trimmed == "auto" || trimmed == "default"

  private def splitList(text: String): List[String] =
    text.split(',').toList.map(_.trim).filter(_.nonEmpty)

  def parseBoolean(text: String): Option[Boolean] =
    text.trim.toLowerCase match
      case "true" | "on" | "enabled"    => Some(true)
      case "false" | "off" | "disabled" => Some(false)
      case _                            => None

/** One setting, declared once for every direction it travels in.
  *
  * The lens (`get`/`set`) and the [[FieldCodec]] together are enough to write the setting to the config file, read it
  * back, write it to session state and read it back -- so a setting cannot be persisted in one direction and dropped in
  * another, which is the failure this exists to make impossible.
  *
  * `aliases` are earlier spellings of the same key that are still read. They are never written.
  */
final case class ConfigField[A](
    key: String,
    aliases: Set[String],
    codec: FieldCodec[A],
    get: AppConfig => A,
    set: (AppConfig, A) => AppConfig,
    jsonKey: Option[String] = None
):
  /** The session-state key, which is the config key unless an already-written session file used a different one. */
  def sessionKey: String = jsonKey.getOrElse(key)

  def spellings: Set[String] = aliases + key

  def setting(config: AppConfig): (String, HoconValue) = key -> codec.render(get(config))

  def read(config: AppConfig, text: String): Option[AppConfig] = codec.parse(text).map(set(config, _))

  def readValue(config: AppConfig, value: ConfigValue): Option[AppConfig] =
    codec.fromConfigValue(value).map(set(config, _))

  def encode(config: AppConfig): (String, Json) = sessionKey -> codec.encoder(get(config))

  /** Session state falls back to whatever the caller already has rather than failing the whole restore, so a file
    * written before this field existed keeps every other setting in it.
    */
  def decode(cursor: HCursor, config: AppConfig): AppConfig =
    cursor.downField(sessionKey).as(using codec.decoder).fold(_ => config, value => set(config, value))

/** A key that is only ever read.
  *
  * Some spellings set more than one field (`font.size` sets both the code and the text size) or were replaced by a
  * different shape entirely. They stay readable so old files keep working, but nothing writes them, so they have no
  * lens and no session-state form.
  */
final case class LegacyConfigKey(
    spellings: Set[String],
    read: (AppConfig, String) => Option[AppConfig]
)
