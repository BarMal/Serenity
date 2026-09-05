package com.serenity.config

import scala.jdk.CollectionConverters.*

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions, ConfigValue, ConfigValueFactory}

/** A setting's value, in both the forms the config file needs it.
  *
  * `config` is the library's own representation, which decides whether a file carrying this setting can be read back.
  * `rendered` is the text the file carries, derived from that representation rather than written by hand -- quoting and
  * escaping are the library's problem, not this module's.
  */
final case class HoconValue(config: ConfigValue, rendered: String)

object HoconValue:

  /** The bare spelling where it is both readable and means the same thing, the library's quoted one otherwise.
    *
    * Most config values (`breathe`, `frosted`, `off`) read better unquoted, and that is the spelling this file has
    * always used. Two things have to hold for the quotes to go, and they answer different questions: the characters
    * decide whether a reader would find the bare form clearer, and parsing the bare form back decides whether it still
    * says the same thing. The second is what a character rule alone got wrong -- an unquoted comma turned a segment
    * list into a file the parser rejected, taking every other setting in it down to defaults.
    */
  def string(value: String): HoconValue =
    val quoted  = ConfigValueFactory.fromAnyRef(value).render(ConfigRenderOptions.concise())
    val legible = value.nonEmpty && value.forall(char => char.isLetterOrDigit || "_./-".contains(char))
    val faithful =
      legible && (try ConfigFactory.parseString(s"probe = $value").getString("probe") == value
      catch case _: Exception => false)
    HoconValue(ConfigValueFactory.fromAnyRef(value), if faithful then value else quoted)

  /** Numbers are written as Scala spells them; the library keeps the text it parsed, so the two agree.
    *
    * `fromAnyRef` would not: it renders a `Double` of 12 as `12`, and widens a `Float` of 0.18f to
    * `0.18000000715255737`.
    */
  def number(value: Double): HoconValue = numberText(value.toString)
  def number(value: Float): HoconValue  = numberText(value.toString)
  def number(value: Int): HoconValue    = numberText(value.toString)
  def number(value: Long): HoconValue   = numberText(value.toString)

  private def numberText(text: String): HoconValue =
    HoconValue(ConfigFactory.parseString(s"probe = $text").getValue("probe"), text)

  def boolean(value: Boolean): HoconValue =
    HoconValue(ConfigValueFactory.fromAnyRef(value), value.toString)

  /** List elements are always quoted: they are free text, and a bare one would run into the separator. */
  def list(values: List[String]): HoconValue =
    HoconValue(
      ConfigValueFactory.fromAnyRef(values.asJava),
      values
        .map(value => ConfigValueFactory.fromAnyRef(value).render(ConfigRenderOptions.concise()))
        .mkString("[", ", ", "]")
    )

  /** `auto` is how this file spells "no value set" for the settings that have such a state. */
  def optionalNumber(value: Option[Double]): HoconValue = value.fold(string("auto"))(number)
  def optionalCount(value: Option[Int]): HoconValue     = value.fold(string("auto"))(number)

  /** An empty string is how it spells the same thing for the ones that predate that convention. */
  def optionalString(value: Option[String]): HoconValue = string(value.getOrElse(""))
