package com.serenity.config

import java.util.Locale

import io.circe.{Decoder, Encoder, Json}

/** The space one side of the text area keeps free: a share of the width, a count of cells, a count of the prose font's
  * `0` advances (CSS's `ch`), or, on the left and right, whatever the wrap point leaves spare.
  */
enum Gutter:
  case Percent(share: Double)
  case Cells(count: Int)
  case Ch(count: Int)
  case Auto

object Gutter:

  val Zero: Gutter = Percent(0.0)

  private val Counted = raw"(\d+)\s*(cells?|ch)".r
  private val Shared  = raw"(\d+(?:\.\d+)?)\s*%?".r

  /** A bare number or `N%` is a percentage, as the setting always was; `Ncells`, `N cells` and `Nch` are counts, and
    * `auto` shares spare width. `allowAuto` is false on the top and bottom, and `allowCh` where `0` has no advance to
    * count along.
    */
  def parse(text: String, allowAuto: Boolean, allowCh: Boolean = true): Option[Gutter] =
    text.trim.toLowerCase(Locale.ROOT) match
      case "auto" => Option.when(allowAuto)(Auto)
      case Counted(digits, unit) =>
        digits.toIntOption.flatMap { count =>
          if unit == "ch" then Option.when(allowCh)(Ch(count)) else Some(Cells(count))
        }
      case Shared(number) =>
        number.toDoubleOption.filter(_ <= TextAreaInsets.MaxInset * 100.0).map(percent => Percent(fraction(percent)))
      case _ => None

  /** A percentage is written as the bare number it was always written as. */
  def render(gutter: Gutter): String =
    gutter match
      case Percent(share) => percentOf(share).toString
      case Cells(count)   => s"${count}cells"
      case Ch(count)      => s"${count}ch"
      case Auto           => "auto"

  /** A gutter as the settings dialog shows it: a percentage to one decimal, the other spellings as written. */
  def display(gutter: Gutter): String =
    gutter match
      case Percent(share) => f"${percentOf(share)}%.1f"
      case other          => render(other)

  /** What the accepted spellings are, for the message that names a refused value. */
  val Accepted: String = "a percentage (12.5 or 12.5%), a count of cells (12cells), a count of ch (4ch) or auto"

  /** A percentage in the file, a fraction in the config. The rounding matters: reading `17.3` back as a raw division
    * gives 0.17299999999999996, which then writes out as a different number than the one that was saved.
    */
  def fraction(percent: Double): Double =
    BigDecimal(percent / 100.0).setScale(9, BigDecimal.RoundingMode.HALF_UP).toDouble

  def percentOf(share: Double): Double =
    BigDecimal(share * 100.0).setScale(9, BigDecimal.RoundingMode.HALF_UP).toDouble

  /** What one setting's JSON holds: a percentage as the number it is in the file, the other spellings as their text. */
  val fieldEncoder: Encoder[Gutter] = Encoder.instance {
    case Percent(share) => Json.fromDoubleOrNull(percentOf(share))
    case other          => Json.fromString(render(other))
  }

  def fieldDecoder(allowAuto: Boolean, allowCh: Boolean): Decoder[Gutter] =
    Decoder.instance { cursor =>
      cursor
        .as[Double]
        .map(percent => Percent(fraction(percent)))
        .orElse(cursor.as[String].flatMap { text =>
          parse(text, allowAuto, allowCh).toRight(io.circe.DecodingFailure(s"Not a gutter: $text", cursor.history))
        })
    }

  /** In a session a percentage is the fraction it always was, and the other spellings are their text. */
  given Encoder[Gutter] = Encoder.instance {
    case Percent(share) => Json.fromDoubleOrNull(share)
    case other          => Json.fromString(render(other))
  }

  given Decoder[Gutter] = Decoder.instance { cursor =>
    cursor.as[Double] match
      case Right(share) => Right(Percent(share))
      case Left(_) =>
        cursor.as[String].flatMap { text =>
          parse(text, allowAuto = true).toRight(
            io.circe.DecodingFailure(s"Not a gutter: $text", cursor.history)
          )
        }
  }
