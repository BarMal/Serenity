package com.serenity.config

import java.util.Locale

import io.circe.{Decoder, DecodingFailure, Encoder, Json}

/** Where a line wraps inside the text area: at its edge, after a count of cells, or after a count of the prose font's
  * `0` advances (CSS's `ch`). With `auto` gutters a set column is centred; with fixed gutters it is left-aligned. In
  * column mode it is the width of one column.
  */
enum WrapColumn:
  case Edge
  case Cells(count: Int)
  case Ch(count: Int)

object WrapColumn:

  val MinCells: Int = 1
  val MaxCells: Int = 400
  val MinCh: Int    = 20
  val MaxCh: Int    = 200

  private val Counted = raw"(\d+)\s*(cells?|ch)?".r

  /** `edge`, `unlimited` and `off` are the edge; a bare number, `Ncells` and `N cells` are cells; `Nch` is ch. A count
    * outside its range is refused rather than clamped.
    */
  def parse(text: String): Option[WrapColumn] =
    text.trim.toLowerCase(Locale.ROOT) match
      case "edge" | "unlimited" | "off" => Some(Edge)
      case Counted(digits, unit) =>
        digits.toIntOption.flatMap { count =>
          if unit == "ch" then Option.when(count >= MinCh && count <= MaxCh)(Ch(count))
          else Option.when(count >= MinCells && count <= MaxCells)(Cells(count))
        }
      case _ => None

  /** A cell count as the bare number it is written as. */
  def render(wrapColumn: WrapColumn): String =
    wrapColumn match
      case Edge         => "edge"
      case Cells(count) => count.toString
      case Ch(count)    => s"${count}ch"

  /** What the accepted spellings are, for the message that names a refused value. */
  val Accepted: String = "edge, a count of cells (80, 1 to 400) or a count of ch (66ch, 20 to 200)"

  /** One setting's JSON, in the config file and in a session: a cell count as the number it is, the rest as text. */
  given Encoder[WrapColumn] = Encoder.instance {
    case Cells(count) => Json.fromInt(count)
    case other        => Json.fromString(render(other))
  }

  given Decoder[WrapColumn] = Decoder.instance { cursor =>
    cursor
      .as[Int]
      .map(count => parse(count.toString))
      .orElse(cursor.as[String].map(parse))
      .flatMap(_.toRight(DecodingFailure(s"Not a wrap column: ${cursor.value.noSpaces}", cursor.history)))
  }
