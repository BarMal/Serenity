package com.serenity.lsp.model

import io.circe.Json

/** How a server counts the `character` of a `Position` (LSP 3.17 §3.17.1, `PositionEncodingKind`). Serenity's text is
  * UTF-16 (`docs/text-indexing.md`), so only [[Utf16]] positions can be used as they arrive.
  */
enum PositionEncoding:
  case Utf8, Utf16, Utf32
  case Other(kind: String)

  def label: String = this match
    case Utf8        => "utf-8"
    case Utf16       => "utf-16"
    case Utf32       => "utf-32"
    case Other(kind) => kind

object PositionEncoding:

  /** What the client offers in `general.positionEncodings`: the one it can use. */
  val offered: List[PositionEncoding] = List(Utf16)

  /** `capabilities.positionEncoding` of the `initialize` result. A server that names none uses UTF-16 (§3.17.1). */
  def fromInitializeResult(result: Json): PositionEncoding =
    result.hcursor.downField("capabilities").downField("positionEncoding").as[String] match
      case Right("utf-8")  => Utf8
      case Right("utf-16") => Utf16
      case Right("utf-32") => Utf32
      case Right(other)    => Other(other)
      case Left(_)         => Utf16
