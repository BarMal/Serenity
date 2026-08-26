package com.serenity.input

import java.nio.charset.StandardCharsets
import java.util.Base64

/** OSC 52 clipboard-write escape sequence: `ESC ] 52 ; c ; <base64> BEL`. Terminals that support the sequence at all
  * enforce a payload cap (client- or terminal-side, commonly tens of kilobytes); this never truncates a selection to
  * fit -- a payload over the cap is reported so the caller can fall back to another clipboard strategy instead of
  * silently losing part of what was copied.
  */
object Osc52:

  val DefaultMaxEncodedBytes = 100_000

  private val Esc = 0x1b.toChar
  private val Bel = 0x07.toChar

  final case class PayloadTooLarge(encodedBytes: Int, maxEncodedBytes: Int)

  def encode(text: String, maxEncodedBytes: Int = DefaultMaxEncodedBytes): Either[PayloadTooLarge, String] =
    val payload = Base64.getEncoder.encodeToString(text.getBytes(StandardCharsets.UTF_8))
    if payload.length > maxEncodedBytes then Left(PayloadTooLarge(payload.length, maxEncodedBytes))
    else Right(s"$Esc]52;c;$payload$Bel")
