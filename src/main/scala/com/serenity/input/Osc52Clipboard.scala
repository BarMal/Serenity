package com.serenity.input

import cats.effect.Sync
import cats.syntax.all.*
import org.typelevel.log4cats.Logger

/** A [[SystemClipboard]] backed by OSC 52 writes through the terminal shell's own writer.
  *
  * Read support for OSC 52 is rare and often disabled by terminal configuration, so `readText` delegates to `fallback`
  * rather than blocking on a response that may never arrive -- `fallback` should normally be [[InProcessClipboard]], so
  * pasting back what Serenity itself just copied still works even when the terminal never answers. A payload over
  * `maxEncodedBytes` is never truncated to fit: it is logged and the write goes to `fallback` instead.
  */
object Osc52Clipboard:

  def apply[F[_] : Sync : Logger](
    write: String => F[Unit],
    fallback: SystemClipboard[F],
    maxEncodedBytes: Int = Osc52.DefaultMaxEncodedBytes
  ): SystemClipboard[F] = new SystemClipboard[F]:

    override def readText: F[Option[String]] = fallback.readText

    override def writeText(text: String): F[Unit] =
      Osc52.encode(text, maxEncodedBytes) match
        case Right(sequence) => write(sequence)
        case Left(Osc52.PayloadTooLarge(encodedBytes, maxBytes)) =>
          Logger[F].warn(
            s"[CLIPBOARD] OSC 52 payload too large ($encodedBytes > $maxBytes encoded bytes); falling back"
          ) >> fallback.writeText(text)
