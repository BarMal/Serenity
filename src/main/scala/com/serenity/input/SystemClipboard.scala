package com.serenity.input

import java.awt.Toolkit
import java.awt.datatransfer.{DataFlavor, StringSelection}

import scala.util.control.NonFatal

import cats.effect.Sync
import cats.syntax.applicativeError.*

/** A cold capability (user-initiated, not a per-keystroke/per-frame boundary) expressed as a record of functions rather
  * than a trait -- see #1017. A test double is a record literal, not a subclass; wrapping one in logging or retry is
  * `copy(readText = ...)`.
  */
final case class SystemClipboard[F[_]](
    readText: F[Option[String]],
    writeText: String => F[Unit]
)

object SystemClipboard:

  def awt[F[_] : Sync]: SystemClipboard[F] =
    SystemClipboard(
      readText = Sync[F]
        .blocking {
          val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
          if clipboard.isDataFlavorAvailable(DataFlavor.stringFlavor) then
            Option(clipboard.getData(DataFlavor.stringFlavor)).map(_.toString)
          else None
        }
        .handleError(_ => None),
      writeText = text =>
        Sync[F]
          .blocking {
            val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
            // No owner-notification behavior is needed here, so this is a no-op ClipboardOwner rather than
            // the null the AWT API also accepts.
            clipboard.setContents(StringSelection(text), (_, _) => ())
          }
          .handleErrorWith { case NonFatal(_) => Sync[F].unit }
    )
