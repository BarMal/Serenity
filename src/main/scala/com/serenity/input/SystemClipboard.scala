package com.serenity.input

import java.awt.Toolkit
import java.awt.datatransfer.{DataFlavor, StringSelection}

import scala.util.control.NonFatal

import cats.effect.Sync
import cats.syntax.applicativeError.*

trait SystemClipboard[F[_]]:
  def readText: F[Option[String]]
  def writeText(text: String): F[Unit]

object SystemClipboard:

  def awt[F[_] : Sync]: SystemClipboard[F] = new SystemClipboard[F]:
    override def readText: F[Option[String]] =
      Sync[F]
        .blocking {
          val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
          if clipboard.isDataFlavorAvailable(DataFlavor.stringFlavor) then
            Option(clipboard.getData(DataFlavor.stringFlavor)).map(_.toString)
          else None
        }
        .handleError(_ => None)

    override def writeText(text: String): F[Unit] =
      Sync[F]
        .blocking {
          val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
          // No owner-notification behavior is needed here, so this is a no-op ClipboardOwner rather than
          // the null the AWT API also accepts.
          clipboard.setContents(StringSelection(text), (_, _) => ())
        }
        .handleErrorWith { case NonFatal(_) => Sync[F].unit }
