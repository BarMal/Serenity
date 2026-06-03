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
      Sync[F].blocking {
        val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
        if clipboard.isDataFlavorAvailable(DataFlavor.stringFlavor) then
          Option(clipboard.getData(DataFlavor.stringFlavor)).map(_.toString)
        else None
      }.handleError(_ => None)

    override def writeText(text: String): F[Unit] =
      Sync[F].blocking {
        val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
        clipboard.setContents(StringSelection(text), null)
      }.handleErrorWith {
        case NonFatal(_) => Sync[F].unit
      }
