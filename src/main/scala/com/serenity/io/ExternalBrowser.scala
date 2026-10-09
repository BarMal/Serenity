package com.serenity.io

import java.awt.Desktop
import java.net.URI

import cats.effect.IO

object ExternalBrowser:

  /** Fails with `UnsupportedOperationException` when the platform has no browser to open, such as a headless session.
    */
  def browse(uri: URI): IO[Unit] =
    IO.blocking(Desktop.isDesktopSupported && Desktop.getDesktop.isSupported(Desktop.Action.BROWSE)).flatMap {
      case true  => IO.blocking(Desktop.getDesktop.browse(uri))
      case false => IO.raiseError(new UnsupportedOperationException("No desktop browser is available"))
    }
