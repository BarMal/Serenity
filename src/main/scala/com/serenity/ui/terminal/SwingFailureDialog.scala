package com.serenity.ui.terminal

import java.awt.datatransfer.StringSelection
import java.awt.{GraphicsEnvironment, Toolkit}
import javax.swing.JOptionPane

import cats.effect.IO
import com.serenity.app.StartupFailure
import com.serenity.diagnostics.LogFolder

/** The native dialog for a failure before Serenity's own window exists. Plain Swing, since nothing of Serenity's
  * rendering is up yet; skipped without a display, where the console message is all there is.
  */
object SwingFailureDialog:

  private val OpenLogs   = "Open Logs Folder"
  private val CopyReport = "Copy Report"
  private val Close      = "Close"

  def show(notice: StartupFailure.Notice): IO[Unit] =
    IO(GraphicsEnvironment.isHeadless).flatMap(headless => if headless then IO.unit else askUntilClosed(notice))

  private def askUntilClosed(notice: StartupFailure.Notice): IO[Unit] =
    IO.blocking(choose(notice)).flatMap {
      case OpenLogs   => LogFolder.open(notice.logDirectory).handleError(_ => ()) >> askUntilClosed(notice)
      case CopyReport => IO.blocking(copy(notice.report)).handleError(_ => ()) >> askUntilClosed(notice)
      case _          => IO.unit
    }

  private def choose(notice: StartupFailure.Notice): String =
    val options = Array[AnyRef](OpenLogs, CopyReport, Close)
    val picked = JOptionPane.showOptionDialog(
      null,
      notice.message,
      notice.title,
      JOptionPane.DEFAULT_OPTION,
      JOptionPane.ERROR_MESSAGE,
      null,
      options,
      Close
    )
    options.toList.lift(picked).fold(Close)(_.toString)

  private def copy(text: String): Unit =
    Toolkit.getDefaultToolkit.getSystemClipboard.setContents(StringSelection(text), null)
