package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.IO
import com.serenity.command.DiagnosticsIntent
import com.serenity.diagnostics.{LogFolder, LogLocation, RuntimeIdentity}
import com.serenity.input.SystemClipboard
import com.serenity.state.models.{ConfirmPrompt, Modal}

/** Runs the commands that show which build this is and where its logs are. */
final private[manager] class DiagnosticsEffects(
    showModal: Modal => IO[Unit],
    identity: IO[RuntimeIdentity],
    logDirectory: Path,
    openFolder: Path => IO[Unit],
    copyText: String => IO[Unit]
):

  def interpret(intent: DiagnosticsIntent): IO[Unit] =
    intent match
      case DiagnosticsIntent.ShowAbout =>
        identity.flatMap(build => showModal(Modal.Confirm(ConfirmPrompt.about(build))))
      case DiagnosticsIntent.OpenLogsFolder =>
        openFolder(logDirectory).handleErrorWith(_ =>
          showModal(
            Modal.Confirm(ConfirmPrompt.startupNotice(s"Could not open the logs folder. It is at $logDirectory."))
          )
        )
      case DiagnosticsIntent.CopyToClipboard(text) => copyText(text)

object DiagnosticsEffects:

  def system(showModal: Modal => IO[Unit]): DiagnosticsEffects =
    DiagnosticsEffects(
      showModal,
      IO(RuntimeIdentity.current),
      LogLocation.current,
      LogFolder.open,
      SystemClipboard.awt[IO].writeText
    )
