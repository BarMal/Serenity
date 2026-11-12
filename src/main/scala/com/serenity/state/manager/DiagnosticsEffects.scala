package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.IO
import com.serenity.command.DiagnosticsIntent
import com.serenity.diagnostics.{LogFolder, LogLocation, RuntimeIdentity}
import com.serenity.input.SystemClipboard
import com.serenity.state.models.{AppState, ConfirmPrompt, Modal}

/** Runs the commands that show which build this is and where its logs are. */
final private[manager] class DiagnosticsEffects(
    showModal: Modal => IO[Unit],
    identity: IO[RuntimeIdentity],
    logDirectory: Path,
    openFolder: Path => IO[Unit],
    copyText: String => IO[Unit],
    opensFolderInFileManager: IO[Boolean],
    exportToTerminalClipboard: String => IO[Unit]
):

  def interpret(intent: DiagnosticsIntent): IO[Unit] =
    intent match
      case DiagnosticsIntent.ShowAbout =>
        identity.flatMap(build => showModal(Modal.Confirm(ConfirmPrompt.about(build))))
      case DiagnosticsIntent.OpenLogsFolder =>
        opensFolderInFileManager.flatMap(opens => if opens then openLogsFolder else sayWhereTheLogsAre)
      case DiagnosticsIntent.CopyToClipboard(text) => copyText(text)

  private def openLogsFolder: IO[Unit] =
    openFolder(logDirectory).handleErrorWith(_ =>
      showModal(
        Modal.Confirm(ConfirmPrompt.startupNotice(s"Could not open the logs folder. It is at $logDirectory."))
      )
    )

  // With no display a file manager would show nothing (or open on another machine over SSH), so the path is
  // said, and sent to the terminal's clipboard: OSC 52 gives no way to learn whether the terminal took it.
  private def sayWhereTheLogsAre: IO[Unit] =
    exportToTerminalClipboard(logDirectory.toString) >>
      showModal(
        Modal.Confirm(
          ConfirmPrompt.startupNotice(
            s"The logs folder is at $logDirectory. Sent to your terminal's clipboard; " +
              "if pasting gives nothing, your terminal doesn't accept OSC 52."
          )
        )
      )

object DiagnosticsEffects:

  /** `text` handed to the clipboard sync that follows the batch, with no clipboard-history entry of its own. */
  def exported(text: String)(state: AppState): AppState =
    state.copy(runtime =
      state.runtime.copy(clipboard = Some(text), clipboardExports = state.runtime.clipboardExports + 1)
    )

  def system(
    showModal: Modal => IO[Unit],
    opensFolderInFileManager: IO[Boolean],
    exportToTerminalClipboard: String => IO[Unit]
  ): DiagnosticsEffects =
    DiagnosticsEffects(
      showModal,
      IO(RuntimeIdentity.current),
      LogLocation.current,
      LogFolder.open,
      SystemClipboard.awt[IO].writeText,
      opensFolderInFileManager,
      exportToTerminalClipboard
    )
