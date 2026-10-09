package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.IO
import com.serenity.command.ManuscriptExportRequest
import com.serenity.io.{FileDialog, FileUtils}
import com.serenity.manuscript.{ManuscriptFileFormat, SourceDocument}
import com.serenity.publish.{ExportOrigin, ManuscriptExport}
import com.serenity.state.effects.{Lane, LaneKey, LanePolicy}
import com.serenity.state.models.{AppState, Buffer}
import org.typelevel.log4cats.Logger

/** "Export Manuscript...": snapshots the focused buffer, asks where to save through the native save dialog, then
  * compiles and writes off the dispatcher. Failures go to the log, the same path a failed save or theme export takes.
  */
final private[manager] class ManuscriptExportEffects(
    logger: Logger[IO],
    fileDialog: Option[FileDialog],
    lanes: EffectLanePort,
    currentState: IO[AppState],
    commitState: (AppState, AppState) => IO[Unit],
    writeDocx: (ExportOrigin, Path) => IO[Unit] = ManuscriptExport.writeDocx,
    writeEpub: (ExportOrigin, Path) => IO[Unit] = ManuscriptExport.writeEpub,
    writePdf: (ExportOrigin, Path) => IO[Unit] = ManuscriptExport.writePdf
):

  def run(request: ManuscriptExportRequest, state: AppState): IO[Unit] =
    request match
      case ManuscriptExportRequest.ChooseFormat =>
        currentState.flatMap(current =>
          ManuscriptExportPicker.withPickerOpened(current).fold(IO.unit)(commitState(_, current))
        )
      case ManuscriptExportRequest.As(format) => exportFocused(state, format)

  def exportFocused(state: AppState, format: ManuscriptFileFormat = ManuscriptFileFormat.Docx): IO[Unit] =
    state.focusedBufferId.flatMap(state.persisted.buffers.get) match
      case None => logger.debug("[EXPORT] Export Manuscript requested without a focused buffer")
      case Some(buffer) =>
        fileDialog match
          case None =>
            logger.warn("[EXPORT] Export Manuscript needs a native save dialog, and none is available")
          case Some(dialog) =>
            lanes.submitEffect(
              ManuscriptExportEffects.DialogLane,
              chooseAndWrite(dialog, ManuscriptExportEffects.originOf(buffer), format)
            )

  private def writer(format: ManuscriptFileFormat): (ExportOrigin, Path) => IO[Unit] =
    format match
      case ManuscriptFileFormat.Docx => writeDocx
      case ManuscriptFileFormat.Epub => writeEpub
      case ManuscriptFileFormat.Pdf  => writePdf

  private def chooseAndWrite(dialog: FileDialog, origin: ExportOrigin, format: ManuscriptFileFormat): IO[Unit] =
    origin.path
      .flatMap(path => Option(path.toAbsolutePath.getParent))
      .fold(FileUtils.getCurrentDirectory)(IO.pure)
      .flatMap(directory =>
        dialog.chooseSaveFile(Some(directory), Some(ManuscriptExport.suggestedFileName(origin.path, format)))
      )
      .flatMap {
        case Some(target) =>
          writer(format)(origin, target) >> logger.info(s"[EXPORT] Exported manuscript to $target")
        case None => IO.unit
      }
      .handleErrorWith(error => logger.error(error)(s"[EXPORT] Manuscript export failed: ${error.getMessage}"))

private[manager] object ManuscriptExportEffects:

  /** Shared with the open-file dialog, so only one native dialog is ever up; a second request while one is open is
    * dropped.
    */
  val DialogLane: Lane.Keyed = Lane.Keyed(LaneKey.Dialog, LanePolicy.DropIfBusy)

  /** A rich buffer exports its formatting only while that formatting still describes its text. */
  def originOf(buffer: Buffer): ExportOrigin =
    val snapshot: SourceDocument =
      buffer.richText.richTextDocument
        .filter(_ => buffer.richTextInSync)
        .fold(SourceDocument.Markdown(buffer.document.content.collect()))(SourceDocument.Rich(_))
    ExportOrigin(buffer.document.filePath, snapshot)
