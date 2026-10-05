package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.IO
import com.serenity.io.{FileDialog, FileUtils}
import com.serenity.manuscript.SourceDocument
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
    writeDocx: (ExportOrigin, Path) => IO[Unit] = ManuscriptExport.writeDocx
):

  def exportFocused(state: AppState): IO[Unit] =
    state.focusedBufferId.flatMap(state.persisted.buffers.get) match
      case None => logger.debug("[EXPORT] Export Manuscript requested without a focused buffer")
      case Some(buffer) =>
        fileDialog match
          case None =>
            logger.warn("[EXPORT] Export Manuscript needs a native save dialog, and none is available")
          case Some(dialog) =>
            lanes.submitEffect(
              ManuscriptExportEffects.DialogLane,
              chooseAndWrite(dialog, ManuscriptExportEffects.originOf(buffer))
            )

  private def chooseAndWrite(dialog: FileDialog, origin: ExportOrigin): IO[Unit] =
    origin.path
      .flatMap(path => Option(path.toAbsolutePath.getParent))
      .fold(FileUtils.getCurrentDirectory)(IO.pure)
      .flatMap(directory =>
        dialog.chooseSaveFile(Some(directory), Some(ManuscriptExport.suggestedFileName(origin.path)))
      )
      .flatMap {
        case Some(target) =>
          writeDocx(origin, target) >> logger.info(s"[EXPORT] Exported manuscript to $target")
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
