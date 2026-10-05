package com.serenity.state.manager

import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer

/** A restored buffer whose unsaved text differs from its file on disk (#1904), and how the two compare. */
final case class RecoveryOffer(bufferId: BufferId, label: String, fileChangedSince: Boolean)

/** Startup's decision about unsaved text a session brought back: whether it is worth asking the user to keep it or open
  * the file instead, and the prompts that ask.
  */
object HotExitRecovery:

  /** A buffer the session restored with unsaved edits to a file -- the only kind with a file to compare against. */
  def holdsBackup(buffer: Buffer): Boolean =
    buffer.document.isDirty && buffer.document.filePath.isDefined && !buffer.hidden

  /** `disk` is the buffer's file as read now. No offer when the text already matches it. The file counts as changed
    * since only when both revisions are known and differ: an older session recorded none, and then the backup is all
    * there is to go on.
    */
  def offer(restored: Buffer, disk: Buffer): Option[RecoveryOffer] =
    Option.when(holdsBackup(restored) && disk.document.content.collect() != restored.document.content.collect()) {
      val fileChangedSince =
        restored.document.revision.exists(recorded => disk.document.revision.exists(_ != recorded))
      RecoveryOffer(restored.id, label(restored), fileChangedSince)
    }

  /** One blocking prompt per offer, stacked in order, for buffers still open. */
  def withRecoveryOffered(state: AppState, offers: List[RecoveryOffer]): AppState =
    offers.filter(offer => state.persisted.buffers.contains(offer.bufferId)).foldLeft(state) { (current, offer) =>
      val prompt = ConfirmPrompt.recoverUnsaved(offer.bufferId, offer.label, offer.fileChangedSince)
      ModalStateReducer.show(Modal.Confirm(prompt), current).state
    }

  private def label(buffer: Buffer): String =
    buffer.document.filePath
      .map(path => Option(path.getFileName).fold(path.toString)(_.toString))
      .getOrElse(s"Buffer ${buffer.id.value}")

end HotExitRecovery
