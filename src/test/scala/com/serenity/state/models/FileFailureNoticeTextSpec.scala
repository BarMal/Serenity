package com.serenity.state.models

import java.io.IOException
import java.nio.file.{AccessDeniedException, Path}

import com.serenity.config.{AppConfig, HotkeyAction}
import com.serenity.io.{DocumentStorageError, FileManagerError, StorageLocation}
import com.serenity.rope.Balance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a failed save, open or reload tells the user (#1717, #2015): the file by name, the cause in plain words, and
  * the keys for what to do next -- never a stack trace's class name when a plainer cause is known.
  */
class FileFailureNoticeTextSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val target = Path.of("/home/writer/drafts/notes.md")
  private val local  = StorageLocation.Local(target)

  private def keysFor(action: HotkeyAction): String =
    AppConfig.default.inputConfig.hotkeyConfig.bindingsFor(action).headOption.map(_.render).getOrElse(fail("unbound"))

  "A failed file save" should "name the file and say the cause plainly" in {
    val notice = FileFailureNotice.fileSaveFailed(
      BufferId(3),
      target,
      FileManagerError.StorageFailure(DocumentStorageError.AccessDenied(local)),
      AppConfig.default
    )

    notice.level shouldBe NoticeLevel.Error
    notice.message shouldBe "Couldn't save notes.md: permission denied."
    notice.topic shouldBe Some(NoticeTopic.FileSave(BufferId(3)))
  }

  it should "offer the configured keys to retry or save elsewhere, and to dismiss" in {
    val notice = FileFailureNotice.fileSaveFailed(BufferId(3), target, new IOException("boom"), AppConfig.default)

    val hint = notice.hint.getOrElse(fail("no hint"))
    hint should include(s"${keysFor(HotkeyAction.Save)} retry")
    hint should include(s"${keysFor(HotkeyAction.SaveAs)} save as")
    hint should include("esc dismiss")
  }

  it should "name an untitled buffer by its label when it has no file yet" in {
    val state  = AppState.initial
    val buffer = state.persisted.bufferOrder.headOption.getOrElse(fail("no buffer"))

    FileFailureNotice.forBuffer(state, buffer, new IOException("boom")).message should startWith(
      s"Couldn't save Buffer ${buffer.value}:"
    )
  }

  "The cause of a failed write" should "be said in plain words for the failures people meet" in {
    FileFailureNotice.cause(new AccessDeniedException(target.toString)) shouldBe "permission denied"
    FileFailureNotice.cause(new IOException("No space left on device")) shouldBe "the disk is full"
    FileFailureNotice.cause(
      FileManagerError.StorageFailure(DocumentStorageError.Failed("No space left on device"))
    ) shouldBe "the disk is full"
    FileFailureNotice.cause(
      FileManagerError.StorageFailure(DocumentStorageError.NotFound(local))
    ) shouldBe "it no longer exists"
    FileFailureNotice.cause(FileManagerError.ExternalConflict(local)) shouldBe
      "the file changed on disk since it was opened"
  }

  it should "fall back to the error's own message, and only then to its kind" in {
    FileFailureNotice.cause(new IOException("cannot write notes.md")) shouldBe "cannot write notes.md"
    FileFailureNotice.cause(new IOException()) shouldBe "IOException"
  }

  "A failed session save" should "be an error naming the session" in {
    val notice = FileFailureNotice.sessionSaveFailed(new AccessDeniedException("/home/writer/.serenity"))

    notice.level shouldBe NoticeLevel.Error
    notice.message shouldBe "Couldn't save the session: permission denied."
    notice.topic shouldBe Some(NoticeTopic.SessionSave)
  }

  "A failed session backup" should "warn that unsaved changes may not survive a crash" in {
    val notice = FileFailureNotice.sessionBackupFailed(new IOException("No space left on device"))

    notice.level shouldBe NoticeLevel.Warning
    notice.message shouldBe
      "Couldn't back up this session: the disk is full. Unsaved changes may not survive a crash."
    notice.topic shouldBe Some(NoticeTopic.SessionSave)
  }

  "A failed open" should "name the file and say why it could not be read" in {
    val notice = FileFailureNotice.openFailed(target, FileManagerError.BinaryContent(target))

    notice.level shouldBe NoticeLevel.Error
    notice.message shouldBe "Couldn't open notes.md: it isn't a text file."
    notice.hint shouldBe Some("esc dismiss")
  }

  it should "say so plainly when the path is not a readable file" in {
    FileFailureNotice.notReadable(target).message shouldBe
      "Couldn't open notes.md: it doesn't exist or can't be read."
  }

  "Opening a folder as a file" should "say it is a folder and point at Open Folder, not claim it does not exist" in {
    val notice = FileFailureNotice.isFolder(Path.of("/home/writer/drafts"))

    notice.level shouldBe NoticeLevel.Error
    notice.message shouldBe "drafts is a folder. Use Open Folder to open it."
    notice.hint shouldBe Some("esc dismiss")
  }

  "A native file dialog that cannot be shown" should "say so and give the cause" in {
    val notice = FileFailureNotice.dialogFailed(new IllegalStateException("no display"))

    notice.level shouldBe NoticeLevel.Error
    notice.message shouldBe "Couldn't show the file dialog: no display."
    notice.hint shouldBe Some("esc dismiss")
  }

  "A failed reload" should "name the file and the cause" in {
    val notice = FileFailureNotice.reloadFailed(
      target,
      FileManagerError.StorageFailure(DocumentStorageError.NotFound(local))
    )

    notice.level shouldBe NoticeLevel.Error
    notice.message shouldBe "Couldn't reload notes.md: it no longer exists."
  }
