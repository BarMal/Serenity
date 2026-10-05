package com.serenity.state.manager

import java.nio.file.Paths

import com.serenity.command.ExternalChangeCommands
import com.serenity.io.DocumentRevision
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1904: startup asks before trusting unsaved text a session brought back over a file it differs from. */
class HotExitRecoverySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(3)
  private val path     = Paths.get("/notes/draft.txt")
  private val base     = DocumentRevision("base")

  private def onDisk(text: String, revision: DocumentRevision = base): Buffer =
    val buffer = Buffer.fromString(bufferId, text)
    buffer.copy(document = buffer.document.copy(filePath = Some(path), revision = Some(revision), isDirty = false))

  private def backup(text: String): Buffer =
    val file = onDisk("saved text")
    file.copy(document = file.document.withContent(Rope(text)))

  "A restored buffer's unsaved text" should "be offered for recovery when it is newer than the unchanged file" in {
    HotExitRecovery.offer(backup("saved text, then more"), onDisk("saved text")) shouldBe
      Some(RecoveryOffer(bufferId, "draft.txt", fileChangedSince = false))
  }

  it should "be offered, noting the change, when the file changed on disk after it was edited" in {
    HotExitRecovery.offer(backup("saved text, then more"), onDisk("rewritten", DocumentRevision("later"))) shouldBe
      Some(RecoveryOffer(bufferId, "draft.txt", fileChangedSince = true))
  }

  it should "not be asked about when it matches the file" in {
    HotExitRecovery.offer(backup("saved text"), onDisk("saved text")) shouldBe None
  }

  "A clean restored buffer" should "never be offered for recovery" in {
    HotExitRecovery.offer(onDisk("saved text"), onDisk("changed on disk")) shouldBe None
  }

  "Offering recovery" should "ask in a blocking prompt that keeps the recovered text unless the file is chosen" in {
    val restored = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = Map(bufferId -> backup("saved text, then more")))
    )

    val offered = HotExitRecovery.withRecoveryOffered(restored, List(RecoveryOffer(bufferId, "draft.txt", false)))

    val prompts = offered.runtime.modalStack.map(_.modal).collect { case Modal.Confirm(prompt) => prompt }
    prompts.map(_.title) shouldBe List("Recover unsaved changes")
    prompts.map(_.blocking) shouldBe List(true)
    prompts.flatMap(_.selectedChoice.map(_.action)) shouldBe List(ConfirmAction.Dismiss)
    prompts.map(_.onDismiss) shouldBe List(ConfirmAction.Dismiss)
    prompts.flatMap(_.choices.items.map(_.action)) should contain(
      ConfirmAction.Run(ExternalChangeCommands.reloadFromDisk(bufferId))
    )
  }

  it should "skip a buffer that is no longer open" in {
    HotExitRecovery.withRecoveryOffered(AppState.initial, List(RecoveryOffer(bufferId, "draft.txt", false))) shouldBe
      AppState.initial
  }

end HotExitRecoverySpec
