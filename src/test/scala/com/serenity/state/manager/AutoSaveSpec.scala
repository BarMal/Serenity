package com.serenity.state.manager

import java.nio.file.Paths

import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Which buffers auto-save may write, and which commits make one due (#1992). */
class AutoSaveSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val clean: Buffer =
    Buffer.fromFile(BufferId(1), Paths.get("notes.txt"), "text")

  private def edited(buffer: Buffer): Buffer =
    buffer.copy(document = buffer.document.withContent(com.serenity.rope.Rope("changed")))

  private def untitled(buffer: Buffer): Buffer =
    buffer.copy(document = buffer.document.copy(filePath = None))

  private def stateWith(buffers: Buffer*): AppState =
    val base = AppState.initial
    base.copy(persisted = base.persisted.copy(buffers = buffers.map(buffer => buffer.id -> buffer).toMap))

  private def withModal(state: AppState): AppState =
    val modal = ModalDialog(
      SurfaceId("close-confirmation"),
      Modal.Confirm(ConfirmPrompt.closeUnsaved("notes.txt")),
      ModalPlacement.Centered
    )
    state.copy(runtime = state.runtime.copy(modalStack = state.runtime.modalStack :+ modal))

  "a buffer" should "be savable once it has a file and unsaved changes" in {
    AutoSave.savable(edited(clean)) shouldBe true
  }

  it should "not be savable while it matches its file" in {
    AutoSave.savable(clean) shouldBe false
  }

  it should "not be savable when it is untitled" in {
    AutoSave.savable(untitled(edited(clean))) shouldBe false
  }

  it should "not be savable when it is a hidden note" in {
    AutoSave.savable(edited(clean).copy(hidden = true)) shouldBe false
  }

  "the buffers edited by a commit" should "be the savable ones whose text changed" in {
    AutoSave.edited(stateWith(clean), stateWith(edited(clean))) shouldBe List(clean.id)
  }

  it should "be empty when no buffer's text changed" in {
    val state = stateWith(edited(clean))

    AutoSave.edited(state, state.copy()) shouldBe Nil
  }

  it should "leave out an untitled buffer's edits" in {
    AutoSave.edited(stateWith(clean), stateWith(untitled(edited(clean)))) shouldBe Nil
  }

  "the buffers savable in a state" should "be none while a blocking modal is up" in {
    val state = stateWith(edited(clean))

    AutoSave.savableBuffers(state) shouldBe List(clean.id)
    AutoSave.savableBuffers(withModal(state)) shouldBe Nil
    AutoSave.savableIn(withModal(state), clean.id) shouldBe false
  }
