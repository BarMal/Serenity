package com.serenity.state.manager

import java.nio.file.Paths

import com.serenity.command.ReopenWithEncodingCommands
import com.serenity.io.{DecodedText, TextFileCodec}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.text.TextEncoding
import com.serenity.ui.widget.Loadable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1627: a file whose encoding was guessed wrong can be read again in one the user picks. */
class ReopenWithEncodingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val path = Paths.get("/notes/draft.txt")

  /** "é" in UTF-8 -- or "Ã©" in windows-1252, which detection cannot tell apart. */
  private val ambiguous = Array(0xc3.toByte, 0xa9.toByte)

  "Decoding as a chosen encoding" should "read the bytes in that encoding even when another one also fits" in {
    TextFileCodec.decode(ambiguous).map(_.content) shouldBe Some("é")
    TextFileCodec.decodeAs(ambiguous, TextEncoding.Windows1252) shouldBe
      Some(DecodedText("Ã©", TextEncoding.Windows1252, hasBom = false))
  }

  it should "strip and remember that encoding's byte order mark" in {
    val withBom = TextEncoding.Utf16Le.byteOrderMark.toArray ++ "hi".getBytes(TextEncoding.Utf16Le.charset)

    TextFileCodec.decodeAs(withBom, TextEncoding.Utf16Le) shouldBe
      Some(DecodedText("hi", TextEncoding.Utf16Le, hasBom = true))
  }

  it should "refuse bytes the encoding cannot represent" in {
    TextFileCodec.decodeAs(Array(0xe9.toByte), TextEncoding.Utf8) shouldBe None
  }

  private def withFile(dirty: Boolean): (AppState, BufferId) =
    val (opened, bufferId) = EditorState.createNewEmptyBuffer(AppState.initial)
    val buffer             = opened.persisted.buffers(bufferId)
    val onDisk             = buffer.document.copy(filePath = Some(path), isNewEmpty = false)
    val document           = if dirty then onDisk.withContent(Rope("edited")) else onDisk
    val state = opened.copy(persisted =
      opened.persisted.copy(buffers = opened.persisted.buffers.updated(bufferId, buffer.copy(document = document)))
    )
    (EditorState.focusBuffer(EditorState.rebalancePanes(state, Some(bufferId)), bufferId), bufferId)

  private def pickers(state: AppState): List[ListPicker] =
    state.runtime.modalStack.map(_.modal).collect { case Modal.ListPicker(picker) => picker } ++
      state.runtime.uiSurfaces.map(_.content).collect { case SurfaceContent.ModalWorkflow(Modal.ListPicker(p)) => p }

  private def prompts(state: AppState): List[ConfirmPrompt] =
    state.runtime.modalStack.map(_.modal).collect { case Modal.Confirm(prompt) => prompt } ++
      state.runtime.uiSurfaces.map(_.content).collect { case SurfaceContent.ModalWorkflow(Modal.Confirm(p)) => p }

  "Choosing an encoding to reopen with" should "offer every encoding, marking the one the file was read in" in {
    val (state, bufferId) = withFile(dirty = false)

    val opened = ReopenWithEncoding.withPickerOpened(state).getOrElse(fail("no picker for a file buffer"))

    val choices = pickers(opened).flatMap(_.items match
      case Loadable.Ready(list) => list.items.toList
      case _                    => Nil)
    choices.map(_.label) shouldBe TextEncoding.values.toList.map(_.configKey)
    choices.map(_.action) shouldBe TextEncoding.values.toList.map(ReopenWithEncodingCommands.reopen(bufferId, _))
    choices.filter(_.detail.contains("current")).map(_.label) shouldBe List(TextEncoding.Utf8.configKey)
  }

  it should "not open for a buffer with no file to read again" in {
    val (state, _) = EditorState.createNewEmptyBuffer(AppState.initial)

    ReopenWithEncoding.withPickerOpened(state) shouldBe None
  }

  "Reopening a buffer with unsaved edits" should "ask before throwing them away" in {
    val (state, bufferId) = withFile(dirty = true)

    val asked = ReopenWithEncoding.withDiscardConfirmation(state, bufferId, TextEncoding.Iso88591, discardEdits = false)

    val prompt = asked.toList.flatMap(prompts)
    prompt.map(_.title) shouldBe List("Discard unsaved changes?")
    prompt.flatMap(_.choices.items.map(_.action)) shouldBe List(
      ConfirmAction.Run(ReopenWithEncodingCommands.reopen(bufferId, TextEncoding.Iso88591, discardEdits = true)),
      ConfirmAction.Dismiss
    )
  }

  it should "go ahead once that has been answered" in {
    val (state, bufferId) = withFile(dirty = true)

    ReopenWithEncoding.withDiscardConfirmation(state, bufferId, TextEncoding.Iso88591, discardEdits = true) shouldBe
      None
  }

  "Reopening a buffer with no unsaved edits" should "go ahead without asking" in {
    val (state, bufferId) = withFile(dirty = false)

    ReopenWithEncoding.withDiscardConfirmation(state, bufferId, TextEncoding.Iso88591, discardEdits = false) shouldBe
      None
  }

  "A reopen the file's bytes don't fit" should "say so and offer another encoding" in {
    val (state, bufferId) = withFile(dirty = false)

    val failed = ReopenWithEncoding.withFailureShown(state, bufferId, TextEncoding.Utf8)

    prompts(failed).map(_.message) shouldBe List(List("draft.txt isn't valid UTF-8."))
    prompts(failed).flatMap(_.choices.items.map(_.action)) should contain(
      ConfirmAction.Run(ReopenWithEncodingCommands.chooseEncoding)
    )
  }

end ReopenWithEncodingSpec
