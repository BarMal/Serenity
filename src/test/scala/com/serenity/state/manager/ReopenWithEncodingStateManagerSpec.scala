package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.StateManagerTestSupport
import com.serenity.command.ReopenWithEncodingCommands
import com.serenity.keystroke.events.InsertChar
import com.serenity.state.models.*
import com.serenity.text.TextEncoding
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1627 through a running editor: the palette command, the picked encoding, and the dirty-buffer prompt. */
class ReopenWithEncodingStateManagerSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  /** Detected as UTF-8 "é"; read as windows-1252 it is "Ã©". */
  private def ambiguousFile: Path =
    val file = Files.createTempDirectory("reopen-with-encoding").resolve("draft.txt")
    Files.write(file, Array(0xc3.toByte, 0xa9.toByte))

  private def opened(file: Path): (StateManager, BufferId) =
    val editor = createStateManager("ReopenWithEncodingStateManagerSpec")
    editor.fileOpener.openFile(file).unsafeRunSync()
    val state = awaitState(editor)(s =>
      s.focusedBufferId.flatMap(s.persisted.buffers.get).exists(_.document.filePath.contains(file))
    ).unsafeRunSync()
    (editor, state.focusedBufferId.getOrElse(fail("no focused buffer")))

  private def bufferOf(editor: StateManager, bufferId: BufferId): Buffer =
    editor.getCurrentState.unsafeRunSync().persisted.buffers.getOrElse(bufferId, fail("buffer gone"))

  private def promptTitles(state: AppState): List[String] =
    state.runtime.modalStack.map(_.modal).collect { case Modal.Confirm(prompt) => prompt.title }

  private def run(editor: StateManager, reopen: IO[Unit]): Unit = reopen.timeout(20.seconds).unsafeRunSync()

  "The Reopen with Encoding command" should "open a picker of encodings for the focused file" in {
    val (editor, _) = opened(ambiguousFile)

    run(editor, editor.executeCommand(ReopenWithEncodingCommands.chooseEncoding))

    val state = editor.getCurrentState.unsafeRunSync()
    val titles = state.runtime.modalStack.map(_.modal).collect { case Modal.ListPicker(picker) => picker.title } ++
      state.runtime.uiSurfaces.map(_.content).collect {
        case SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)) => picker.title
      }
    titles shouldBe List("Reopen with Encoding")
  }

  "Picking an encoding" should "read the file again in it" in {
    val (editor, bufferId) = opened(ambiguousFile)
    bufferOf(editor, bufferId).document.content.collect() shouldBe "é"

    run(editor, editor.executeCommand(ReopenWithEncodingCommands.reopen(bufferId, TextEncoding.Windows1252)))

    val reopened =
      awaitState(editor)(_.persisted.buffers.get(bufferId).exists(_.document.content.collect() == "Ã©"))
        .unsafeRunSync()
        .persisted
        .buffers(bufferId)
    reopened.document.encoding shouldBe TextEncoding.Windows1252
    reopened.document.isDirty shouldBe false
  }

  it should "ask first when the buffer has unsaved edits, and only discard them once confirmed" in {
    val (editor, bufferId) = opened(ambiguousFile)
    editor.applyEvent(InsertChar('x')).unsafeRunSync()
    val edited = bufferOf(editor, bufferId).document.content.collect()

    run(editor, editor.executeCommand(ReopenWithEncodingCommands.reopen(bufferId, TextEncoding.Windows1252)))

    promptTitles(editor.getCurrentState.unsafeRunSync()) shouldBe List("Discard unsaved changes?")
    bufferOf(editor, bufferId).document.content.collect() shouldBe edited

    run(
      editor,
      editor.executeCommand(ReopenWithEncodingCommands.reopen(bufferId, TextEncoding.Windows1252, discardEdits = true))
    )

    awaitState(editor)(_.persisted.buffers.get(bufferId).exists(_.document.content.collect() == "Ã©")).unsafeRunSync()
  }

  it should "leave the buffer alone and say so when the file is not valid in the picked encoding" in {
    val file = Files.createTempDirectory("reopen-with-encoding").resolve("latin.txt")
    Files.write(file, Array('c'.toByte, 0xe9.toByte))
    val (editor, bufferId) = opened(file)
    val before             = bufferOf(editor, bufferId).document.content.collect()

    run(editor, editor.executeCommand(ReopenWithEncodingCommands.reopen(bufferId, TextEncoding.Utf8)))

    val failed = awaitState(editor)(state =>
      state.runtime.uiSurfaces.map(_.content).exists {
        case SurfaceContent.ModalWorkflow(Modal.Confirm(prompt)) => prompt.title == "Can't reopen"
        case _                                                   => false
      }
    ).unsafeRunSync()
    failed.persisted.buffers(bufferId).document.content.collect() shouldBe before
  }

end ReopenWithEncodingStateManagerSpec
