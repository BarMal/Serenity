package com.serenity.state.manager

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import com.serenity.StateManagerTestSupport
import com.serenity.command.{CommandRegistry, LineEndingCommands}
import com.serenity.keystroke.events.InsertChar
import com.serenity.state.models.*
import com.serenity.text.LineEnding
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1964 through a running editor: the notice on opening a mixed file, the commands, and what the next save writes. */
class LineEndingStateManagerSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def fileWith(text: String): Path =
    Files.write(Files.createTempDirectory("line-endings").resolve("draft.txt"), text.getBytes(UTF_8))

  private def opened(file: Path): (StateManager, BufferId) =
    val editor = createStateManager("LineEndingStateManagerSpec")
    editor.fileOpener.openFile(file).unsafeRunSync()
    val state = awaitOpened(editor, file).unsafeRunSync()
    val bufferId = state.persisted.buffers.collectFirst {
      case (id, buffer) if buffer.document.filePath.contains(file) => id
    }
    (editor, bufferId.getOrElse(fail("file not opened")))

  private def promptTitles(state: AppState): List[String] =
    state.runtime.modalStack.map(_.modal).collect { case Modal.Confirm(prompt) => prompt.title } ++
      state.runtime.uiSurfaces.map(_.content).collect { case SurfaceContent.ModalWorkflow(Modal.Confirm(p)) => p.title }

  private def savedBytes(file: Path): String = new String(Files.readAllBytes(file), UTF_8)

  /** `fileService.saveBuffer` hands the write to the file lane, so the disk is only read once the buffer is clean. */
  private def saveAndAwaitClean(editor: StateManager, bufferId: BufferId): Unit =
    editor.fileService.saveBuffer(bufferId).timeout(20.seconds).unsafeRunSync()
    val _ = awaitState(editor)(_.persisted.buffers.get(bufferId).exists(!_.hasUnsavedChanges)).unsafeRunSync()

  "Opening a file with mixed line endings" should "show a notice saying what saving will do" in {
    val (editor, bufferId) = opened(fileWith("a\r\nb\r\nc\n"))

    val state = editor.getCurrentState.unsafeRunSync()
    promptTitles(state) shouldBe List("Mixed line endings")
    state.persisted.buffers(bufferId).document.lineEnding shouldBe LineEnding.Crlf
    state.persisted.buffers(bufferId).document.mixedLineEndings.map(_.isMixed) shouldBe Some(true)
  }

  it should "not rewrite anything on disk until a save" in {
    val file = fileWith("a\r\nb\r\nc\n")
    opened(file)

    savedBytes(file) shouldBe "a\r\nb\r\nc\n"
  }

  it should "make the file uniform on save, and then no longer count as mixed" in {
    val file               = fileWith("a\r\nb\r\nc\n")
    val (editor, bufferId) = opened(file)

    editor.fileService.saveBuffer(bufferId).timeout(20.seconds).unsafeRunSync()

    awaitState(editor)(_.persisted.buffers.get(bufferId).exists(_.document.mixedLineEndings.isEmpty))
      .unsafeRunSync()
    savedBytes(file) shouldBe "a\r\nb\r\nc\r\n"
  }

  it should "say what a save wrote when no ending was chosen" in {
    val file               = fileWith("a\r\nb\r\nc\n")
    val (editor, bufferId) = opened(file)

    editor.fileService.saveBuffer(bufferId).timeout(20.seconds).unsafeRunSync()

    val state = awaitState(editor)(promptTitles(_).contains("Line endings changed")).unsafeRunSync()
    val messages = state.runtime.uiSurfaces
      .map(_.content)
      .collect { case SurfaceContent.ModalWorkflow(Modal.Confirm(prompt)) => prompt.message }
      .flatten
    messages should contain("Saved with CRLF line endings; the file had 1 LF, 2 CRLF.")
  }

  it should "not say so after the user chose an ending" in {
    val file               = fileWith("a\r\nb\r\nc\n")
    val (editor, bufferId) = opened(file)
    editor.executeCommand(LineEndingCommands.set(bufferId, LineEnding.Lf)).timeout(20.seconds).unsafeRunSync()

    saveAndAwaitClean(editor, bufferId)

    savedBytes(file) shouldBe "a\nb\nc\n"
    promptTitles(editor.getCurrentState.unsafeRunSync()) should not contain "Line endings changed"
  }

  "Opening a file with one line ending" should "show no notice" in {
    val (editor, _) = opened(fileWith("a\r\nb\r\n"))

    promptTitles(editor.getCurrentState.unsafeRunSync()) shouldBe Nil
  }

  "A file of lone CRs" should "be saved back as lone CRs after an edit" in {
    val file               = fileWith("a\rb\r")
    val (editor, bufferId) = opened(file)
    editor.applyEvent(InsertChar('x')).unsafeRunSync()

    saveAndAwaitClean(editor, bufferId)

    savedBytes(file) shouldBe "xa\rb\r"
    editor.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).document.lineEnding shouldBe LineEnding.Cr
  }

  "The Change Line Ending command" should "be in the command palette registry" in {
    CommandRegistry.default.getAllCommands.map(_.name) should contain(LineEndingCommands.chooseLineEnding.name)
  }

  it should "open a picker of line endings for the focused file" in {
    val (editor, _) = opened(fileWith("a\n"))

    editor.executeCommand(LineEndingCommands.chooseLineEnding).timeout(20.seconds).unsafeRunSync()

    val state = editor.getCurrentState.unsafeRunSync()
    val titles = state.runtime.modalStack.map(_.modal).collect { case Modal.ListPicker(picker) => picker.title } ++
      state.runtime.uiSurfaces.map(_.content).collect {
        case SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)) => picker.title
      }
    titles shouldBe List("Change Line Ending")
  }

  "Picking a line ending" should "mark the buffer dirty, and the next save writes it" in {
    val file               = fileWith("a\nb\n")
    val (editor, bufferId) = opened(file)

    editor
      .executeCommand(LineEndingCommands.set(bufferId, LineEnding.Crlf))
      .timeout(20.seconds)
      .unsafeRunSync()

    val changed = editor.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    changed.document.lineEnding shouldBe LineEnding.Crlf
    changed.hasUnsavedChanges shouldBe true
    savedBytes(file) shouldBe "a\nb\n"

    saveAndAwaitClean(editor, bufferId)

    savedBytes(file) shouldBe "a\r\nb\r\n"
  }

end LineEndingStateManagerSpec
