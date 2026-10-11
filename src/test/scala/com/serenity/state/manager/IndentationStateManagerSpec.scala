package com.serenity.state.manager

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import com.serenity.command.{CommandRegistry, IndentationCommands}
import com.serenity.config.StatusSegment
import com.serenity.keystroke.events.TabKey
import com.serenity.state.models.*
import com.serenity.{StateManagerTestSupport, TestTemp}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1996 through a running editor: indentation detected as a file opens, and the Change Indentation command. */
class IndentationStateManagerSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def fileWith(name: String, text: String): Path =
    Files.write(TestTemp.directory("indentation").resolve(name), text.getBytes(UTF_8))

  private def opened(file: Path): (StateManager, BufferId) =
    val editor = createStateManager("IndentationStateManagerSpec")
    editor.fileOpener.openFile(file).unsafeRunSync()
    val state = awaitOpened(editor, file).unsafeRunSync()
    val bufferId = state.persisted.buffers.collectFirst {
      case (id, buffer) if buffer.document.filePath.contains(file) => id
    }
    (editor, bufferId.getOrElse(fail("file not opened")))

  private def effective(editor: StateManager, bufferId: BufferId): IndentationSettings =
    val state = editor.getCurrentState.unsafeRunSync()
    state.persisted.buffers(bufferId).effectiveIndentation(state.persisted.config.indentation)

  private def statusLine(editor: StateManager): Option[String] =
    StatusLineText.render(editor.getCurrentState.unsafeRunSync(), List(StatusSegment.Indentation))

  "Opening a file" should "detect spaces and their width from its text" in {
    val (editor, bufferId) = opened(fileWith("two.txt", "a:\n  b:\n    c: 1\n  d: 2\n"))

    effective(editor, bufferId) shouldBe IndentationSettings(tabSize = 2, insertSpaces = true)
    statusLine(editor) shouldBe Some("Spaces: 2")
  }

  it should "detect tabs, and keep the global tab size for their width" in {
    val (editor, bufferId) = opened(fileWith("tabs.txt", "a\n\tb\n\t\tc\n"))

    effective(editor, bufferId) shouldBe IndentationSettings(tabSize = 4, insertSpaces = false)
    statusLine(editor) shouldBe Some("Tab Size: 4")
  }

  it should "use the global settings for a file with no indentation" in {
    val (editor, bufferId) = opened(fileWith("flat.txt", "one\ntwo\n"))

    effective(editor, bufferId) shouldBe IndentationSettings()
  }

  "Tab in a file opened with detected indentation" should "insert that file's unit" in {
    val (editor, bufferId) = opened(fileWith("tab-key.txt", "a\n\tb\n"))

    editor.applyEvent(TabKey).unsafeRunSync()

    editor.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).document.content.collect() shouldBe
      "\ta\n\tb\n"
  }

  "The Change Indentation command" should "be in the command palette registry" in {
    CommandRegistry.default.getAllCommands.map(_.name) should contain(IndentationCommands.chooseIndentation.name)
  }

  it should "open a picker of indentations for the focused file" in {
    val (editor, _) = opened(fileWith("pick.txt", "a\n"))

    editor.executeCommand(IndentationCommands.chooseIndentation).timeout(20.seconds).unsafeRunSync()

    val state = editor.getCurrentState.unsafeRunSync()
    val titles = state.runtime.modalStack.map(_.modal).collect { case Modal.ListPicker(picker) => picker.title } ++
      state.runtime.uiSurfaces.map(_.content).collect {
        case SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)) => picker.title
      }
    titles shouldBe List("Change Indentation")
  }

  "Picking an indentation" should "apply to that file only, without marking it dirty" in {
    val (editor, bufferId) = opened(fileWith("chosen.txt", "a:\n  b\n"))
    val eightTabs          = IndentationSettings(tabSize = 8, insertSpaces = false)

    editor.executeCommand(IndentationCommands.set(bufferId, eightTabs)).timeout(20.seconds).unsafeRunSync()

    effective(editor, bufferId) shouldBe eightTabs
    statusLine(editor) shouldBe Some("Tab Size: 8")
    editor.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).hasUnsavedChanges shouldBe false
  }

  it should "change what Tab inserts" in {
    val (editor, bufferId) = opened(fileWith("chosen-tab.txt", "a:\n  b\n"))
    editor
      .executeCommand(IndentationCommands.set(bufferId, IndentationSettings(tabSize = 3)))
      .timeout(20.seconds)
      .unsafeRunSync()

    editor.applyEvent(TabKey).unsafeRunSync()

    editor.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).document.content.collect() shouldBe
      "   a:\n  b\n"
  }

  "Choosing automatic indentation" should "return to what the file shows" in {
    val (editor, bufferId) = opened(fileWith("automatic.txt", "a:\n  b\n"))
    editor
      .executeCommand(IndentationCommands.set(bufferId, IndentationSettings(tabSize = 8, insertSpaces = false)))
      .timeout(20.seconds)
      .unsafeRunSync()

    editor.executeCommand(IndentationCommands.useAutomatic(bufferId)).timeout(20.seconds).unsafeRunSync()

    effective(editor, bufferId) shouldBe IndentationSettings(tabSize = 2, insertSpaces = true)
  }
