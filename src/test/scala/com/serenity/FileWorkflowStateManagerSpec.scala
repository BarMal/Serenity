package com.serenity

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{Enter, InsertChar, TabKey}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class FileWorkflowStateManagerSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("FileWorkflowStateManagerSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def currentWorkflow(stateManager: StateManager): FileWorkflowState =
    stateManager.getCurrentState
      .unsafeRunSync()
      .modalSurface
      .flatMap {
        _.content match
          case SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)) => Some(workflow)
          case _                                                          => None
      }
      .getOrElse(fail("Expected active file workflow modal"))

  "StateManager.applyEvent" should "refresh file workflow suggestions after path edits" in {
    val tempRoot   = Files.createTempDirectory("workflow-suggestions")
    val projectDir = Files.createDirectory(tempRoot.resolve("project"))
    Files.createDirectory(tempRoot.resolve("private"))

    try
      val stateManager = createStateManager()
      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              filename = "notes.scala",
              path = tempRoot.resolve("p").toString,
              activeField = FileWorkflowField.Path
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(InsertChar('r')).unsafeRunSync()

      val workflow = currentWorkflow(stateManager)
      workflow shouldBe a[OpenFileWorkflowState]
      workflow.path shouldBe tempRoot.resolve("pr").toString
      workflow.suggestions.map(_.value) should contain(projectDir.toString)
      workflow.selectedSuggestionIndex shouldBe 0
    finally
      Files.deleteIfExists(projectDir)
      Files.deleteIfExists(tempRoot.resolve("private"))
      Files.deleteIfExists(tempRoot)
  }

  it should "mark missing directories and require confirmation before save-as creates them" in {
    val tempRoot   = Files.createTempDirectory("workflow-save")
    val targetDir  = tempRoot.resolve("new").resolve("nested")
    val targetFile = targetDir.resolve("notes.scala")
    val bufferId   = BufferId(0)
    val bufferText = "object Notes"

    try
      val stateManager = createStateManager()
      stateManager
        .updateState { state =>
          val buffer = state.buffers(bufferId).copy(content = com.serenity.rope.Rope(bufferText), isDirty = true)
          state.copy(buffers = state.buffers + (bufferId -> buffer))
        }
        .unsafeRunSync()

      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.SaveAs,
              filename = "notes.scala",
              path = targetDir.toString
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(TabKey).unsafeRunSync()

      val refreshed = currentWorkflow(stateManager)
      refreshed shouldBe a[SaveAsFileWorkflowState]
      refreshed.missingPathSegments shouldBe List("new", "nested")
      refreshed.confirmCreateDirectories shouldBe false

      stateManager.applyEvent(Enter).unsafeRunSync()

      val awaitingConfirmation = currentWorkflow(stateManager)
      awaitingConfirmation.confirmCreateDirectories shouldBe true
      Files.exists(targetFile) shouldBe false

      stateManager.applyEvent(Enter).unsafeRunSync()

      val updatedState = stateManager.getCurrentState.unsafeRunSync()
      updatedState.modalSurface shouldBe None
      Files.readString(targetFile) shouldBe bufferText
      updatedState.buffers(bufferId).filePath shouldBe Some(targetFile)
      updatedState.buffers(bufferId).isDirty shouldBe false
    finally
      Files.deleteIfExists(targetFile)
      Files.deleteIfExists(targetDir)
      Files.deleteIfExists(tempRoot.resolve("new"))
      Files.deleteIfExists(tempRoot)
  }

  it should "open a file from the workflow modal into a new focused buffer" in {
    val tempRoot   = Files.createTempDirectory("workflow-open")
    val targetFile = tempRoot.resolve("notes.scala")
    Files.writeString(targetFile, "val answer = 42")

    try
      val stateManager = createStateManager()
      val initialState = stateManager.getCurrentState.unsafeRunSync()

      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              filename = "notes.scala",
              path = tempRoot.toString
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(Enter).unsafeRunSync()

      val updatedState = stateManager.getCurrentState.unsafeRunSync()
      updatedState.modalSurface shouldBe None
      updatedState.bufferOrder should have size (initialState.bufferOrder.size + 1)
      val openedBufferId = updatedState.bufferOrder.last
      updatedState.focusedBufferId shouldBe Some(openedBufferId)
      updatedState.buffers(openedBufferId).filePath shouldBe Some(targetFile)
      updatedState.buffers(openedBufferId).content.collect() shouldBe "val answer = 42"
    finally
      Files.deleteIfExists(targetFile)
      Files.deleteIfExists(tempRoot)
  }

  it should "append a trailing separator when accepting a directory suggestion in the path field with tab" in {
    val tempRoot   = Files.createTempDirectory("workflow-directory-accept")
    val projectDir = Files.createDirectory(tempRoot.resolve("project"))

    try
      val stateManager = createStateManager()
      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              path = tempRoot.resolve("pro").toString,
              activeField = FileWorkflowField.Path
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(InsertChar('j')).unsafeRunSync()
      stateManager.applyEvent(TabKey).unsafeRunSync()

      val workflow = currentWorkflow(stateManager)
      workflow.path shouldBe projectDir.toString + java.io.File.separator
    finally
      Files.deleteIfExists(projectDir)
      Files.deleteIfExists(tempRoot)
  }

  it should "accept a filename suggestion with tab in open workflow mode" in {
    val tempRoot   = Files.createTempDirectory("workflow-file-accept")
    val targetFile = tempRoot.resolve("notes.scala")
    Files.writeString(targetFile, "val answer = 42")

    try
      val stateManager = createStateManager()
      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              path = tempRoot.toString,
              activeField = FileWorkflowField.Filename
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(InsertChar('n')).unsafeRunSync()
      stateManager.applyEvent(TabKey).unsafeRunSync()

      val workflow = currentWorkflow(stateManager)
      workflow.filename shouldBe "notes.scala"
    finally
      Files.deleteIfExists(targetFile)
      Files.deleteIfExists(tempRoot)
  }

  it should "offer matching readable files when open workflow focus is on the filename field" in {
    val tempRoot   = Files.createTempDirectory("workflow-open-files")
    val targetFile = tempRoot.resolve("notes.scala")
    val otherFile  = tempRoot.resolve("draft.txt")
    Files.writeString(targetFile, "val answer = 42")
    Files.writeString(otherFile, "draft")

    try
      val stateManager = createStateManager()
      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              path = tempRoot.toString,
              activeField = FileWorkflowField.Filename
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(InsertChar('n')).unsafeRunSync()

      val workflow = currentWorkflow(stateManager)
      workflow.filename shouldBe "n"
      workflow.suggestions.map(_.value) should contain("notes.scala")
      workflow.suggestions.map(_.value) should not contain "draft.txt"
    finally
      Files.deleteIfExists(targetFile)
      Files.deleteIfExists(otherFile)
      Files.deleteIfExists(tempRoot)
  }

  it should "keep the modal open and surface a visible status when open target is missing" in {
    val tempRoot = Files.createTempDirectory("workflow-open-missing")

    try
      val stateManager = createStateManager()
      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              filename = "missing.scala",
              path = tempRoot.toString
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(Enter).unsafeRunSync()

      val workflow = currentWorkflow(stateManager)
      workflow.statusMessage shouldBe Some(s"File not found: ${tempRoot.resolve("missing.scala")}")
    finally Files.deleteIfExists(tempRoot)
  }
