package com.serenity

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{Enter, InsertChar, OpenFile, TabKey}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** StateManager-level coverage for the open dialog's unified directory browser (#1289) -- kept separate from
  * `FileWorkflowStateManagerSpec` so neither spec crosses the architecture-check file-length target.
  */
class FileWorkflowOpenDialogStateManagerSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("FileWorkflowOpenDialogStateManagerSpec"))
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

  "StateManager open dialog" should "begin in the Path field when opened in TUI mode (no native dialog)" in {
    val stateManager = createStateManager()
    stateManager.applyEvent(OpenFile).unsafeRunSync()

    val workflow = currentWorkflow(stateManager)
    workflow shouldBe a[OpenFileWorkflowState]
    workflow.activeField shouldBe FileWorkflowField.Path
  }

  it should "include readable files alongside directories in the Path field suggestions" in {
    val tempDir  = Files.createTempDirectory("open-path-dir-and-file")
    val subDir   = Files.createDirectory(tempDir.resolve("documents"))
    val textFile = Files.createTempFile(tempDir, "notes", ".txt")
    Files.writeString(textFile, "hello")

    try
      val stateManager = createStateManager()
      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              path = tempDir.toString,
              activeField = FileWorkflowField.Path
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(InsertChar('/')).unsafeRunSync()

      val workflow        = currentWorkflow(stateManager)
      val suggestionPaths = workflow.suggestions.map(_.value)
      suggestionPaths should contain(subDir.toString)
      suggestionPaths should contain(textFile.toString)
      workflow.suggestions.find(_.value == subDir.toString).map(_.isDirectory) shouldBe Some(true)
      workflow.suggestions.find(_.value == textFile.toString).map(_.isDirectory) shouldBe Some(false)
      workflow.suggestions.indexWhere(_.value == subDir.toString) should be <
        workflow.suggestions.indexWhere(_.value == textFile.toString)
    finally
      Files.deleteIfExists(textFile)
      Files.deleteIfExists(subDir)
      Files.deleteIfExists(tempDir)
  }

  it should "open a file immediately when its path suggestion is accepted with Tab" in {
    val tempDir  = Files.createTempDirectory("open-tab-file-path")
    val textFile = Files.createTempFile(tempDir, "notes", ".txt")
    Files.writeString(textFile, "tab-opened content")

    try
      val stateManager = createStateManager()
      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              path = tempDir.toString,
              activeField = FileWorkflowField.Path
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(InsertChar('/')).unsafeRunSync()

      val beforeTab = currentWorkflow(stateManager)
      beforeTab.suggestions.map(_.value) should contain(textFile.toString)

      stateManager.applyEvent(TabKey).unsafeRunSync()

      val finalState   = stateManager.getCurrentState.unsafeRunSync()
      val openedBuffer = finalState.persisted.buffers.values.find(_.document.filePath.contains(textFile))
      finalState.modalSurface shouldBe None
      openedBuffer.map(_.document.content.collect()) shouldBe Some("tab-opened content")
    finally
      Files.deleteIfExists(textFile)
      Files.deleteIfExists(tempDir)
  }

  it should "navigate into a directory when Enter is pressed on a path that resolves to a directory" in {
    val tempDir = Files.createTempDirectory("open-enter-dir")
    val subDir  = Files.createDirectory(tempDir.resolve("inner"))
    val subFile = Files.createTempFile(subDir, "notes", ".txt")
    Files.writeString(subFile, "inner content")

    try
      val stateManager = createStateManager()
      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              path = subDir.toString,
              activeField = FileWorkflowField.Path
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(Enter).unsafeRunSync()

      val workflow = currentWorkflow(stateManager)
      workflow.statusMessage shouldBe None
      workflow.path should startWith(subDir.toString)
    finally
      Files.deleteIfExists(subFile)
      Files.deleteIfExists(subDir)
      Files.deleteIfExists(tempDir)
  }
