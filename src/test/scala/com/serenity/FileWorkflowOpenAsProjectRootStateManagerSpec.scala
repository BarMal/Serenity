package com.serenity

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.ModalOpenAsProjectRoot
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.PanelPosition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** StateManager-level coverage for opening a directory from the Open dialog as a project root (issue #1525): wires
  * directory selection to the same `ExplorerEffect.OpenRoot` pin-panel machinery a UI preset's docked directory tree
  * already uses, rather than a parallel "open folder" path.
  */
class FileWorkflowOpenAsProjectRootStateManagerSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("FileWorkflowOpenAsProjectRootStateManagerSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def currentWorkflow(stateManager: StateManager): Option[FileWorkflowState] =
    stateManager.getCurrentState
      .unsafeRunSync()
      .topModal
      .flatMap {
        _.modal match
          case Modal.FileWorkflow(workflow) => Some(workflow)
          case _                            => None
      }

  "the Open dialog" should "pin the browsed directory as a project root and dismiss itself" in {
    val tempDir = Files.createTempDirectory("open-as-project-root")
    val child   = Files.createDirectory(tempDir.resolve("src"))

    try
      val stateManager = createStateManager()
      stateManager.modalService
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

      stateManager.applyEvent(ModalOpenAsProjectRoot).unsafeRunSync()

      val finalState = stateManager.getCurrentState.unsafeRunSync()
      finalState.topModal shouldBe None

      def rootPathOf(surface: UiSurface): Option[java.nio.file.Path] = surface.content match
        case SurfaceContent.DirectoryTree(tree, _) => Some(tree.rootPath)
        case _                                     => None

      val pinnedRoot = finalState.pinnedSurfaces.find(surface => rootPathOf(surface).contains(tempDir))
      pinnedRoot shouldBe defined

      val dockedPosition =
        finalState.persisted.layout.workspaceTree.flatMap(_.positionForSurface(pinnedRoot.get.id))
      dockedPosition shouldBe Some(PanelPosition.Left)
    finally
      Files.deleteIfExists(child)
      Files.deleteIfExists(tempDir)
  }

  it should "report a status message and keep the dialog open when the path is not a directory" in {
    val tempDir  = Files.createTempDirectory("open-as-project-root-file")
    val textFile = Files.createTempFile(tempDir, "notes", ".txt")

    try
      val stateManager = createStateManager()
      stateManager.modalService
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              path = textFile.toString,
              activeField = FileWorkflowField.Path
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(ModalOpenAsProjectRoot).unsafeRunSync()

      val workflow = currentWorkflow(stateManager).getOrElse(fail("expected the open dialog to still be showing"))
      workflow.statusMessage shouldBe Some(s"Not a directory: $textFile")
    finally
      Files.deleteIfExists(textFile)
      Files.deleteIfExists(tempDir)
  }

  it should "do nothing for a Save As workflow, which has no project-root concept" in {
    val stateManager = createStateManager()
    stateManager.modalService
      .showModal(
        Modal.FileWorkflow(
          FileWorkflowState(mode = FileWorkflowMode.SaveAs, filename = "notes.txt", path = "/tmp")
        )
      )
      .unsafeRunSync()

    val before = stateManager.getCurrentState.unsafeRunSync()
    stateManager.applyEvent(ModalOpenAsProjectRoot).unsafeRunSync()
    val after = stateManager.getCurrentState.unsafeRunSync()

    after shouldBe before
  }
end FileWorkflowOpenAsProjectRootStateManagerSpec
