package com.serenity

import java.io.File.separator
import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The in-app Open Folder form against the real state manager: what it lists, what Enter does with the Path, and
  * confirming a folder through the one project-root route.
  */
class OpenFolderStateManagerSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def workflowIn(state: AppState): Option[FileWorkflowState] =
    state.topModal.map(_.modal).collect { case Modal.FileWorkflow(workflow) => workflow }

  private def explorerRoots(state: AppState): List[Path] =
    state.pinnedSurfaces.map(_.content).collect { case SurfaceContent.DirectoryTree(tree, _, _) => tree.rootPath }

  private def withForm[A](path: Path)(body: StateManager => IO[A]): A =
    val program = for
      stateManager <- createStateManagerIO("OpenFolderStateManagerSpec")
      _ <- stateManager.updateState(state =>
        ModalStateReducer
          .show(
            Modal.FileWorkflow(
              FileWorkflowState(
                mode = FileWorkflowMode.OpenFolder,
                path = path.toString,
                activeField = FileWorkflowField.Path
              )
            ),
            state
          )
          .state
      )
      result <- body(stateManager)
    yield result
    program.unsafeRunSync()

  private def settle(stateManager: StateManager): IO[AppState] =
    stateManager.runtimeLifecycle.awaitEffects >> stateManager.getCurrentState

  private def inTempTree(test: (Path, Path, Path) => Unit): Unit =
    val root   = TestTemp.directory("open-folder-form")
    val folder = Files.createDirectory(root.resolve("src"))
    val file   = Files.createFile(root.resolve("notes.txt"))
    try test(root, folder, file)
    finally
      Files.deleteIfExists(file)
      Files.deleteIfExists(folder)
      Files.deleteIfExists(root)

  "Enter in the Open Folder form" should "browse into the folder named by the Path and list only folders" in
    inTempTree { (root, folder, _) =>
      val shown = withForm(root) { stateManager =>
        stateManager.applyEvent(ModalSubmit) >>
          awaitState(stateManager)(state =>
            workflowIn(state).exists(workflow => workflow.path.endsWith(separator) && workflow.suggestions.nonEmpty)
          )
      }

      val workflow = workflowIn(shown).getOrElse(fail("expected the form to stay open"))
      workflow.path shouldBe root.toString + separator
      workflow.suggestions shouldBe List(FileWorkflowSuggestion(folder.toString, isDirectory = true))
      workflow.statusMessage shouldBe None
    }

  it should "say a file is not a folder and stay where it is" in
    inTempTree { (_, _, file) =>
      val after = withForm(file)(stateManager => stateManager.applyEvent(ModalSubmit) >> settle(stateManager))

      val workflow = workflowIn(after).getOrElse(fail("expected the form to stay open"))
      workflow.statusMessage shouldBe Some(s"Not a folder: $file")
      workflow.path shouldBe file.toString
      explorerRoots(after) shouldBe Nil
    }

  it should "say a missing path is not found" in
    inTempTree { (root, _, _) =>
      val missing = root.resolve("nowhere")
      val after   = withForm(missing)(stateManager => stateManager.applyEvent(ModalSubmit) >> settle(stateManager))

      workflowIn(after).flatMap(_.statusMessage) shouldBe Some(s"Folder not found: $missing")
    }

  "Tab in the Open Folder form" should "descend into the highlighted folder" in
    inTempTree { (root, folder, _) =>
      val descended = withForm(root) { stateManager =>
        stateManager.applyEvent(ModalSubmit) >>
          awaitState(stateManager)(state => workflowIn(state).exists(_.suggestions.nonEmpty)) >>
          stateManager.applyEvent(ModalNextField) >>
          awaitState(stateManager)(state => workflowIn(state).exists(_.path == folder.toString + separator))
      }

      workflowIn(descended).map(_.path) shouldBe Some(folder.toString + separator)
    }

  "Confirming in the Open Folder form" should "pin the shown folder as the Explorer root and close the form" in
    inTempTree { (_, folder, _) =>
      val finalState =
        withForm(folder)(stateManager => stateManager.applyEvent(ModalOpenAsProjectRoot) >> settle(stateManager))

      finalState.topModal shouldBe None
      explorerRoots(finalState) shouldBe List(folder)
    }

  it should "keep the form open and say so when the Path is a file" in
    inTempTree { (_, _, file) =>
      val after =
        withForm(file)(stateManager => stateManager.applyEvent(ModalOpenAsProjectRoot) >> settle(stateManager))

      workflowIn(after).flatMap(_.statusMessage) shouldBe Some(s"Not a directory: $file")
      explorerRoots(after) shouldBe Nil
    }
end OpenFolderStateManagerSpec
