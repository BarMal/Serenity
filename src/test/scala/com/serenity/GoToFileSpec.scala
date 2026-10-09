package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.state.reducers.PinnedPanelContentReducer
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.PanelPosition
import com.serenity.ui.widget.Loadable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** "Go to File" end to end: the hotkey's event opens the finder over the docked explorer's root, the listing lands from
  * its lane, typing ranks it, and Enter opens the highlighted file through the ordinary open path.
  */
class GoToFileSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def project(files: (String, String)*): Path =
    val root = TestTemp.directory("go-to-file")
    files.foreach { (relative, content) =>
      val file = root.resolve(relative)
      Files.createDirectories(file.getParent)
      Files.writeString(file, content)
    }
    root

  private def stateManagerRootedAt(root: Path): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("GoToFileSpec"))
    val stateManager =
      StateManager(
        logger,
        sessionRootOverride = Some(TestTemp.directory("go-to-file-session")),
        dictionaryCache = SharedDictionary.default
      ).unsafeRunSync()
    stateManager
      .updateState(PinnedPanelContentReducer.pinExplorerRoot(PanelPosition.Left, root, 30, _).state)
      .unsafeRunSync()
    stateManager

  private def finder(stateManager: StateManager): Option[ListPicker] =
    stateManager.getCurrentState.unsafeRunSync().modalSurface.map(_.content).collect {
      case SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)) => picker
    }

  private def labels(stateManager: StateManager): Vector[String] =
    finder(stateManager).flatMap(_.items.toOption).fold(Vector.empty)(_.items.map(_.label))

  private def settled(stateManager: StateManager, event: Event): Unit =
    (stateManager.applyEvent(event) >> stateManager.runtimeLifecycle.awaitEffects).unsafeRunSync()

  "Go to File" should "list the project's files, rank them as the user types, and open the picked one" in {
    val root = project(
      "docs/guide.md"          -> "a guide",
      "src/main/App.scala"     -> "object App",
      "target/stale/App.class" -> "compiled"
    )
    val stateManager = stateManagerRootedAt(root)

    settled(stateManager, GoToFile)
    finder(stateManager).map(_.title) shouldBe Some("Go to File")
    labels(stateManager) shouldBe Vector("guide.md", "App.scala")

    "app".foreach(char => settled(stateManager, InsertChar(char)))
    labels(stateManager) shouldBe Vector("App.scala")

    settled(stateManager, Enter)
    val opened = stateManager.getCurrentState.unsafeRunSync()
    opened.modalSurface shouldBe None
    opened.activeBuffer.flatMap(_.document.filePath) shouldBe Some(root.resolve("src/main/App.scala"))
    opened.activeBuffer.map(_.document.content.collect()) shouldBe Some("object App")
  }

  it should "show why the project root couldn't be listed" in {
    val root         = project("a.txt" -> "a")
    val stateManager = stateManagerRootedAt(root.resolve("missing"))

    settled(stateManager, GoToFile)

    finder(stateManager).map(_.items) should matchPattern { case Some(Loadable.Failed(_)) => }
  }
