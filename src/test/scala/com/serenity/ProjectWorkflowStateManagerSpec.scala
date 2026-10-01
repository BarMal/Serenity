package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.AppMode
import com.serenity.keystroke.events.ToggleCommandRunner
import com.serenity.project.{ProjectPresence, ProjectTaskKind}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.{AppState, BufferId, SurfaceContent}
import com.serenity.ui.layout.PanelPosition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class ProjectWorkflowStateManagerSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("ProjectWorkflowStateManagerSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def bottomTerminalText(state: com.serenity.state.models.AppState): Option[String] =
    state.pinnedSurfaces
      .collectFirst {
        case surface @ com.serenity.state.models.UiSurface(_, SurfaceContent.Terminal(buffer, _), _, _)
            if state.persisted.layout.workspaceTree
              .flatMap(_.positionForSurface(surface.id))
              .contains(
                PanelPosition.Bottom
              ) =>
          buffer
      }

  "Project workflow commands" should "be registered under the project category" in {
    val commands = CommandRegistry.default.commandsForCategory(CommandCategory.Project)

    commands.map(_.name) should contain allOf (
      "project-build",
      "project-test",
      "project-run",
      "project-debug",
      "project-dependencies",
      "project-cancel"
    )
  }

  it should "describe the debug command as running a debug task without changing its identity" in {
    val command = CommandRegistry.default.findCommand("project-debug").getOrElse(fail("Missing project-debug command"))

    command.label shouldBe "Run Debug Task"
    command.description shouldBe "Launch the detected project through its debug task."
    command.intent shouldBe CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Debug))
  }

  it should "pin a terminal status when no project task can be detected" in {
    val tempRoot = Files.createTempDirectory("no-project-workflow")
    try
      val stateManager = createStateManager()
      val bufferPath   = tempRoot.resolve("notes.txt")
      Files.writeString(bufferPath, "notes")

      stateManager
        .updateState { state =>
          val buffer  = state.persisted.buffers(BufferId(0))
          val updated = buffer.copy(document = buffer.document.copy(filePath = Some(bufferPath)))
          state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (BufferId(0) -> updated)))
        }
        .unsafeRunSync()

      stateManager
        .executeCommand(
          Command.typed(
            "project-build",
            "Build the detected project.",
            CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Build)),
            CommandCategory.Project
          )
        )
        .unsafeRunSync()

      val terminalText = bottomTerminalText(stateManager.getCurrentState.unsafeRunSync())
        .getOrElse(fail("Expected bottom terminal panel"))

      terminalText should include("No build task found")
      terminalText should include(tempRoot.toString)
      terminalText should include("build.sbt")
    finally
      val stream = Files.walk(tempRoot)
      try stream.toArray.toList.map(_.asInstanceOf[Path]).sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists)
      finally stream.close()
  }

  it should "not run a project task while the app is in prose mode" in {
    val stateManager = createStateManager()
    stateManager
      .executeCommand(
        Command.typed(
          "app-mode-prose",
          "Switch to prose mode",
          CommandIntent.View(ViewIntent.SetAppMode(AppMode.Prose)),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager
      .executeCommand(
        Command.typed(
          "project-run",
          "Run the detected project.",
          CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Run)),
          CommandCategory.Project
        )
      )
      .unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    bottomTerminalText(state) shouldBe None
    state.peekSurface.map(_.content) shouldBe Some(SurfaceContent.QuickInfo("Only available in code mode."))
  }

  private def withTempRoot[A](prefix: String)(use: Path => A): A =
    val root = Files.createTempDirectory(prefix)
    try use(root)
    finally
      val stream = Files.walk(root)
      try stream.toArray.toList.map(_.asInstanceOf[Path]).sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists)
      finally stream.close()

  private def paletteOpenedOn(bufferPath: Path): AppState =
    val stateManager = createStateManager()
    stateManager.setBufferFilePath(BufferId(0), bufferPath).unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync()

  private def projectBuildItem(state: AppState): CommandSurfaceItem.CommandItem =
    state.commandRunnerSurface
      .map(_.content)
      .collect { case SurfaceContent.CommandPalette(runner) => runner.visibleItems }
      .getOrElse(fail("expected the command palette to be open"))
      .collectFirst { case item: CommandSurfaceItem.CommandItem if item.command.name == "project-build" => item }
      .getOrElse(fail("expected project-build to stay listed"))

  it should "detect, on opening the palette, that no project encloses the file being edited" in
    withTempRoot("no-project-palette") { root =>
      val bufferPath = Files.writeString(root.resolve("notes.txt"), "notes")

      val state = paletteOpenedOn(bufferPath)

      state.runtime.projectPresence shouldBe ProjectPresence.NotDetected
      projectBuildItem(state).disabledReason shouldBe Some("No project detected.")
    }

  it should "detect, on opening the palette, the project enclosing the file being edited" in
    withTempRoot("project-palette") { root =>
      Files.writeString(root.resolve("build.sbt"), "scalaVersion := \"3.5.0\"")
      val bufferPath = Files.writeString(root.resolve("Main.scala"), "object Main")

      val state = paletteOpenedOn(bufferPath)

      state.runtime.projectPresence shouldBe ProjectPresence.Detected
      projectBuildItem(state).disabledReason shouldBe None
    }

  it should "update the project terminal panel in place rather than pinning a new surface each refresh" in {
    val stateManager = createStateManager()

    stateManager.pinOrUpdateTerminalPanel("first output", PanelPosition.Bottom, 14).unsafeRunSync()
    stateManager.pinOrUpdateTerminalPanel("second output", PanelPosition.Bottom, 14).unsafeRunSync()

    val terminalSurfaces = stateManager.getCurrentState
      .unsafeRunSync()
      .pinnedSurfaces
      .filter(_.content match
        case SurfaceContent.Terminal(_, _) => true
        case _                             => false)

    terminalSurfaces should have size 1
    terminalSurfaces.head.content shouldBe SurfaceContent.Terminal("second output", "second output".length)
  }

  // See StateManagerRuntimeSpec ("should keep a running project task going when its output panel is closed") for the
  // deterministic version of this: it needs a fake never-completing fiber rather than a real spawned process, so it
  // lives alongside the other project-task-fiber tests that already build a StateManagerComposition directly.
