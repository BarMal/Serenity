package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.AppMode
import com.serenity.project.ProjectTaskKind
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{BufferId, SurfaceContent, SurfacePresentation}
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

      val terminalText = stateManager.getCurrentState
        .unsafeRunSync()
        .pinnedSurfaces
        .collectFirst {
          case surface if surface.presentation == SurfacePresentation.Pinned(PanelPosition.Bottom, 14) =>
            surface.content
        }
        .collect { case SurfaceContent.Terminal(buffer, _) => buffer }
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

    val terminalText = stateManager.getCurrentState
      .unsafeRunSync()
      .pinnedSurfaces
      .collectFirst {
        case surface if surface.presentation == SurfacePresentation.Pinned(PanelPosition.Bottom, 14) =>
          surface.content
      }
      .collect { case SurfaceContent.Terminal(buffer, _) => buffer }
      .getOrElse(fail("Expected bottom terminal panel"))

    terminalText should include("not available in prose mode")
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

  // See StateManagerRuntimeSpec ("should cancel a running project task when its output panel is closed") for the
  // deterministic version of this: it needs a fake never-completing fiber rather than a real spawned process, so it
  // lives alongside the other project-task-fiber tests that already build a StateManagerComposition directly.
