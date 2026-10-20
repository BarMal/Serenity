package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import cats.syntax.all.*
import com.serenity.command.*
import com.serenity.config.{AppConfig, AppMode, ConfigManager}
import com.serenity.project.{ProjectTaskKind, ProjectTaskResult}
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.testkit.AwaitCondition.awaitValue
import com.serenity.{StateManagerTestSupport, TestTemp}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A project task is code tooling: prose mode has no way to start, watch or cancel one. Leaving code mode by choice
  * asks first, as closing a buffer with unsaved changes does; when the mode changes without anyone being asked -- the
  * config file edited from outside -- the task is stopped and the writer told.
  *
  * A command returns only once the lane work it started has settled, which a running task never does, so each command
  * here is sent without waiting for it and its effect is awaited in the state.
  */
class ModeSwitchProjectTaskSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  /** An editor with a build task that runs until it is cancelled, which it reports. */
  final private class Rig(
      val manager: StateManager,
      directory: Path,
      started: Deferred[IO, Unit],
      cancelled: Deferred[IO, Unit]
  ):

    def state: AppState = manager.getCurrentState.unsafeRunSync()

    def send(command: Command): Unit = manager.executeCommand(command).start.void.unsafeRunSync()

    def eventually[A](read: => A)(condition: A => Boolean): A =
      awaitValue(IO(read), 5.seconds)(condition).attempt.unsafeRunSync().getOrElse(read)

    def runBuild(): Unit =
      send(
        Command.typed(
          "project-build",
          "Build",
          CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Build)),
          CommandCategory.File
        )
      )
      started.get.timeout(5.seconds).unsafeRunSync()

    def switchTo(mode: AppMode): Unit =
      send(
        Command.typed(
          s"app-mode-${mode.configKey}",
          s"Switch to ${mode.configKey} mode",
          CommandIntent.View(ViewIntent.SetAppMode(mode)),
          CommandCategory.Settings
        )
      )

    def prompt: Option[ConfirmPrompt] =
      state.topModal.map(_.modal).collect { case Modal.Confirm(prompt) => prompt }

    def answer(label: String): Unit =
      prompt
        .flatMap(_.choices.items.find(_.label == label))
        .map(_.action)
        .collect { case ConfirmAction.Run(command) => command }
        .foreach(send)

    def appMode: AppMode = state.persisted.config.appMode

    def taskStopped: Boolean = cancelled.get.timeout(5.seconds).attempt.unsafeRunSync().isRight

    def taskLeftRunning: Boolean = cancelled.get.timeout(500.millis).attempt.unsafeRunSync().isLeft

    def notices: List[String] =
      eventually(state.runtime.uiSurfaces.map(_.content).collect {
        case SurfaceContent.Notice(notice, _) =>
          notice.message
      })(_.nonEmpty)

    def reloadConfig(mode: AppMode): Unit =
      Files.writeString(
        directory.resolve("config.conf"),
        ConfigManager.configToString(AppConfig.default.withAppMode(mode))
      )
      manager.fileService.configWatch.traverse_(_.reload).unsafeRunSync()

    def close(): Unit = manager.runtimeLifecycle.forceQuit.timeout(10.seconds).attempt.unsafeRunSync(): Unit

  private def withRig(test: Rig => Unit): Unit =
    val directory = TestTemp.directory("mode-switch-project-task")
    Files.writeString(directory.resolve("Makefile"), "all:\n\ttrue\n")
    val rig =
      (for
        started   <- Deferred[IO, Unit]
        cancelled <- Deferred[IO, Unit]
        manager <- adjustedStateManager(
          _.copy(
            runProjectTask =
              (_, _) => started.complete(()) >> IO.never[ProjectTaskResult].onCancel(cancelled.complete(()).void),
            configPersistencePath = Some(directory.resolve("config.conf")),
            configOnDisk = Some(AppConfig.default)
          )
        )
        _ <- manager.setBufferFilePath(BufferId(0), directory.resolve("main.c"))
      yield Rig(manager, directory, started, cancelled)).unsafeRunSync()
    try test(rig)
    finally rig.close()

  private val stopAndSwitch = "Stop task and switch to Prose"
  private val stay          = "Stay in Code mode"

  "Switching to prose while a project task runs" should "ask first, and change nothing until answered" in withRig {
    rig =>
      rig.runBuild()

      rig.switchTo(AppMode.Prose)

      rig.eventually(rig.prompt)(_.isDefined).map(_.choices.items.map(_.label)) shouldBe Some(List(stay, stopAndSwitch))
      rig.appMode shouldBe AppMode.Code
      rig.state.runtime.projectTasks.running should not be empty
      rig.taskLeftRunning shouldBe true
  }

  it should "stop the task, switch, and say so when the writer agrees" in withRig { rig =>
    rig.runBuild()
    rig.switchTo(AppMode.Prose)
    rig.eventually(rig.prompt)(_.isDefined)

    rig.answer(stopAndSwitch)

    rig.eventually(rig.appMode)(_ == AppMode.Prose) shouldBe AppMode.Prose
    rig.taskStopped shouldBe true
    rig.state.runtime.projectTasks.running shouldBe None
    rig.notices.exists(_.contains("build task")) shouldBe true
  }

  it should "stay in code mode with the task running when the writer declines or dismisses" in withRig { rig =>
    rig.runBuild()
    rig.switchTo(AppMode.Prose)
    val prompt = rig.eventually(rig.prompt)(_.isDefined)

    prompt.flatMap(_.choices.items.find(_.label == stay)).map(_.action) shouldBe Some(ConfirmAction.Dismiss)
    prompt.map(_.onDismiss) shouldBe Some(ConfirmAction.Dismiss)
    rig.appMode shouldBe AppMode.Code
    rig.state.runtime.projectTasks.running should not be empty
  }

  it should "not ask when no task is running" in withRig { rig =>
    rig.switchTo(AppMode.Prose)

    rig.eventually(rig.appMode)(_ == AppMode.Prose) shouldBe AppMode.Prose
    rig.prompt shouldBe None
  }

  "The config file putting the workspace in prose mode while a project task runs" should "stop the task and say so" in
    withRig { rig =>
      rig.runBuild()

      rig.reloadConfig(AppMode.Prose)

      rig.eventually(rig.appMode)(_ == AppMode.Prose) shouldBe AppMode.Prose
      rig.taskStopped shouldBe true
      rig.state.runtime.projectTasks.running shouldBe None
      rig.notices.exists(_.contains("build task")) shouldBe true
    }
