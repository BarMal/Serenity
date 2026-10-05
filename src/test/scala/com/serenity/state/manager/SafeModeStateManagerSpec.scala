package com.serenity.state.manager

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.{CommandRegistry, SafeModeCommands}
import com.serenity.config.AppConfig
import com.serenity.project.{ProjectTaskCommand, ProjectTaskKind}
import com.serenity.rope.Balance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class SafeModeStateManagerSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val logger = LoggerFactory[IO].getLogger(using LoggerName("SafeModeStateManagerSpec"))

  private def run(manager: StateManager, name: String): Unit =
    manager.executeCommand(CommandRegistry.default.findCommand(name).get).unsafeRunSync()

  "Restart in Safe Mode" should "ask the launcher to restart, then quit" in {
    val restarts = Ref.unsafe[IO, Int](0)
    val manager =
      StateManager.apply(logger, restartInSafeMode = Some(restarts.update(_ + 1))).unsafeRunSync()

    manager.executeCommand(SafeModeCommands.restart).unsafeRunSync()

    restarts.get.unsafeRunSync() shouldBe 1
    manager.runtimeLifecycle.awaitQuit.unsafeRunSync()
  }

  it should "leave a session that cannot restart running" in {
    val manager = StateManager.apply(logger).unsafeRunSync()

    manager.executeCommand(SafeModeCommands.restart).unsafeRunSync()

    manager.getCurrentState.unsafeRunSync().commandRunnerSurface shouldBe None
  }

  "Reset Settings" should "keep the old config file as a timestamped backup and write the defaults" in {
    val folder = Files.createTempDirectory("serenity-reset-settings")
    val config = folder.resolve("config.conf")
    Files.writeString(config, "editor.word_wrap = false\n", StandardCharsets.UTF_8)
    val manager = StateManager.apply(logger, configPersistencePath = Some(config)).unsafeRunSync()

    run(manager, "reset-settings")
    manager.runtimeLifecycle.awaitEffects.unsafeRunSync()

    val backups = backupsIn(folder)
    backups.map(Files.readString(_, StandardCharsets.UTF_8)) shouldBe List("editor.word_wrap = false\n")
    Files.readString(config, StandardCharsets.UTF_8) should include("editor.word_wrap = true")
    manager.getCurrentState.unsafeRunSync().persisted.config shouldBe AppConfig.default
  }

  it should "change nothing when there is no config file behind the session" in {
    val manager = StateManager.apply(logger).unsafeRunSync()
    val before  = manager.getCurrentState.unsafeRunSync().persisted.config

    run(manager, "reset-settings")

    manager.getCurrentState.unsafeRunSync().persisted.config shouldBe before
  }

  "A state manager without project tasks" should "fail to launch one instead of running it" in {
    val manager = StateManager.apply(logger, projectTasksEnabled = false).unsafeRunSync()
    val command = ProjectTaskCommand(ProjectTaskKind.Test, "make", Path.of("."), "true", Nil)

    manager.composition.runProjectTask(command, _ => IO.unit).attempt.unsafeRunSync().isLeft shouldBe true
  }

  private def backupsIn(folder: Path): List[Path] =
    val stream = Files.list(folder)
    try stream.toArray.toList.collect { case path: Path if path.getFileName.toString.contains(".backup-") => path }
    finally stream.close()
