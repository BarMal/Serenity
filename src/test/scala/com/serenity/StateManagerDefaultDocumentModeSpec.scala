package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.config.DefaultDocumentMode
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateManagerDefaultDocumentModeSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerDefaultDocumentModeSpec"))
    StateManager(logger).unsafeRunSync()

  "StateManager default document mode commands" should "update the default document mode config" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "default-document-mode-markdown",
          "Set default document mode",
          CommandIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().config.defaultDocumentMode shouldBe DefaultDocumentMode.Markdown
  }
