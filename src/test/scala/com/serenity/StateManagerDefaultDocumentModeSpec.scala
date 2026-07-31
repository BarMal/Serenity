package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.config.DefaultDocumentMode
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.PaneId
import com.serenity.ui.layout.{WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
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

  "StateManager startup" should "retain the explicit default workspace tree" in {
    val stateManager = createStateManager()

    stateManager.getCurrentState.unsafeRunSync().layout.workspaceTree shouldBe Some(
      WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), PaneId(0)))
    )
  }

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
