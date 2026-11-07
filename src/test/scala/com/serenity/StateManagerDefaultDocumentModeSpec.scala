package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent, ViewIntent}
import com.serenity.config.{AppConfig, DefaultDocumentMode}
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.PaneId
import com.serenity.ui.layout.{WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateManagerDefaultDocumentModeSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def createStateManager(): StateManager =
    createStateManager("StateManagerDefaultDocumentModeSpec", initialConfig = AppConfig.default)

  "StateManager startup" should "retain the explicit default workspace tree" in {
    val stateManager = createStateManager()

    stateManager.getCurrentState.unsafeRunSync().persisted.layout.workspaceTree shouldBe Some(
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
          CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown)),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .config
      .defaultDocumentMode shouldBe DefaultDocumentMode.Markdown
  }
