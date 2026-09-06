package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.state.manager.StateManager
import com.serenity.state.models.SurfacePresentation
import com.serenity.ui.layout.{DirectoryTreeData, PanelContent, PanelPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** The command/keyboard resize entry point (issue #1310): `ViewIntent.SetPanelSize` feeds the same
  * `PanelStateReducer.resize` the existing mouse-drag path already uses, via a plain `Command` -- the same generic
  * execution path the command palette itself runs through.
  */
class PanelResizeCommandSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def createStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("PanelResizeCommandSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def resizeCommand(surfaceId: com.serenity.state.models.SurfaceId, delta: Int): Command =
    Command.typed(
      "resize-focused-panel",
      "Resize the focused panel.",
      CommandIntent.View(ViewIntent.SetPanelSize(surfaceId, delta)),
      CommandCategory.View
    )

  "ViewIntent.SetPanelSize" should "grow a pinned panel by the given delta" in {
    val stateManager = createStateManager()
    stateManager
      .pinPanel(
        PanelContent.DirectoryTree(DirectoryTreeData(java.nio.file.Paths.get("/tmp")), None),
        PanelPosition.Left,
        24
      )
      .unsafeRunSync()
    val panelId = stateManager.getCurrentState.unsafeRunSync().pinnedSurfaces.head.id

    stateManager.executeCommand(resizeCommand(panelId, 6)).unsafeRunSync()

    val resized = stateManager.getCurrentState.unsafeRunSync().surfaceById(panelId).map(_.presentation)
    resized shouldBe Some(SurfacePresentation.Pinned(PanelPosition.Left, 30))
  }

  it should "shrink a pinned panel by a negative delta" in {
    val stateManager = createStateManager()
    stateManager
      .pinPanel(
        PanelContent.DirectoryTree(DirectoryTreeData(java.nio.file.Paths.get("/tmp")), None),
        PanelPosition.Left,
        24
      )
      .unsafeRunSync()
    val panelId = stateManager.getCurrentState.unsafeRunSync().pinnedSurfaces.head.id

    stateManager.executeCommand(resizeCommand(panelId, -6)).unsafeRunSync()

    val resized = stateManager.getCurrentState.unsafeRunSync().surfaceById(panelId).map(_.presentation)
    resized shouldBe Some(SurfacePresentation.Pinned(PanelPosition.Left, 18))
  }

  it should "clamp shrinking at the minimum panel size rather than going to zero or negative" in {
    val stateManager = createStateManager()
    stateManager
      .pinPanel(
        PanelContent.DirectoryTree(DirectoryTreeData(java.nio.file.Paths.get("/tmp")), None),
        PanelPosition.Left,
        5
      )
      .unsafeRunSync()
    val panelId = stateManager.getCurrentState.unsafeRunSync().pinnedSurfaces.head.id

    stateManager.executeCommand(resizeCommand(panelId, -100)).unsafeRunSync()

    val resized = stateManager.getCurrentState.unsafeRunSync().surfaceById(panelId).map(_.presentation)
    resized shouldBe Some(SurfacePresentation.Pinned(PanelPosition.Left, 4))
  }

  it should "no-op when the target surface isn't pinned" in {
    val stateManager = createStateManager()
    val before       = stateManager.getCurrentState.unsafeRunSync()

    stateManager
      .executeCommand(resizeCommand(com.serenity.state.models.SurfaceId("missing"), 6))
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync() shouldBe before
  }
