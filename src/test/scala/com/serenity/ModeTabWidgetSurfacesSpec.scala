package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.SurfaceContent
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** The tab list and recent-in-mode list are summonable via the command runner (issue #1307), toggling open/closed like
  * the existing keyboard-shortcuts reference (`ToggleShortcutsHelp`) rather than needing a dedicated dismiss gesture.
  */
class ModeTabWidgetSurfacesSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("ModeTabWidgetSurfacesSpec"))
    StateManager.apply(logger).unsafeRunSync()

  "CommandRegistry" should "register the tab-list and recent-in-mode commands" in {
    val commandNames = CommandRegistry.default.getAllCommands.map(_.name)

    commandNames should contain("toggle-tab-list")
    commandNames should contain("toggle-recent-in-mode")
  }

  "toggle-tab-list" should "open the tab list, then close it on a second toggle" in {
    val stateManager = createStateManager()
    val command = Command.typed(
      "toggle-tab-list",
      "Show the list of open tabs.",
      CommandIntent.View(ViewIntent.ToggleTabList),
      CommandCategory.View
    )

    stateManager.executeCommand(command).unsafeRunSync()
    val opened = stateManager.getCurrentState.unsafeRunSync()
    opened.runtime.uiSurfaces.exists {
      case surface =>
        surface.content match
          case SurfaceContent.TabList(_, _) => true
          case _                            => false
    } shouldBe true

    stateManager.executeCommand(command).unsafeRunSync()
    val closed = stateManager.getCurrentState.unsafeRunSync()
    closed.runtime.uiSurfaces.exists {
      case surface =>
        surface.content match
          case SurfaceContent.TabList(_, _) => true
          case _                            => false
    } shouldBe false
  }

  "toggle-recent-in-mode" should "open the recent-in-mode list, then close it on a second toggle" in {
    val stateManager = createStateManager()
    val command = Command.typed(
      "toggle-recent-in-mode",
      "Show recent files opened in the current app mode.",
      CommandIntent.View(ViewIntent.ToggleRecentFilesInMode),
      CommandCategory.View
    )

    stateManager.executeCommand(command).unsafeRunSync()
    val opened = stateManager.getCurrentState.unsafeRunSync()
    opened.runtime.uiSurfaces.exists {
      case surface =>
        surface.content match
          case SurfaceContent.RecentFilesInMode(_, _) => true
          case _                                      => false
    } shouldBe true

    stateManager.executeCommand(command).unsafeRunSync()
    val closed = stateManager.getCurrentState.unsafeRunSync()
    closed.runtime.uiSurfaces.exists {
      case surface =>
        surface.content match
          case SurfaceContent.RecentFilesInMode(_, _) => true
          case _                                      => false
    } shouldBe false
  }
