package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{CommandCategory, CommandIntent, CommandRegistry, ViewIntent}
import com.serenity.config.AppMode
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.testkit.SharedDictionary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** The mode a workspace is in decides its typography, panels and tooling, yet it was reachable only by digging through
  * the settings tree (#2570). Both modes now have a command the palette finds by "mode".
  */
class AppModeSwitchCommandsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registries = List("default" -> CommandRegistry.default, "withToggleUI" -> CommandRegistry.withToggleUI)

  private val modes = List(
    ("switch-to-prose-mode", "Switch to Prose Mode", AppMode.Prose),
    ("switch-to-code-mode", "Switch to Code Mode", AppMode.Code)
  )

  "Mode switch commands" should "be registered in every registry, labelled and set to the mode they name" in {
    for
      (registryName, registry) <- registries
      (id, label, mode)        <- modes
    do
      withClue(s"$registryName / $id: ") {
        val command = registry.findCommand(id)
        command.map(_.intent) shouldBe Some(CommandIntent.View(ViewIntent.SetAppMode(mode)))
        command.map(_.label) shouldBe Some(label)
        command.map(_.category) shouldBe Some(CommandCategory.View)
      }
  }

  it should "be found by searching for \"mode\"" in {
    val found = CommandRegistry.withToggleUI.searchCommands("mode", maxResults = 100).map(_.name)

    found should contain allOf ("switch-to-prose-mode", "switch-to-code-mode")
  }

  it should "change the workspace's app mode when executed, in either direction" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager        = StateManager.apply(logger, dictionaryCache = SharedDictionary.default).unsafeRunSync()
    def appMode             = stateManager.getCurrentState.unsafeRunSync().persisted.config.appMode
    def run(id: String) =
      stateManager.executeCommand(CommandRegistry.withToggleUI.findCommand(id).get).unsafeRunSync()

    run("switch-to-prose-mode")
    appMode shouldBe AppMode.Prose
    run("switch-to-code-mode")
    appMode shouldBe AppMode.Code
  }
