package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.SurfaceContent
import com.serenity.ui.fonts.FontLoader.FontConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.{LoggerFactory, LoggerName}
import org.typelevel.log4cats.slf4j.Slf4jFactory

class CommandRunnerActivationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry = CommandRegistry.default

  "CommandRunner.activate" should "reflect non-default ligature settings in option selections" in {
    val config = AppConfig.default.withFontConfig(FontConfig(enableLigatures = false))
    val runner = CommandRunner.empty.activate(registry, config)

    runner.optionSelections.get("ligatures") shouldBe Some(1)
  }

  it should "reflect non-default code font family in option selections" in {
    val config = AppConfig.default.withFontConfig(FontConfig(codeFontFamily = "Courier New"))
    val runner = CommandRunner.empty.activate(registry, config)

    val expectedIndex = com.serenity.ui.fonts.FontLoader.availableMonospaceFamilies.indexOf("Courier New")
    if expectedIndex >= 0 then runner.optionSelections.get("code-font") shouldBe Some(expectedIndex)
    else succeed
  }

  "ensureCommandRunnerSurface (via closePane)" should "use the current config, not defaults" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val sm = com.serenity.state.manager.StateManager.apply(logger).unsafeRunSync()

    sm.updateState(s =>
      s.copy(config = s.config.withFontConfig(s.config.fontConfig.copy(enableLigatures = false)))
    ).unsafeRunSync()

    val stateBefore = sm.getCurrentState.unsafeRunSync()
    val paneId      = stateBefore.layout.editorPanes.keys.head

    sm.closePane(paneId).unsafeRunSync()

    val stateAfter = sm.getCurrentState.unsafeRunSync()
    val runner = stateAfter.commandRunnerSurface
      .map(_.content)
      .collect { case SurfaceContent.CommandPalette(r) => r }

    runner shouldBe defined
    runner.get.optionSelections.get("ligatures") shouldBe Some(1)
  }
