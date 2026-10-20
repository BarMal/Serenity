package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.IndentationSettings
import com.serenity.testkit.SharedDictionary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** The indentation settings: where they sit in the settings tree, how their rows parse, and how they reach the config
  * (#1996).
  */
class IndentationSettingsTreeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def createStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("IndentationSettingsTreeSpec"))
    StateManager.apply(logger, dictionaryCache = SharedDictionary.default).unsafeRunSync()

  private def dispatch(stateManager: StateManager, intent: TextDisplayIntent): Unit =
    stateManager
      .executeCommand(
        Command.typed(
          "indentation-setting",
          "Indentation setting.",
          CommandIntent.Settings(SettingsIntent.TextDisplay(intent)),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

  private def indentationGroup(config: AppConfig): CommandSurfaceItem.GroupItem =
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner            = CommandRunner.empty.activate(registry, config.withShowAllSettingsRegardlessOfMode(true))
    runner.settingsGroups
      .flatMap(group => group :: group.children.collect { case nested: CommandSurfaceItem.GroupItem => nested })
      .find(_.id == "settings-indentation")
      .getOrElse(fail("missing settings-indentation group"))

  "The Indentation settings group" should "sit in the editor settings with the tab size and the spaces switch" in {
    val group = indentationGroup(AppConfig.default)

    group.label shouldBe "Indentation"
    group.children.map(_.id) shouldBe List("tab-size", "insert-spaces")
  }

  it should "show the configured tab size and whether spaces are inserted" in {
    val group =
      indentationGroup(AppConfig.default.withIndentation(IndentationSettings(tabSize = 6, insertSpaces = false)))

    group.children.collectFirst { case input: CommandSurfaceItem.InputItem => input.currentValue } shouldBe Some("6")
    group.children.collectFirst { case option: CommandSurfaceItem.OptionItem => option.selectedOption } shouldBe
      Some("Off")
  }

  "The tab size row" should "parse sizes in range and reject the rest" in {
    val tabSize = indentationGroup(AppConfig.default).children
      .collectFirst { case input: CommandSurfaceItem.InputItem if input.id == "tab-size" => input }
      .getOrElse(fail("missing tab-size input"))

    tabSize.parse("8") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetTabSize(8)))
    )
    tabSize.parse(" 2 ") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetTabSize(2)))
    )
    tabSize.parse("0") shouldBe None
    tabSize.parse((IndentationSettings.MaxTabSize + 1).toString) shouldBe None
    tabSize.parse("wide") shouldBe None
    tabSize.defaultValue shouldBe Some("4")
  }

  "The insert spaces row" should "switch the intent on and off" in {
    val insertSpaces = indentationGroup(AppConfig.default).children
      .collectFirst { case option: CommandSurfaceItem.OptionItem if option.id == "insert-spaces" => option }
      .getOrElse(fail("missing insert-spaces option"))

    insertSpaces.options.map(_.intent) shouldBe List(
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetInsertSpaces(true))),
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetInsertSpaces(false)))
    )
  }

  "CommandRunnerOptionSelections" should "select On when spaces are inserted and Off when they are not" in {
    CommandRunnerOptionSelections.default(AppConfig.default)("insert-spaces") shouldBe 0
    CommandRunnerOptionSelections.default(
      AppConfig.default.withIndentation(IndentationSettings(insertSpaces = false))
    )("insert-spaces") shouldBe 1
  }

  "Dispatching the indentation intents" should "update the config" in {
    val stateManager = createStateManager()

    dispatch(stateManager, TextDisplayIntent.SetTabSize(2))
    dispatch(stateManager, TextDisplayIntent.SetInsertSpaces(false))

    stateManager.getCurrentState.unsafeRunSync().persisted.config.indentation shouldBe
      IndentationSettings(tabSize = 2, insertSpaces = false)
  }
