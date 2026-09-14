package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Settings UI + intents for configurable line-number placement, margin, and padding. */
class LineNumberSettingsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def createStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("LineNumberSettingsSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def dispatch(stateManager: StateManager, intent: PanelChromeIntent): Unit =
    stateManager
      .executeCommand(
        Command.typed(
          "line-number-setting",
          "Line number setting.",
          CommandIntent.Settings(SettingsIntent.PanelChrome(intent)),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

  private def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
    group.children.flatMap {
      case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
      case child                               => List(child)
    }

  private def textDisplayItems(config: AppConfig): List[CommandSurfaceItem] =
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner            = CommandRunner.empty.activate(registry, config.withShowAllSettingsRegardlessOfMode(true))
    val groups            = runner.settingsGroups
    (groups ++ groups.flatMap(descendants).collect { case g: CommandSurfaceItem.GroupItem => g })
      .find(_.id == "settings-text-display")
      .map(_.children)
      .getOrElse(fail("missing settings-text-display group"))

  "CommandRunnerOptionSelections" should "select the current line-number side" in {
    def indexFor(side: LineNumberSide): Int =
      CommandRunnerOptionSelections.default(
        AppConfig.default.withLineNumberLayout(LineNumberLayout(side = side))
      )("line-number-side")

    indexFor(LineNumberSide.Left) shouldBe 0
    indexFor(LineNumberSide.Right) shouldBe 1
    indexFor(LineNumberSide.Both) shouldBe 2
  }

  "The Text Display settings group" should "carry the line-number side option and spacing inputs" in {
    val ids = textDisplayItems(AppConfig.default).map(_.id)
    ids should contain("line-number-side")
    ids should contain("line-number-margin-left")
    ids should contain("line-number-margin-right")
    ids should contain("line-number-padding")
  }

  "The line-number side option" should "offer left, right, and both" in {
    val item = textDisplayItems(AppConfig.default)
      .collectFirst {
        case option: CommandSurfaceItem.OptionItem if option.id == "line-number-side" => option
      }
      .getOrElse(fail("missing line-number-side option"))

    item.options.map(_.intent) shouldBe List(
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetLineNumberSide(LineNumberSide.Left))),
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetLineNumberSide(LineNumberSide.Right))),
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetLineNumberSide(LineNumberSide.Both)))
    )
  }

  "The margin/padding inputs" should "parse valid cells and reject out-of-range values" in {
    val padding = textDisplayItems(AppConfig.default)
      .collectFirst {
        case input: CommandSurfaceItem.InputItem if input.id == "line-number-padding" => input
      }
      .getOrElse(fail("missing line-number-padding input"))

    padding.parse("3") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetLineNumberPadding(3)))
    )
    padding.parse("-1") shouldBe None
    padding.parse((LineNumberLayout.MaxCells + 1).toString) shouldBe None
    padding.parse("abc") shouldBe None
  }

  "Dispatching line-number intents" should "update the config, clamping margins and padding" in {
    val stateManager = createStateManager()

    dispatch(stateManager, PanelChromeIntent.SetLineNumberSide(LineNumberSide.Both))
    dispatch(stateManager, PanelChromeIntent.SetLineNumberMarginLeft(4))
    dispatch(stateManager, PanelChromeIntent.SetLineNumberPadding(2))

    val layout = stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.lineNumberLayout
    layout.side shouldBe LineNumberSide.Both
    layout.marginLeft shouldBe 4
    layout.padding shouldBe 2

    dispatch(stateManager, PanelChromeIntent.SetLineNumberMarginRight(LineNumberLayout.MaxCells + 100))
    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .config
      .surfaceConfig
      .lineNumberLayout
      .marginRight shouldBe LineNumberLayout.MaxCells
  }
