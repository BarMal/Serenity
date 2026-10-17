package com.serenity.app

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.command.{CommandIntent, InterfaceChromeIntent, LayoutOverrideCommands, SettingsIntent}
import com.serenity.config.{AppConfig, ConfigManagerTestSupport, LayoutPin, UiLayout}
import com.serenity.diagnostics.PreviousRun
import com.serenity.state.models.{AppState, ConfirmAction, ConfirmPrompt, Modal}
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import com.serenity.{StateManagerTestSupport, TestTemp}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The first launch after chrome keys stopped being written for everyone: a file written by an earlier build pins them,
  * so the layout it now starts from does nothing until the user is asked once.
  */
class LayoutOverrideReviewSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private val pinned =
    "config.version = 2\neditor.line_numbers = true\nstatus.placement = pinned\neditor.tab_size = 3\n"

  private def configFile(text: String): Path =
    val file = TestTemp.directory("layout-review-config").resolve("config.conf")
    Files.writeString(file, text)
    file

  private def freshMarker: Path = TestTemp.directory("layout-review-marker").resolve("layout-override-reviewed")

  private def configOf(file: Path): AppConfig = ConfigManagerTestSupport.loadConfig(Some(file.toString))

  private def due(marker: Path, file: Path): List[LayoutPin] =
    val config = configOf(file)
    LayoutOverrideReview.due(marker, file, config).unsafeRunSync()

  "The first launch review" should "name the keys the file pins away from the active layout" in {
    due(freshMarker, configFile(pinned)) shouldBe List(
      LayoutPin("editor.line_numbers", current = "true", layoutValue = "false"),
      LayoutPin("status.placement", current = "pinned", layoutValue = "off")
    )
  }

  it should "be due only once, whatever was answered" in {
    val marker = freshMarker
    val file   = configFile(pinned)

    due(marker, file) should not be empty
    Files.exists(marker) shouldBe true
    due(marker, file) shouldBe Nil
  }

  it should "count as answered when nothing is pinned, so a later pin is not asked about" in {
    val marker = freshMarker
    val file   = configFile("config.version = 2\neditor.tab_size = 3\n")

    due(marker, file) shouldBe Nil
    Files.exists(marker) shouldBe true
    Files.writeString(file, pinned)
    due(marker, file) shouldBe Nil
  }

  it should "ask nothing of a first launch that has no config file yet" in {
    val marker = freshMarker
    val absent = TestTemp.directory("layout-review-absent").resolve("config.conf")

    LayoutOverrideReview.due(marker, absent, AppConfig.default).unsafeRunSync() shouldBe Nil
    Files.exists(marker) shouldBe true
  }

  it should "never nag when it cannot remember the answer" in {
    val blocker = TestTemp.file("layout-review-blocker", ".txt")
    val marker  = blocker.resolve("layout-override-reviewed")

    due(marker, configFile(pinned)) shouldBe Nil
  }

  it should "ask nothing of a file it cannot parse, and leave no marker so the next launch tries again" in {
    val marker = freshMarker
    val file   = configFile("this is not = valid = hocon {\n")

    LayoutOverrideReview.due(marker, file, AppConfig.default).unsafeRunSync() shouldBe Nil
    Files.exists(marker) shouldBe false
  }

  it should "ask nothing of a config path it cannot read, and leave no marker" in {
    val marker    = freshMarker
    val directory = TestTemp.directory("layout-review-unreadable")

    LayoutOverrideReview.due(marker, directory, AppConfig.default).unsafeRunSync() shouldBe Nil
    Files.exists(marker) shouldBe false
  }

  it should "ask once the file can be read again, and only then leave the marker" in {
    val marker = freshMarker
    val file   = configFile("this is not = valid = hocon {\n")
    LayoutOverrideReview.due(marker, file, AppConfig.default).unsafeRunSync() shouldBe Nil

    Files.writeString(file, pinned)

    due(marker, file) should not be empty
    Files.exists(marker) shouldBe true
    due(marker, file) shouldBe Nil
  }

  it should "say why it could not read the file" in {
    val marker = freshMarker
    val file   = configFile("this is not = valid = hocon {\n")

    val review = LayoutOverrideReview.review(marker, file, AppConfig.default).unsafeRunSync()

    review.left.map(_.path) shouldBe Left(file)
  }

  "The first launch prompt" should "offer the layout, a choice per key, or keeping the file as it is" in {
    val pins   = List(LayoutPin("editor.line_numbers", "true", "false"))
    val prompt = ConfirmPrompt.layoutOverrideAtStartup(UiLayout.Minimal, pins)

    prompt.blocking shouldBe true
    prompt.message should contain("Editor Line Numbers: true -> false")
    prompt.choices.items.map(_.label).toList shouldBe
      List("Use Minimal for all", "Choose one by one", "Keep my settings")
    prompt.choices.items.map(_.action).toList shouldBe List(
      ConfirmAction.Run(LayoutOverrideCommands.useLayout(UiLayout.Minimal, pins)),
      ConfirmAction.Run(LayoutOverrideCommands.chooseEach(UiLayout.Minimal)),
      ConfirmAction.Dismiss
    )
    prompt.onDismiss shouldBe ConfirmAction.Dismiss
  }

  "The switch prompt" should "also offer to keep the user's settings while switching, and to cancel" in {
    val pins   = List(LayoutPin("editor.line_numbers", "false", "true"))
    val prompt = ConfirmPrompt.layoutOverride(UiLayout.Conventional, pins)

    prompt.choices.items.map(_.label).toList shouldBe
      List("Use Conventional for all", "Keep my settings", "Choose one by one", "Cancel")
    prompt.choices.items.map(_.action).toList shouldBe List(
      ConfirmAction.Run(LayoutOverrideCommands.useLayout(UiLayout.Conventional, pins)),
      ConfirmAction.Run(LayoutOverrideCommands.keepMine(UiLayout.Conventional)),
      ConfirmAction.Run(LayoutOverrideCommands.chooseEach(UiLayout.Conventional)),
      ConfirmAction.Dismiss
    )
    prompt.onDismiss shouldBe ConfirmAction.Dismiss
  }

  "The layout commands" should "confirm the layout with the keys they release" in {
    val pins = List(LayoutPin("editor.line_numbers", "true", "false"), LayoutPin("status.placement", "pinned", "off"))

    LayoutOverrideCommands.useLayout(UiLayout.Minimal, pins).intent shouldBe CommandIntent.Settings(
      SettingsIntent.InterfaceChrome(
        InterfaceChromeIntent.ConfirmUiLayoutOverride(UiLayout.Minimal, List("editor.line_numbers", "status.placement"))
      )
    )
    LayoutOverrideCommands.keepMine(UiLayout.Minimal).intent shouldBe CommandIntent.Settings(
      SettingsIntent.InterfaceChrome(InterfaceChromeIntent.ConfirmUiLayoutOverride(UiLayout.Minimal, Nil))
    )
    LayoutOverrideCommands.chooseEach(UiLayout.Minimal).intent shouldBe CommandIntent.Settings(
      SettingsIntent.InterfaceChrome(InterfaceChromeIntent.ReviewUiLayoutOverride(UiLayout.Minimal))
    )
  }

  private val pins = List(LayoutPin("editor.line_numbers", "true", "false"))
  private val base = AppState.empty(AppConfig.default)

  "A recovery plan with pins" should "show the prompt in the first state" in {
    val applied = StartupRecovery.Plan.normal.copy(layoutPins = pins).appliedTo(base)

    applied.runtime.modalStack.map(_.modal) shouldBe
      List(Modal.Confirm(ConfirmPrompt.layoutOverrideAtStartup(UiLayout.Minimal, pins)))
  }

  it should "leave a more urgent prompt on top of it" in {
    val plan = StartupRecovery
      .plan(LaunchOptions(), StartupCrashGuard.Decision.Proceed, Nil, Nil, PreviousRun.Abnormal("the report"))
      .copy(layoutPins = pins)

    plan.appliedTo(base).runtime.modalStack.map(_.modal) shouldBe List(
      Modal.Confirm(ConfirmPrompt.layoutOverrideAtStartup(UiLayout.Minimal, pins)),
      Modal.Confirm(ConfirmPrompt.closedUnexpectedly("the report"))
    )
  }

  it should "not be the normal plan, so startup applies it" in {
    StartupRecovery.Plan.normal.copy(layoutPins = pins) should not be StartupRecovery.Plan.normal
  }

  "Startup with the prompt" should "keep the start page under it, to be reached once the prompt is dismissed" in {
    val stateManager = createStateManager("LayoutOverrideReviewSpec-startup")

    val state = AppStartup
      .initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        Theme.default,
        ViewportSize(80, 24),
        appConfig = AppConfig.default,
        recovery = StartupRecovery.Plan.normal.copy(layoutPins = pins)
      )
      .unsafeRunSync()

    state.startPageSurface should not be empty
    state.runtime.modalStack.map(_.modal) should have size 1
  }
