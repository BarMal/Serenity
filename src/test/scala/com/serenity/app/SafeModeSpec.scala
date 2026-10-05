package com.serenity.app

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{CommandRegistry, SafeModeCommands}
import com.serenity.config.AppConfig
import com.serenity.lsp.config.{LanguageId, LspServerRegistry}
import com.serenity.state.models.{AppState, ConfirmAction, ConfirmPrompt, Modal, StatusLineText}
import com.serenity.ui.presets.{UiPreset, UiPresetStore}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SafeModeSpec extends AnyFlatSpec with Matchers:

  "SafeMode.config" should "start no language server for any language" in {
    val servers = SafeMode.config.languageToolsConfig.lspUserConfig
    LanguageId.values.toList.flatMap(LspServerRegistry.configuredServer(_, servers)) shouldBe Nil
  }

  it should "leave spell check off" in {
    SafeMode.config.languageToolsConfig.spellCheck.enabled shouldBe false
  }

  it should "otherwise be the default configuration" in {
    SafeMode.config.withLanguageToolsConfig(AppConfig.default.languageToolsConfig) shouldBe AppConfig.default
  }

  "SafeMode.scratchSessionRoot" should "be an empty directory that is gone once the session ends" in {
    val (wasEmpty, root) = SafeMode.scratchSessionRoot
      .use(root => IO.blocking((Files.list(root).count() == 0L, root)))
      .unsafeRunSync()

    wasEmpty shouldBe true
    Files.exists(root) shouldBe false
  }

  "StartupRecovery.Plan.noticeWith" should "show the safe-mode indicator ahead of any config notice" in {
    val plan = StartupRecovery.Plan(safeMode = true, notices = List(SafeMode.Notice))
    plan.noticeWith(Some("Configuration could not be read.")) shouldBe
      Some(s"${SafeMode.Notice} Configuration could not be read.")
    plan.noticeWith(None) shouldBe Some(SafeMode.Notice)
    SafeMode.Notice should startWith("Safe mode")
  }

  it should "add nothing for a normal launch" in {
    StartupRecovery.Plan(safeMode = false, notices = Nil).noticeWith(None) shouldBe None
  }

  "StartupRecovery.Plan.configPersistencePath" should "never write the config file in safe mode" in {
    val path = Files.createTempDirectory("serenity-safe").resolve("config.conf")
    StartupRecovery.Plan(safeMode = true, notices = Nil).configPersistencePath(path) shouldBe None
    StartupRecovery.Plan(safeMode = false, notices = Nil).configPersistencePath(path) shouldBe Some(path)
  }

  "StartupRecovery.plan" should "put safe mode on only when it was asked for or a crash loop took it over" in {
    val normal = StartupRecovery.plan(LaunchOptions(), StartupCrashGuard.Decision.Proceed, Nil, Nil)
    val safe   = StartupRecovery.plan(LaunchOptions(safeMode = true), StartupCrashGuard.Decision.Proceed, Nil, Nil)

    normal shouldBe StartupRecovery.Plan.normal
    safe shouldBe StartupRecovery.Plan(safeMode = true, notices = List(SafeMode.Notice))
  }

  it should "start in safe mode by itself when starts did not finish, and say why" in {
    val plan = StartupRecovery.plan(LaunchOptions(), StartupCrashGuard.Decision.StartSafeMode(3), Nil, Nil)

    plan.safeMode shouldBe true
    plan.crashLoopAfter shouldBe Some(3)
    plan.notices.exists(_.contains("3 times")) shouldBe true
  }

  it should "say where the old settings and session were kept" in {
    val root    = Path.of("/home/me/.serenity")
    val config  = LaunchReset.Moved(root.resolve("config.conf"), root.resolve("config.conf.backup-1"))
    val session = LaunchReset.Moved(root.resolve("sessions"), root.resolve("session-backup-1").resolve("sessions"))

    val plan = StartupRecovery.plan(LaunchOptions(), StartupCrashGuard.Decision.Proceed, List(config), List(session))

    plan.notices should have size 2
    plan.notices.head should include(config.to.toString)
    plan.notices(1) should include(root.resolve("session-backup-1").toString)
  }

  "StartupRecovery.Plan.windowTitle" should "name safe mode in the title only when it is on" in {
    StartupRecovery.Plan.normal.windowTitle shouldBe "Serenity"
    StartupRecovery.Plan(safeMode = true, notices = Nil).windowTitle shouldBe "Serenity -- Safe mode"
  }

  "AppState.statusLineText" should "lead with the safe-mode label while safe mode is on, even with nothing open" in {
    val base = AppState.empty(AppConfig.default)
    val safe = StartupRecovery.Plan(safeMode = true, notices = Nil).appliedTo(base)

    safe.runtime.safeMode shouldBe true
    safe.statusLineText.exists(_.startsWith(StatusLineText.SafeModeLabel)) shouldBe true
    base.statusLineText.exists(_.contains(StatusLineText.SafeModeLabel)) shouldBe false
  }

  "StartupRecovery.Plan.appliedTo" should "show the crash-loop prompt as a blocking prompt only when one took over" in {
    val base = AppState.empty(AppConfig.default)
    val loop = StartupRecovery.Plan(safeMode = true, notices = Nil, crashLoopAfter = Some(2)).appliedTo(base)

    loop.runtime.safeMode shouldBe true
    loop.runtime.modalStack.map(_.modal) shouldBe List(Modal.Confirm(ConfirmPrompt.startedInSafeMode(2)))
    StartupRecovery.Plan.normal.appliedTo(base) shouldBe base
  }

  "ConfirmPrompt.startedInSafeMode" should "restart normally when accepted and stay in safe mode when declined" in {
    val prompt = ConfirmPrompt.startedInSafeMode(2)

    prompt.blocking shouldBe true
    prompt.message.head should include("didn't finish starting 2 times in a row; started in safe mode")
    prompt.message.last shouldBe "Restart normally?"
    prompt.choices.items.map(_.action).toList shouldBe
      List(ConfirmAction.Run(SafeModeCommands.restartNormally), ConfirmAction.Dismiss)
    prompt.onDismiss shouldBe ConfirmAction.Dismiss
  }

  "StartupRecovery.Plan.uiPresetStore" should "keep presets beside the scratch session so the user's file is never written" in {
    val root   = Files.createTempDirectory("serenity-safe-presets")
    val plan   = StartupRecovery.Plan(safeMode = true, notices = Nil)
    val preset = UiPreset.builtIns.head.copy(name = "Mine")

    plan.uiPresetStore(Some(root)).create(preset).unsafeRunSync()

    Files.exists(root.resolve("ui-presets.json")) shouldBe true
    plan.uiPresetStore(Some(root)).list().unsafeRunSync().map(_.name) should contain("Mine")
    plan.uiPresetStore(Some(root)) should not be UiPresetStore.default
  }

  "The command palette" should "offer Restart in Safe Mode, Restart Normally and Reset Settings" in {
    CommandRegistry.default.findCommand("restart-safe-mode").map(_.label) shouldBe Some("Restart in Safe Mode")
    CommandRegistry.default.findCommand("restart-normally").map(_.label) shouldBe Some("Restart Normally")
    CommandRegistry.default.findCommand("reset-settings").map(_.label) shouldBe Some("Reset Settings")
  }
