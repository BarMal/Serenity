package com.serenity.app

import com.serenity.command.{CommandIntent, CommandRegistry, DiagnosticsCommands, DiagnosticsIntent}
import com.serenity.config.AppConfig
import com.serenity.diagnostics.{PreviousRun, RuntimeIdentity}
import com.serenity.state.models.{AppState, ConfirmAction, ConfirmPrompt, Modal}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UnexpectedExitSpec extends AnyFlatSpec with Matchers:

  private val base = AppState.empty(AppConfig.default)

  private def plan(previous: PreviousRun, decision: StartupCrashGuard.Decision = StartupCrashGuard.Decision.Proceed) =
    StartupRecovery.plan(LaunchOptions(), decision, Nil, Nil, previous)

  "StartupRecovery.plan" should "carry the report of an abnormal previous run" in {
    plan(PreviousRun.Abnormal("the report")).unexpectedExit shouldBe Some("the report")
  }

  it should "carry nothing after a clean previous run" in {
    plan(PreviousRun.Clean).unexpectedExit shouldBe None
    plan(PreviousRun.Clean) shouldBe StartupRecovery.Plan.normal
  }

  "StartupRecovery.Plan.appliedTo" should "show the closed-unexpectedly prompt after an abnormal exit" in {
    val applied = plan(PreviousRun.Abnormal("the report")).appliedTo(base)

    applied.runtime.modalStack.map(_.modal) shouldBe
      List(Modal.Confirm(ConfirmPrompt.closedUnexpectedly("the report")))
  }

  it should "leave the crash-loop prompt to speak first when safe mode took over as well" in {
    val applied = plan(PreviousRun.Abnormal("the report"), StartupCrashGuard.Decision.StartSafeMode(2)).appliedTo(base)

    applied.runtime.modalStack.map(_.modal) shouldBe List(Modal.Confirm(ConfirmPrompt.startedInSafeMode(2)))
  }

  "ConfirmPrompt.closedUnexpectedly" should "say Serenity closed unexpectedly and offer the log folder and a copied report" in {
    val prompt = ConfirmPrompt.closedUnexpectedly("the report")

    prompt.title shouldBe "Serenity closed unexpectedly"
    prompt.blocking shouldBe true
    prompt.choices.items.map(_.label).toList shouldBe List("Copy report", "Open Logs Folder", "Dismiss")
    prompt.choices.items.map(_.action).toList shouldBe List(
      ConfirmAction.Run(DiagnosticsCommands.copyToClipboard("the report")),
      ConfirmAction.Run(DiagnosticsCommands.openLogsFolder),
      ConfirmAction.Dismiss
    )
    prompt.onDismiss shouldBe ConfirmAction.Dismiss
  }

  "ConfirmPrompt.about" should "list the build's facts and offer to copy them or open the logs" in {
    val identity = RuntimeIdentity.of("1.2.3", "abc1234", _ => None)
    val prompt   = ConfirmPrompt.about(identity)

    prompt.title shouldBe "About Serenity"
    identity.lines.foreach(line => prompt.message should contain(line))
    prompt.choices.items.map(_.action).toList shouldBe List(
      ConfirmAction.Run(DiagnosticsCommands.copyToClipboard(identity.summary)),
      ConfirmAction.Run(DiagnosticsCommands.openLogsFolder),
      ConfirmAction.Dismiss
    )
  }

  "The command palette" should "offer About Serenity and Open Logs Folder" in {
    CommandRegistry.default.findCommand("about-serenity").map(_.label) shouldBe Some("About Serenity")
    CommandRegistry.default.findCommand("open-logs-folder").map(_.label) shouldBe Some("Open Logs Folder")
    CommandRegistry.default.findCommand("about-serenity").map(_.intent) shouldBe
      Some(CommandIntent.Diagnostics(DiagnosticsIntent.ShowAbout))
    CommandRegistry.default.findCommand("open-logs-folder").map(_.intent) shouldBe
      Some(CommandIntent.Diagnostics(DiagnosticsIntent.OpenLogsFolder))
  }
