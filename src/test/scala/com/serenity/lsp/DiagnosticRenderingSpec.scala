package com.serenity.lsp

import java.nio.file.Paths

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{LspEvent, ResizeEvent}
import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.manager.StateManager
import com.serenity.state.reducers.SystemEventReducer
import com.serenity.ui.layout.{Layout, LayoutRect}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}
import com.serenity.MockRenderSurface

class DiagnosticRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance            = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def diag(line: Int, severity: DiagnosticSeverity): Diagnostic =
    Diagnostic(
      range    = LspRange(LspPosition(line, 0), LspPosition(line, 10)),
      severity = Some(severity),
      message  = s"diagnostic at line $line"
    )

  "SystemEventReducer" should "store diagnostics from LspDiagnosticsReceived" in {
    given Balance = Balance.default
    val state = AppState.initial

    val uri   = "file:///foo/Bar.scala"
    val diags = List(diag(0, DiagnosticSeverity.Error), diag(5, DiagnosticSeverity.Warning))

    val result = SystemEventReducer.reduce(LspEvent.LspDiagnosticsReceived(uri, diags), state)

    result.state.diagnostics should contain key uri
    result.state.diagnostics(uri) should have size 2
    result.effects shouldBe empty
  }

  it should "replace diagnostics for the same URI" in {
    given Balance = Balance.default
    val uri   = "file:///foo/Bar.scala"
    val state = AppState.initial.copy(
      diagnostics = Map(uri -> List(diag(0, DiagnosticSeverity.Error)))
    )

    val newDiags = List(diag(3, DiagnosticSeverity.Warning))
    val result   = SystemEventReducer.reduce(LspEvent.LspDiagnosticsReceived(uri, newDiags), state)

    result.state.diagnostics(uri) should have size 1
    result.state.diagnostics(uri).head.range.start.line shouldBe 3
  }

  it should "clear diagnostics when an empty list is received" in {
    given Balance = Balance.default
    val uri   = "file:///foo/Bar.scala"
    val state = AppState.initial.copy(
      diagnostics = Map(uri -> List(diag(0, DiagnosticSeverity.Error)))
    )

    val result = SystemEventReducer.reduce(LspEvent.LspDiagnosticsReceived(uri, Nil), state)
    result.state.diagnostics(uri) shouldBe empty
  }

  "Theme" should "have error and warning colors" in {
    val theme = Theme.dark
    theme.error.foreground   should not be null
    theme.warning.foreground should not be null
    theme.warning.foreground should not be theme.error.foreground
  }

  "StateManager" should "apply LspDiagnosticsReceived and update diagnostics in AppState" in {
    val logger = LoggerFactory[IO].getLogger(using LoggerName("DiagnosticRenderingSpec"))
    val sm     = StateManager.apply(logger).unsafeRunSync()

    val uri   = "file:///foo/Bar.scala"
    val diags = List(diag(0, DiagnosticSeverity.Error))
    sm.applyEvent(LspEvent.LspDiagnosticsReceived(uri, diags)).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    state.diagnostics should contain key uri
    state.diagnostics(uri).head.severity shouldBe Some(DiagnosticSeverity.Error)
  }
