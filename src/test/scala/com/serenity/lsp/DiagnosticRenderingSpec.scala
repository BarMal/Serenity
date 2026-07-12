package com.serenity.lsp

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.LspEvent
import com.serenity.lsp.model.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.state.reducers.SystemEventReducer
import com.serenity.ui.layout.Location
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class DiagnosticRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def diag(line: Int, severity: DiagnosticSeverity): Diagnostic =
    Diagnostic(
      range = LspRange(LspPosition(line, 0), LspPosition(line, 10)),
      severity = Some(severity),
      message = s"diagnostic at line $line"
    )

  "SystemEventReducer" should "store diagnostics from LspDiagnosticsReceived" in {
    given Balance = Balance.default
    val state     = AppState.initial

    val uri   = "file:///foo/Bar.scala"
    val diags = List(diag(0, DiagnosticSeverity.Error), diag(5, DiagnosticSeverity.Warning))

    val result = SystemEventReducer.reduce(LspEvent.LspDiagnosticsReceived(uri, diags), state)

    result.state.diagnostics should contain key uri
    result.state.diagnostics(uri) should have size 2
    result.effects shouldBe empty
  }

  it should "replace diagnostics for the same URI" in {
    given Balance = Balance.default
    val uri       = "file:///foo/Bar.scala"
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
    val uri       = "file:///foo/Bar.scala"
    val state = AppState.initial.copy(
      diagnostics = Map(uri -> List(diag(0, DiagnosticSeverity.Error)))
    )

    val result = SystemEventReducer.reduce(LspEvent.LspDiagnosticsReceived(uri, Nil), state)
    result.state.diagnostics(uri) shouldBe empty
  }

  it should "show LSP hover text as a quick-info peek surface" in {
    given Balance = Balance.default
    val result = SystemEventReducer.reduce(
      LspEvent.LspHoverReceived("def map[B](f: A => B): List[B]", CursorPosition(2, 4)),
      AppState.initial
    )

    val surface = result.state.uiSurfaces.headOption.getOrElse(fail("Expected hover surface"))
    surface.content shouldBe SurfaceContent.QuickInfo("def map[B](f: A => B): List[B]")
    surface.presentation shouldBe SurfacePresentation.Floating(Some(CursorPosition(2, 4)), SurfacePlacement.AboveCursor)
    result.state.focus shouldBe Focus.Surface(surface.id)
  }

  it should "show LSP definition locations as symbol-definition peek surfaces" in {
    given Balance = Balance.default
    val event = LspEvent.LspDefinitionReceived(
      symbol = "map",
      uri = "file:///workspace/Foo.scala",
      position = LspPosition(9, 2),
      anchor = CursorPosition(1, 3)
    )

    val result  = SystemEventReducer.reduce(event, AppState.initial)
    val surface = result.state.uiSurfaces.headOption.getOrElse(fail("Expected definition surface"))

    surface.content shouldBe SurfaceContent.SymbolDefinition("map @ file:///workspace/Foo.scala", Location(9, 2))
    surface.presentation shouldBe SurfacePresentation.Floating(Some(CursorPosition(1, 3)), SurfacePlacement.AboveCursor)
  }

  it should "show LSP completion candidates as a quick-info peek surface" in {
    given Balance = Balance.default
    val result = SystemEventReducer.reduce(
      LspEvent.LspCompletionReceived(List("map", "mapValues"), CursorPosition(2, 4)),
      AppState.initial
    )

    val surface = result.state.uiSurfaces.headOption.getOrElse(fail("Expected completion surface"))
    surface.content shouldBe SurfaceContent.QuickInfo("map\nmapValues")
    surface.presentation shouldBe SurfacePresentation.Floating(Some(CursorPosition(2, 4)), SurfacePlacement.AboveCursor)
    result.state.focus shouldBe Focus.Surface(surface.id)
  }

  it should "show an explicit empty state when LSP completion returns no candidates" in {
    given Balance = Balance.default
    val result = SystemEventReducer.reduce(
      LspEvent.LspCompletionReceived(Nil, CursorPosition(2, 4)),
      AppState.initial
    )

    result.state.uiSurfaces.headOption.map(_.content) shouldBe Some(
      SurfaceContent.QuickInfo("No completions available.")
    )
  }

  "Theme" should "have error and warning colors" in {
    val theme = Theme.dark
    theme.error.foreground should not be null
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
