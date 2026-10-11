package com.serenity.ui.renderer

import java.awt.Font

import com.serenity.config.AppConfig
import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange}
import com.serenity.rope.Balance
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme
import com.serenity.{MockRenderSurface, TestWorkspaceTrees}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** In a multi-column (e-reader) pane the page shows several column snapshots at once, so the comments and diagnostics
  * (spelling marks included) of every line on any column must reach the renderer, not only those of column 0.
  */
class RendererColumnAnnotationsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val monoFont     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val cellMetrics  = CellMetrics.fromFont(monoFont)
  private val viewportSize = ViewportSize(160, 20)
  private val paneId       = PaneId(0)
  private val bufferId     = BufferId(1)

  private def config(columnMode: Boolean): AppConfig =
    AppConfig.default.withoutStatusLine
      .withLineNumbers(false)
      .withWordWrap(true)
      .withColumnMode(columnMode)
      .withColumnTargetWidth(20)
      .withColumnGap(2)

  private def diagnosticOn(line: Int): Diagnostic =
    Diagnostic(
      range = LspRange(LspPosition(line, 0), LspPosition(line, 4)),
      severity = Some(DiagnosticSeverity.Warning),
      message = "Possible spelling issue",
      source = Some(SpellChecker.Source),
      code = Some("unknown-word")
    )

  private def commentOn(line: Int): DocumentComment =
    DocumentComment(CursorPosition(line, 0), CursorPosition(line, 4), "note", id = CommentId(1))

  private def stateWith(columnMode: Boolean, annotatedLine: Int): AppState =
    val content = (0 until 400).map(i => f"L$i%03d").mkString("\n")
    val plain   = Buffer.fromString(bufferId, content)
    val buffer =
      plain.copy(annotations = plain.annotations.copy(documentComments = List(commentOn(annotatedLine))))
    val base = AppState.initial
    base.copy(
      persisted = base.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = Theme.light,
        config = config(columnMode)
      ),
      runtime = base.runtime.copy(languageService =
        LanguageServiceState(diagnosticsState =
          DiagnosticsState(diagnostics = Map(SpellChecker.diagnosticsUri(buffer) -> List(diagnosticOn(annotatedLine))))
        )
      )
    )

  private def planFor(state: AppState): (EditorPaneRenderPlan, UiSceneSnapshot) =
    val caches  = RenderCaches.create()
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)
    val context = RenderContext(
      surface = surface,
      layout = CalculatedLayout(
        editorPanelRect = LayoutRect(0, 0, surface.width, surface.height),
        leftSpacerRect = LayoutRect(0, 0, 0, 0),
        rightSpacerRect = LayoutRect(0, 0, 0, 0)
      ),
      codeFont = monoFont,
      textFont = monoFont,
      uiFont = monoFont,
      cellMetrics = cellMetrics,
      uiMetrics = cellMetrics,
      caches = caches
    )
    val scene = caches.authoritativeScene.forState(state, viewportSize, monoFont, monoFont)
    (RendererPaneSetup.prepareEditorPaneRenderPlan(state, context, scene), scene)

  private def firstLineOfColumn(scene: UiSceneSnapshot, columnIndex: Int): Int =
    scene
      .columnSnapshotsFor(paneId)
      .lift(columnIndex)
      .flatMap(_.snapshot.visualLines.headOption)
      .map(_.bufferLine)
      .getOrElse(fail(s"test setup expected a column $columnIndex"))

  "Annotations for a multi-column pane" should "include the diagnostics of a line shown in column 2" in {
    val probe     = planFor(stateWith(columnMode = true, annotatedLine = 0))._2
    val line      = firstLineOfColumn(probe, 2)
    val (plan, _) = planFor(stateWith(columnMode = true, annotatedLine = line))

    plan.annotations(bufferId).diagnosticsByLine.keySet shouldBe Set(line)
  }

  it should "include the comments of a line shown in column 2" in {
    val probe     = planFor(stateWith(columnMode = true, annotatedLine = 0))._2
    val line      = firstLineOfColumn(probe, 2)
    val (plan, _) = planFor(stateWith(columnMode = true, annotatedLine = line))

    plan.annotations(bufferId).commentsByLine.keySet shouldBe Set(line)
  }

  it should "leave out annotations on lines no column shows" in {
    val (plan, scene) = planFor(stateWith(columnMode = true, annotatedLine = 399))

    scene.columnSnapshotsFor(paneId).flatMap(_.snapshot.visualLines.map(_.bufferLine)) should not contain 399
    plan.annotations(bufferId).diagnosticsByLine shouldBe empty
    plan.annotations(bufferId).commentsByLine shouldBe empty
  }

  "Annotations for a single-column pane" should "cover only the lines the pane shows" in {
    val (shown, _)  = planFor(stateWith(columnMode = false, annotatedLine = 0))
    val (hidden, _) = planFor(stateWith(columnMode = false, annotatedLine = 399))

    shown.annotations(bufferId).diagnosticsByLine.keySet shouldBe Set(0)
    hidden.annotations(bufferId).diagnosticsByLine shouldBe empty
  }
