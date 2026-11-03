package com.serenity

import java.util.concurrent.atomic.AtomicReference

import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange}
import com.serenity.rope.Balance
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.RendererEntryPoints
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A misspelled word's squiggle is one batch of rects, not one fill per two pixels of its width: on a rasteriser each
  * fill is a call into native code, and a screenful of misspellings made over a thousand of them per typing frame.
  */
class SquiggleDrawCallsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  /** Records each batch the renderer sends, and still draws it through [[MockRenderSurface]]'s one-by-one fills. */
  final private class BatchRecordingSurface extends MockRenderSurface(100, 30):
    private val recorded = new AtomicReference(List.empty[(IArray[Int], RenderColor)])

    def batches: List[(IArray[Int], RenderColor)] = recorded.get()

    override def fillPixelRects(rects: IArray[Int], color: RenderColor): Unit =
      val _ = recorded.updateAndGet(_ :+ (rects -> color))
      super.fillPixelRects(rects, color)

  private def stateWithMisspellings(text: String, words: List[(Int, Int)]): AppState =
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, text).copy(editing = EditingState(List(CursorPosition(0, 0))))
    val misspellings = words.map((from, to) =>
      Diagnostic(
        range = LspRange(LspPosition(0, from), LspPosition(0, to)),
        severity = Some(DiagnosticSeverity.Hint),
        message = "Unknown word",
        source = Some(SpellChecker.Source),
        code = Some(SpellChecker.UnknownWordCode)
      )
    )
    val initial = AppState.initial(com.serenity.config.AppConfig.conventional)
    initial.copy(
      persisted = initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = Theme.light,
        config = com.serenity.config.AppConfig.conventional.withSyntaxHighlighting(false)
      ),
      runtime = initial.runtime.copy(
        languageService = LanguageServiceState(
          diagnosticsState = DiagnosticsState(diagnostics = Map(SpellChecker.diagnosticsUri(buffer) -> misspellings))
        )
      )
    )

  private def render(state: AppState): BatchRecordingSurface =
    val surface = new BatchRecordingSurface
    RendererEntryPoints.render(state, cursorVisible = false, surface, ViewportSize(100, 30), RenderCaches.create())
    surface

  "A misspelled word" should "be drawn as one batch of rects in the theme's error colour" in {
    val surface = render(stateWithMisspellings("alpha betaa gamma", List(6 -> 11)))
    surface.batches should have size 1
    val (rects, colour) = surface.batches.head
    colour shouldBe Theme.light.error.foreground
    rects.length should be > 4
    rects.length % 4 shouldBe 0
  }

  it should "give each misspelled word its own batch" in {
    val surface = render(stateWithMisspellings("alpha betaa gammaa", List(6 -> 11, 12 -> 18)))
    surface.batches should have size 2
  }
