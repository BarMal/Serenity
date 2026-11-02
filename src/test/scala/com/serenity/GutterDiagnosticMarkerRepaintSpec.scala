package com.serenity

import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange}
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A gutter's `!` marker shares the divider's column, so a frame redraws a marker only where it repainted the divider
  * over it. A frame that edits one row must not touch the markers of the rows it kept, and must still equal a frame
  * drawn from scratch to the last device pixel, at the laptop's 2x scale.
  *
  * "Not touched" is observed on the image itself: a sentinel colour is stamped into the pooled image the next frame
  * will draw into, and survives only if nothing drew over those pixels.
  */
class GutterDiagnosticMarkerRepaintSpec extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  private val scale    = 2.0
  private val sentinel = 0xff00ff

  private def withErrorOnEveryLine(state: AppState, lines: Int): AppState =
    val buffer = state.persisted.buffers(bufferId)
    val diagnostics = (0 until lines).toList.map(line =>
      Diagnostic(
        range = LspRange(LspPosition(line, 0), LspPosition(line, 4)),
        severity = Some(DiagnosticSeverity.Error),
        message = s"problem on line $line",
        source = Some("test"),
        code = Some("test")
      )
    )
    state.copy(runtime =
      state.runtime.copy(languageService =
        LanguageServiceState(diagnosticsState =
          DiagnosticsState(diagnostics = Map(SpellChecker.diagnosticsUri(buffer) -> diagnostics))
        )
      )
    )

  private def stateWithErrors(language: Option[LanguageId]): AppState =
    withErrorOnEveryLine(
      stateOf(codeLines.mkString("\n"), CursorPosition(10, 0), language = language),
      codeLines.length
    )

  private def markerCell(plan: com.serenity.ui.renderer.EditorPaneRenderPlan, row: Int): (Int, Int) =
    val gutter = plan.layoutContract.lineNumberRect.getOrElse(fail("expected a line-number rect"))
    val x      = math.round(cellMetrics.toPixelX(gutter.x + gutter.width - 1) * scale).toInt + 1
    val y      = math.round(cellMetrics.toPixelY(gutter.y + row) * scale).toInt + 1
    (x, y)

  private def stamp(image: FramePixels, at: (Int, Int)): (Int, Int) =
    image.stamp(at._1, at._2, sentinel)
    at

  private def survives(image: FramePixels, at: (Int, Int)): Boolean =
    (image.rgbAt(at._1, at._2) & 0xffffff) == sentinel

  "A keystroke frame" should "leave the markers of the rows it kept untouched" in {
    val before  = stateWithErrors(Some(LanguageId.Scala))
    val after   = typedAt(before, codeLines, line = 10, text = "x")
    val session = Session()
    val plan    = session.settle(before)

    val target    = session.pendingImage
    val farRow    = stamp(target, markerCell(plan, 25))
    val editedRow = stamp(target, markerCell(plan, 10))

    val (drawn, _) = session.draw(after, DamageProducer.forTransition(before, after))

    withClue("marker of a row the edit did not touch: ")(survives(drawn, farRow) shouldBe true)
    withClue("marker of the edited row: ")(survives(drawn, editedRow) shouldBe false)
  }

  it should "equal a frame drawn from scratch where the counter is a cell grid" in {
    val before  = stateWithErrors(Some(LanguageId.Scala))
    val after   = typedAt(before, codeLines, line = 10, text = "x")
    val session = Session()
    val _       = session.settle(before)

    val (drawn, _) = session.draw(after, DamageProducer.forTransition(before, after))

    differingPixels(drawn, cold(after)) shouldBe empty
  }

  it should "equal a frame drawn from scratch where the counter is measured" in {
    val before  = stateWithErrors(None)
    val after   = typedAt(before, codeLines, line = 10, text = "x")
    val session = Session()
    val _       = session.settle(before)

    val (drawn, _) = session.draw(after, DamageProducer.forTransition(before, after))

    differingPixels(drawn, cold(after)) shouldBe empty
  }

  it should "equal a frame drawn from scratch after a run of keystrokes on different rows" in {
    val start   = stateWithErrors(Some(LanguageId.Scala))
    val session = Session()
    val _       = session.settle(start)

    val (last, lastImage) = List(3, 40, 41, 8, 8).foldLeft((start, session.pendingImage)) {
      case ((current, _), line) =>
        val next       = typedAt(current, codeLines, line = line, text = "x")
        val (image, _) = session.draw(next, DamageProducer.forTransition(current, next))
        (next, image)
    }

    differingPixels(lastImage, cold(last)) shouldBe empty
  }
