package com.serenity

import java.awt.image.BufferedImage
import java.awt.{Color, Dimension, Font}
import javax.swing.JPanel

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.StartupWarmUp
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.{AppState, CaretStyle}
import com.serenity.ui.layout.{PixelRect, ViewportSize}
import com.serenity.ui.renderer.{
  CaretRecordingSurface,
  FontSpec,
  Java2DRenderSurface,
  RendererCursorOverlay,
  RendererEntryPoints
}
import com.serenity.ui.skia.SkiaCaretCanvases
import com.serenity.ui.terminal.SwingCanvasGeometry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The carets the cursor-only path records over Skia are the ones a Java2D surface places on a cold frame of the same
  * state, step by step, whatever fonts the machine has: both are measured here, on the same machine.
  */
class CaretRecordingParitySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val environment = UiScenarioEnvironment(viewport = ViewportSize(120, 40))
  private val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont    = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val metrics     = environment.cellMetrics
  private val widthPx     = environment.viewport.width * metrics.charWidth
  private val heightPx    = environment.viewport.height * metrics.lineHeight
  private val ink         = Color(244, 208, 63)

  private val steps: List[(String, Event)] =
    List(
      "click"           -> MouseClick(col = 60, row = 15),
      "type L"          -> InsertChar('L'),
      "type o"          -> InsertChar('o'),
      "page down"       -> PageDown,
      "type X"          -> InsertChar('X'),
      "down"            -> MoveDown,
      "up"              -> MoveUp,
      "click elsewhere" -> MouseClick(col = 30, row = 5)
    )

  private def recordedFills(state: AppState, caches: RenderCaches): List[CaretRecordingSurface.CaretFill] =
    val canvas = JPanel()
    canvas.setPreferredSize(Dimension(widthPx, heightPx))
    canvas.setSize(Dimension(widthPx, heightPx))
    val surface =
      CaretRecordingSurface.forGeometry(
        SkiaCaretCanvases,
        metrics,
        FontSpec.fromAwt(codeFont),
        SwingCanvasGeometry.of(canvas)
      )
    RendererCursorOverlay.renderCursorOnly(
      state,
      cursorVisible = true,
      surface,
      environment.viewport,
      FontSpec.fromAwt(codeFont),
      FontSpec.fromAwt(textFont),
      FontSpec.fromAwt(codeFont),
      metrics,
      metrics,
      None,
      caches
    )
    surface.recordedFills

  private def java2dColdCarets(state: AppState): List[PixelRect] =
    val surface = new Java2DRenderSurface(
      new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_ARGB),
      metrics,
      codeFont,
      _ => (),
      logicalWidthPx = widthPx,
      logicalHeightPx = heightPx
    )
    RendererEntryPoints.cursorRepaintRects(
      state,
      surface,
      environment.viewport,
      FontSpec.fromAwt(codeFont),
      FontSpec.fromAwt(textFont),
      FontSpec.fromAwt(codeFont),
      metrics,
      metrics,
      None,
      RenderCaches.create()
    )

  "The Skia caret recorder" should "record the carets a Java2D cold frame places, step by step" in {
    val observed = (for
      driver <- UiScenarioDriver.create(
        "caret-recording-parity",
        environment,
        initialConfig = AppConfig.default.withWordWrap(true)
      )
      _ <- driver.updateState(state => StartupWarmUp.seeded(state, StartupWarmUp.document(40)).getOrElse(state))
      steps <- steps.foldLeft(IO.pure(Vector.empty[(String, List[CaretRecordingSurface.CaretFill], List[PixelRect])])) {
        case (acc, (label, event)) =>
          for
            done  <- acc
            _     <- driver.dispatch(event)
            state <- driver.state
          yield done :+ ((label, recordedFills(state, driver.stateManager.renderCaches), java2dColdCarets(state)))
      }
    yield steps).unsafeRunSync()

    observed.foreach {
      case (label, fills, expected) =>
        withClue(s"after $label: ") {
          expected should not be empty
          fills.map(_.rect) shouldBe expected
          fills.map(fill => (fill.color, fill.style)).distinct shouldBe List((ink, CaretStyle.Plain))
        }
    }
  }

end CaretRecordingParitySpec
