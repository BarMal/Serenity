package com.serenity

import java.awt.{Color, Dimension, Font}
import javax.swing.JPanel

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.StartupWarmUp
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.CaretStyle
import com.serenity.ui.layout.{PixelRect, ViewportSize}
import com.serenity.ui.renderer.{CaretRecordingSurface, FontSpec, RendererCursorOverlay}
import com.serenity.ui.terminal.SwingCanvasGeometry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The carets the cursor-only path records for a fixed sequence of input. The values were captured from the Java2D
  * recorder before the recorder moved to Skia, so they hold the move to the same pixels.
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

  private val steps: List[(String, Event, PixelRect)] =
    List(
      ("click", MouseClick(col = 60, row = 15), PixelRect(477, 225, 2, 15)),
      ("type L", InsertChar('L'), PixelRect(484, 225, 2, 15)),
      ("type o", InsertChar('o'), PixelRect(491, 225, 2, 15)),
      ("page down", PageDown, PixelRect(0, 315, 2, 15)),
      ("type X", InsertChar('X'), PixelRect(8, 315, 2, 15)),
      ("down", MoveDown, PixelRect(11, 330, 2, 15)),
      ("up", MoveUp, PixelRect(8, 315, 2, 15)),
      ("click elsewhere", MouseClick(col = 30, row = 5), PixelRect(241, 75, 2, 15))
    )

  "The caret recorder" should "record the carets the Java2D recorder recorded, step by step" in {
    val recorded = (for
      driver <- UiScenarioDriver.create(
        "caret-recording-parity",
        environment,
        initialConfig = AppConfig.default.withWordWrap(true)
      )
      _ <- driver.updateState(state => StartupWarmUp.seeded(state, StartupWarmUp.document(40)).getOrElse(state))
      fills <- steps.foldLeft(IO.pure(Vector.empty[(String, List[CaretRecordingSurface.CaretFill])])) {
        case (acc, (label, event, _)) =>
          for
            done  <- acc
            _     <- driver.dispatch(event)
            state <- driver.state
          yield
            val canvas = JPanel()
            canvas.setPreferredSize(Dimension(widthPx, heightPx))
            canvas.setSize(Dimension(widthPx, heightPx))
            val surface =
              CaretRecordingSurface.forGeometry(metrics, FontSpec.fromAwt(codeFont), SwingCanvasGeometry.of(canvas))
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
              driver.stateManager.renderCaches
            )
            done :+ (label -> surface.recordedFills)
      }
    yield fills).unsafeRunSync()

    recorded.toList shouldBe steps.map((label, _, rect) =>
      label -> List(CaretRecordingSurface.CaretFill(rect, ink, CaretStyle.Plain))
    )
  }

end CaretRecordingParitySpec
