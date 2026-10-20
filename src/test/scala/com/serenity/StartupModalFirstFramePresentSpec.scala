package com.serenity

import java.awt.{Dimension, Font, GraphicsEnvironment}
import javax.swing.SwingUtilities

import cats.effect.unsafe.implicits.global
import com.serenity.app.{AppStartup, StartupRecovery}
import com.serenity.config.{AppConfig, LayoutPin, WindowChromeMode}
import com.serenity.diagnostics.{FramePhase, FrameTimings}
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.Damage
import com.serenity.testkit.DisplayExclusive
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{FontSpec, RendererCursorOverlay}
import com.serenity.ui.terminal.SwingWindow
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A blocking prompt over the start page, as the first launch after upgrading shows, is drawn into the window's base
  * frame like any other. The window has to be told to paint it, or it stays black until some later input repaints.
  * Needs a display: the window under test is a real `JFrame`.
  */
class StartupModalFirstFramePresentSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private val pins = List(LayoutPin("editor.line_numbers", "true", "false"))

  private def paintsAfterRendering(recovery: StartupRecovery.Plan): Int =
    assume(!GraphicsEnvironment.isHeadless, "Presenting a frame needs a real frame, which needs a display")
    val stateManager = createStateManager("StartupModalFirstFramePresentSpec")
    val state = AppStartup
      .initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        Theme.default,
        ViewportSize(80, 24),
        appConfig = AppConfig.default,
        recovery = recovery
      )
      .unsafeRunSync()
    val timings = FrameTimings()
    DisplayExclusive:
      val window = new SwingWindow(
        new Dimension(640, 480),
        CellMetrics(8, 16, 13),
        WindowChromeMode.Custom,
        CellMetrics(8, 16, 13),
        timings,
        env = Map("XDG_SESSION_TYPE" -> "x11")
      )
      try
        window.start()
        settle()
        timings.setEnabled(true)
        val font = FontSpec.fromAwt(new Font(Font.MONOSPACED, Font.PLAIN, 13))
        val _ = RendererCursorOverlay.renderWithCursorOverlay(
          state,
          window,
          font,
          font,
          font,
          CellMetrics(8, 16, 13),
          None,
          Damage.Everything,
          RenderCaches.create()
        )
        settle()
        timings.drain().phases.get(FramePhase.Paint).fold(0)(_.count)
      finally
        window.stop()
        settle()

  private def settle(): Unit =
    SwingUtilities.invokeAndWait(() => ())
    SwingUtilities.invokeAndWait(() => ())

  "The first frame" should "be painted when a prompt sits over the start page" in {
    paintsAfterRendering(StartupRecovery.Plan.normal.copy(layoutPins = pins)) should be > 0
  }

  it should "be painted when nothing sits over the start page" in {
    paintsAfterRendering(StartupRecovery.Plan.normal) should be > 0
  }
