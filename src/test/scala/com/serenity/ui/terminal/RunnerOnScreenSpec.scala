package com.serenity.ui.terminal

import java.awt.{GraphicsEnvironment, Robot, Font}
import javax.swing.SwingUtilities

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{FontSpec, RendererEntryPoints}
import com.serenity.TestWorkspaceTrees
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RunnerOnScreenSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val runnerId = SurfaceId("command-runner")
  private val registry = CommandRegistry.default
  private val runner = CommandRunner.empty.activate(registry, AppConfig.default).updateSearchTerm("")(using registry)

  private def state(withRunner: Boolean): AppState =
    val buffer = Buffer.fromString(bufferId, (1 to 40).map(i => s"line number $i with some content").mkString("\n"))
    val base   = AppState.initial
    base.copy(
      persisted = base.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = if withRunner then Focus.Surface(runnerId) else Focus.EditorPane(paneId)
      ),
      runtime = base.runtime.copy(
        uiSurfaces =
          if withRunner then
            List(
              UiSurface(
                runnerId,
                SurfaceContent.CommandPalette(runner),
                SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
              )
            )
          else Nil
      )
    )

  for repaintOnFlush <- List(false, true) do
    s"The runner shown in a real window (repaintOnFlush=$repaintOnFlush)" should "not be a flat block" in {
      assume(!GraphicsEnvironment.isHeadless, "needs a display")
      val caches = RenderCaches.create()
      val out = SwingWindow
        .resource()
        .use { window =>
          IO.blocking {
            SwingUtilities.invokeAndWait(() => ())
            val font = FontSpec.fromAwt(new Font(Font.MONOSPACED, Font.PLAIN, 14))
            val ui   = FontSpec.fromAwt(new Font(Font.SANS_SERIF, Font.PLAIN, 14))
            val before = state(false)
            val after  = state(true)
            def draw(s: AppState, damage: Damage): Unit =
              RendererEntryPoints.render(s, false, window, font, font, ui, window.metrics, None, repaintOnFlush, damage, caches)
              SwingUtilities.invokeAndWait(() => window.canvas.paintImmediately(0, 0, window.canvas.getWidth, window.canvas.getHeight))
            draw(before, Damage.Everything)
            draw(after, DamageProducer.forTransition(before, after))
            Thread.sleep(500)
            val rect = LayoutEngine
              .calculateLayout(after, window.viewportSize)
              .belowCursorOverlayRect
              .getOrElse(fail("no runner rect"))
            val origin = window.canvas.getLocationOnScreen
            val m      = window.metrics
            val shot = new Robot().createScreenCapture(
              new java.awt.Rectangle(
                origin.x + rect.x * m.charWidth, origin.y + rect.y * m.lineHeight,
                rect.width * m.charWidth, rect.height * m.lineHeight
              )
            )
            (for y <- 0 until shot.getHeight; x <- 0 until shot.getWidth yield shot.getRGB(x, y)).toSet.size
          }
        }
        .unsafeRunSync()
      info(s"distinct colours on screen in runner rect: $out")
      out should be > 2
    }
