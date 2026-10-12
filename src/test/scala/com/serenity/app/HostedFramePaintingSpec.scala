package com.serenity.app

import java.awt.{Color, Component, Font}
import javax.swing.JPanel

import scala.collection.mutable.ListBuffer
import scala.util.Try

import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, Layout, ViewportSize}
import com.serenity.ui.renderer.{HostedCanvas, HostedFrame, HostedOutcome, RenderSurface, ScreenIdentity}
import com.serenity.ui.skia.SkiaRenderSurface
import com.serenity.{SkiaPixels, TestWorkspaceTrees}
import org.jetbrains.skia.Surface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A frame painted through a [[HostedCanvas]]: the path a Skia GPU window takes, with a raster surface standing in for
  * the layer's canvas so it runs without a window.
  */
class HostedFramePaintingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val codeFont = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val uiFont   = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val metrics  = CellMetrics.fromFont(codeFont)
  private val viewport = ViewportSize(40, 8)
  private val widthPx  = viewport.width * metrics.charWidth
  private val heightPx = viewport.height * metrics.lineHeight

  private val display = RuntimeDisplayState.Snapshot(
    codeFont,
    codeFont,
    uiFont,
    metrics,
    metrics,
    CellMetrics.fromFont(uiFont)
  )

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def editorState(): AppState =
    val buffer = Buffer.fromString(bufferId, "alpha\nbeta\ngamma")
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId)
      )
    )

  final private class RasterCanvas extends HostedCanvas:
    val target: Surface                    = Surface.Companion.makeRasterN32Premul(widthPx, heightPx)
    val owners: ListBuffer[ScreenIdentity] = ListBuffer.empty

    def component: Component = new JPanel()
    def renderApi: String    = "RASTER"

    def drawFrame(frame: HostedFrame)(paint: RenderSurface => Unit): HostedOutcome =
      val surface = SkiaRenderSurface.onCanvas(
        target.getCanvas,
        frame.metrics,
        frame.font,
        widthPx,
        heightPx,
        1.0,
        layerCacheOwner = Some(frame.screen)
      )
      val _ = owners += surface.layerCacheOwner
      Try {
        paint(surface)
        surface.flush()
      }.fold(HostedOutcome.Failed(_), _ => HostedOutcome.Drawn)

    def pixels: Array[Int] = SkiaPixels.read(target)

  private def paint(
    canvas: HostedCanvas,
    cursorVisible: Boolean = false,
    screen: ScreenIdentity = ScreenIdentity(new Object),
    caches: RenderCaches = RenderCaches.create()
  ): HostedOutcome =
    GuiFramePainting.paintHostedFrame(
      canvas,
      editorState(),
      cursorVisible,
      Option.when(cursorVisible)(RenderColor.fromAwt(Color.RED)),
      viewport,
      metrics,
      screen,
      display,
      Damage.Everything,
      caches
    )

  "paintHostedFrame" should "draw the editor into the hosted canvas" in {
    val canvas = new RasterCanvas
    paint(canvas) shouldBe HostedOutcome.Drawn

    val pixels = canvas.pixels
    pixels.distinct.length should be > 1
  }

  it should "build every frame's surface with the screen it was given" in {
    val canvas = new RasterCanvas
    val screen = ScreenIdentity(new Object)
    val caches = RenderCaches.create()
    paint(canvas, screen = screen, caches = caches)
    paint(canvas, screen = screen, caches = caches)

    canvas.owners.toList shouldBe List(screen, screen)
  }

  it should "draw the caret into the same frame when it is visible" in {
    val withoutCaret = new RasterCanvas
    val withCaret    = new RasterCanvas
    paint(withoutCaret)
    paint(withCaret, cursorVisible = true)

    withCaret.pixels should not be withoutCaret.pixels
  }

  it should "report a frame whose drawing fails as failed, with the cause" in {
    val boom   = new IllegalStateException("boom")
    val canvas = new RasterCanvas
    val outcome = canvas.drawFrame(
      HostedFrame(metrics, com.serenity.ui.renderer.FontSpec.fromAwt(codeFont), ScreenIdentity(canvas))
    )(_ => throw boom)

    outcome shouldBe HostedOutcome.Failed(boom)
  }

  "GuiFramePainting.settle" should "fail the frame step for a failed frame and ignore the others" in {
    val boom = new IllegalStateException("boom")
    GuiFramePainting.settle(HostedOutcome.Failed(boom)).attempt.unsafeRunSync() shouldBe Left(boom)
    GuiFramePainting.settle(HostedOutcome.Drawn).attempt.unsafeRunSync() shouldBe Right(())
    GuiFramePainting.settle(HostedOutcome.Skipped("hidden")).attempt.unsafeRunSync() shouldBe Right(())
  }
