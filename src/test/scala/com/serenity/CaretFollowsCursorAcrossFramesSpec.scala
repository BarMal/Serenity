package com.serenity

import java.awt.image.BufferedImage
import java.awt.{Color, Font}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.StartupWarmUp
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.AppState
import com.serenity.ui.layout.{PixelRect, ViewportSize}
import com.serenity.ui.renderer.{
  CaretRecordingSurface,
  Java2DRenderSurface,
  RendererCursorOverlay,
  RendererEntryPoints,
  ScreenIdentity
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The GUI's frame sequence -- a base frame per input over a persistent, pooled image, the caret filled over it
  * afterwards, cursor-only frames in between, all sharing the editor's render caches -- must paint the caret exactly
  * where a cold frame of the same state would, after clicks, typing and navigation alike.
  */
class CaretFollowsCursorAcrossFramesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val environment = UiScenarioEnvironment(viewport = ViewportSize(120, 40))
  private val viewport    = environment.viewport
  private val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont    = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val metrics     = environment.cellMetrics
  private val widthPx     = viewport.width * metrics.charWidth
  private val heightPx    = viewport.height * metrics.lineHeight

  /** Records the fills made after the base frame was flushed: on a cursor-overlay frame those are the carets. */
  final private class CaretFillRecorder(image: BufferedImage, screen: ScreenIdentity)
      extends Java2DRenderSurface(
        image,
        metrics,
        codeFont,
        _ => (),
        logicalWidthPx = widthPx,
        logicalHeightPx = heightPx,
        contentPersists = true,
        layerCacheOwnerOverride = Some(screen)
      ):
    private val flushed = new java.util.concurrent.atomic.AtomicBoolean(false)
    private val fills   = new java.util.concurrent.atomic.AtomicReference(Vector.empty[PixelRect])

    override def flush(): Unit =
      if !flushed.get() then super.flush()
      flushed.set(true)

    override def fillPixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: Color): Unit =
      if flushed.get() then
        val _ = fills.updateAndGet(_ :+ PixelRect(xPx, yPx, widthPx, heightPx))
      else super.fillPixelRect(xPx, yPx, widthPx, heightPx, color)

    def carets: List[PixelRect] = fills.get().toList

  /** Like the Swing window: base frames alternate between two pooled images, and cursor-only frames record their carets
    * on a surface of their own.
    */
  final private class WarmScreen(caches: RenderCaches):
    private val canvas = new javax.swing.JPanel
    canvas.setPreferredSize(new java.awt.Dimension(widthPx, heightPx))
    private val images = Vector.fill(2)(new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_ARGB))
    private val frames = new java.util.concurrent.atomic.AtomicInteger(0)
    private val screen = ScreenIdentity(canvas)

    def fullFrame(before: AppState, after: AppState): List[PixelRect] =
      val surface = new CaretFillRecorder(images(frames.getAndIncrement() % images.size), screen)
      RendererCursorOverlay.renderWithCursorOverlay(
        after,
        surface,
        viewport,
        codeFont,
        textFont,
        codeFont,
        metrics,
        metrics,
        None,
        DamageProducer.forTransition(before, after),
        caches
      ) shouldBe true
      surface.carets

    def cursorOnlyFrame(state: AppState): List[PixelRect] =
      val surface = CaretRecordingSurface.forCanvas(metrics, codeFont, canvas)
      RendererCursorOverlay.renderCursorOnly(
        state,
        cursorVisible = true,
        surface,
        viewport,
        codeFont,
        textFont,
        codeFont,
        metrics,
        metrics,
        None,
        caches
      ) shouldBe true
      surface.recordedFills.map(_.rect)

  private def coldCaret(state: AppState): List[PixelRect] =
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
      viewport,
      codeFont,
      textFont,
      codeFont,
      metrics,
      metrics,
      None,
      RenderCaches.create()
    )

  private val input: List[(String, Event)] =
    List("click" -> MouseClick(col = 60, row = 15)) ++
      "Lorem ipsum ".toList.map(c => s"type '$c'" -> InsertChar(c)) ++
      List("page down" -> PageDown) ++
      "XYZ".toList.map(c => s"type '$c' after page down" -> InsertChar(c)) ++
      List.fill(3)("down" -> MoveDown) ++
      "QQ".toList.map(c => s"type '$c' after down" -> InsertChar(c)) ++
      List.fill(2)("up" -> MoveUp) ++
      List("click elsewhere" -> MouseClick(col = 30, row = 5)) ++
      "ab".toList.map(c => s"type '$c' after the second click" -> InsertChar(c))

  "The caret painted by warm GUI frames" should "follow the cursor through clicks, typing and navigation" in {
    val program =
      for
        driver <- UiScenarioDriver.create(
          "caret-follows-cursor",
          environment,
          initialConfig = AppConfig.default.withWordWrap(true)
        )
        _ <- driver.updateState(state => StartupWarmUp.seeded(state, StartupWarmUp.document(40)).getOrElse(state))
        screen = new WarmScreen(driver.stateManager.renderCaches)
        initial <- driver.state
        _ = screen.fullFrame(initial, initial)
        steps <- input.foldLeft(IO.pure(Vector.empty[(String, AppState, List[PixelRect], List[PixelRect])])) {
          case (acc, (label, event)) =>
            for
              done   <- acc
              before <- driver.state
              _      <- driver.dispatch(event)
              after  <- driver.state
            yield
              val full       = screen.fullFrame(before, after)
              val cursorOnly = screen.cursorOnlyFrame(after)
              done :+ (label, after, full, cursorOnly)
        }
      yield steps

    val steps = program.unsafeRunSync()
    val positions = steps.map {
      case (label, state, full, cursorOnly) =>
        val expected = coldCaret(state)
        withClue(s"after $label (cursor ${state.persisted.buffers.values.head.editing.cursorPositions}): ") {
          expected should not be empty
          full shouldBe expected
          cursorOnly shouldBe expected
        }
        expected
    }
    positions.distinct.size should be > (steps.size / 2)
  }
