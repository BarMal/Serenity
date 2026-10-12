package com.serenity.perf

import java.awt.Font
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicReference

import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import com.serenity.diagnostics.{FrameCounter, FrameCounts}
import com.serenity.keystroke.events.{Event, InsertChar}
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, Java2DRenderSurface, RendererFramePlanner, ScreenIdentity}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A multi-column (e-reader) pane keeps the rows an edit leaves alone, as a single-column pane does (#1863): rows are
  * recorded across every column, so the one row typed into is redrawn and the rows of all the columns beside it are
  * kept -- and the frame still equals one drawn from scratch.
  */
class ColumnPaneRowReuseSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont    = Font(Font.SERIF, Font.PLAIN, 14)
  private val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val uiMetrics   = CellMetrics.fromFont(uiFont)
  private val viewport    = ViewportSize(1500 / cellMetrics.charWidth, 1000 / cellMetrics.lineHeight)

  private val columnConfig = AppConfig.default.withColumnMode(true).withColumnTargetWidth(40).withLineNumbers(false)
  private val railedConfig = columnConfig.withLineNumbers(true)

  final private class PersistingImage:
    private val image  = LaptopFrameBenchmarks.frameImage()
    private val caches = RenderCaches.create()
    private val screen = ScreenIdentity(this)
    caches.frameCounters.setEnabled(true)

    def render(state: AppState, damage: Damage): (BufferedImage, FrameCounts) =
      val surface = new Java2DRenderSurface(
        image,
        cellMetrics,
        codeFont,
        _ => (),
        logicalWidthPx = LaptopFrameBenchmarks.LogicalWidthPx,
        logicalHeightPx = LaptopFrameBenchmarks.LogicalHeightPx,
        deviceScaleX = LaptopFrameBenchmarks.DeviceScale,
        deviceScaleY = LaptopFrameBenchmarks.DeviceScale,
        contentPersists = true,
        layerCacheOwnerOverride = Some(screen)
      )
      val _ = RendererFramePlanner.renderFrame(
        state,
        cursorVisible = false,
        surface,
        viewport,
        caches.authoritativeScene.forState(state, viewport, codeFont, textFont),
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics,
        None,
        Some(FrameOutput(screen, new AtomicReference(None))),
        damage,
        caches = caches
      )
      (image, caches.frameCounters.lastFrame)

  private def differing(drawn: BufferedImage, state: AppState): Seq[(Int, Int)] =
    val cold = LaptopFrameBenchmarks.renderedFrame(
      state,
      RenderCaches.create(),
      LaptopFrameBenchmarks.frameImage(),
      cursorVisible = false
    )
    for
      x <- 0 until drawn.getWidth
      y <- 0 until drawn.getHeight
      if (drawn.getRGB(x, y) & 0xffffff) != (cold.getRGB(x, y) & 0xffffff)
    yield (x, y)

  private def summary(differences: Seq[(Int, Int)]): String =
    val xs = differences.map(_._1).distinct.sorted
    val ys = differences.map(_._2).distinct.sorted
    s"${differences.size} pixels differ, x ${xs.take(12).mkString(",")} y ${ys.take(12).mkString(",")}"

  private def typingTransition(config: AppConfig = columnConfig): (AppState, AppState) =
    val (stateManager, _) = LaptopFrameBenchmarks.proseStateManager(config = config)
    val before            = stateManager.getCurrentState.unsafeRunSync()
    stateManager.applyEvent(InsertChar('x'): Event).unsafeRunSync()
    (before, stateManager.getCurrentState.unsafeRunSync())

  private def describe(counts: FrameCounts): String =
    FrameCounter.values.map(counter => s"${counter.label}=${counts(counter)}").mkString(" ")

  "The fixture" should "lay the pane out in more than one column" in {
    val (before, _) = typingTransition()
    val scene       = RenderCaches.create().authoritativeScene.forState(before, viewport, codeFont, textFont)
    val paneId      = before.persisted.layout.editorPanes.keys.head

    scene.columnSnapshotsFor(paneId).size should be > 1
  }

  "A typing frame in a multi-column pane" should "redraw the typed row and keep the rows of every column" in
    keepsUnaffectedRows(typingTransition())

  "A typing frame in a multi-column pane with line-number rails" should
    "redraw the typed row and keep the rows of every column" in
    keepsUnaffectedRows(typingTransition(railedConfig))

  private def keepsUnaffectedRows(transition: (AppState, AppState)): Unit =
    val (before, after) = transition
    val frames          = PersistingImage()
    val (_, first)      = frames.render(before, Damage.Everything)

    val (_, typed) = frames.render(after, DamageProducer.forTransition(before, after))

    withClue(s"first ${describe(first)}; typed ${describe(typed)}: ") {
      typed(FrameCounter.RowsDrawn) should be >= 1L
      typed(FrameCounter.RowsDrawn) should be < (first(FrameCounter.RowsDrawn) / 10)
      typed(FrameCounter.RowsDrawn) + typed(FrameCounter.RowsPreserved) shouldBe first(FrameCounter.RowsDrawn)
      typed(FrameCounter.RowsPreserved) should be > (first(FrameCounter.RowsDrawn) / 2)
    }

  it should "equal a frame drawn from scratch, there and back" in
    equalsColdFrames(typingTransition())

  "A typing frame in a multi-column pane with line-number rails" should "equal a frame drawn from scratch" in
    equalsColdFrames(typingTransition(railedConfig))

  private def equalsColdFrames(transition: (AppState, AppState)): Unit =
    val (before, after) = transition
    val frames          = PersistingImage()
    val _               = frames.render(before, Damage.Everything)

    val (there, _) = frames.render(after, DamageProducer.forTransition(before, after))
    val thereDiffs = differing(there, after)
    val (back, _)  = frames.render(before, DamageProducer.forTransition(after, before))
    val backDiffs  = differing(back, before)

    withClue(s"the frame typed into, ${summary(thereDiffs)}: ")(thereDiffs shouldBe empty)
    withClue(s"the frame typed back out of, ${summary(backDiffs)}: ")(backDiffs shouldBe empty)
