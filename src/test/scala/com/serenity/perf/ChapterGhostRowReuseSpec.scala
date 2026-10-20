package com.serenity.perf

import java.awt.Font
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicReference

import com.serenity.config.{HeadingsConfig, MarkdownViewMode}
import com.serenity.diagnostics.{FrameCounter, FrameCounts}
import com.serenity.keystroke.events.InsertChar
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.state.reducers.EditorEventReducer
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, Java2DRenderSurface, RendererFramePlanner, ScreenIdentity}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A pane showing chapter ghosts keeps the rows an edit leaves alone (#1863): the ghosts that changed between frames
  * mark their rows dirty alongside the text damage, and only those rows are redrawn -- the frame still equals one drawn
  * from scratch.
  */
class ChapterGhostRowReuseSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont    = Font(Font.SERIF, Font.PLAIN, 14)
  private val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val uiMetrics   = CellMetrics.fromFont(uiFont)
  private val viewport    = ViewportSize(1500 / cellMetrics.charWidth, 1000 / cellMetrics.lineHeight)

  private val manuscriptId = BufferId(1)
  private val noteId       = BufferId(7)
  private val storm        = NoteKey.Chapter(HeadingIdentity("storm", 0))
  private val paneId       = PaneId(0)

  private val GhostLine = 1
  private val ProseLine = 12

  private val manuscript =
    ("# Chapter 1: Storm" :: "" :: "" :: "" :: "# Chapter 2: Calm" :: ""
      :: (1 to 40).map(n => s"Paragraph $n of the calm chapter, long enough to be a line of prose.").toList)
      .mkString("\n")

  private def stateWith(overview: String, cursorLine: Int = ProseLine): AppState =
    val base  = BenchmarkFixtures.editorState(manuscript, Some(LanguageId.Markdown))
    val owner = base.persisted.buffers(manuscriptId)
    val noted = owner.copy(
      annotations = owner.annotations.copy(notes = Map(storm -> Notes(noteId))),
      editing = EditingState(List(CursorPosition(cursorLine, 5)))
    )
    val note = Buffer.fromString(noteId, overview).copy(hidden = true)
    base.copy(
      persisted =
        base.persisted.copy(buffers = base.persisted.buffers.updated(manuscriptId, noted).updated(noteId, note)),
      runtime = base.runtime.copy(chapterGhostsVisible = true, nextBufferId = BufferId(noteId.value + 1))
    )

  private def typedInto(state: AppState): AppState =
    EditorEventReducer.reduce(InsertChar('x'), paneId, state).state

  private def withOverview(state: AppState, overview: String): AppState =
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(noteId, Buffer.fromString(noteId, overview).copy(hidden = true))
      )
    )

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
        layerCacheOwnerOverride = Some(screen),
        textCaches = caches.surfaceText
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

  private def describe(counts: FrameCounts): String =
    FrameCounter.values.map(counter => s"${counter.label}=${counts(counter)}").mkString(" ")

  private val overview = "Gale hits\nLiz leaves"

  private val typing: (AppState, AppState) =
    val before = stateWith(overview)
    before -> typedInto(before)

  private val rewrittenNote: (AppState, AppState) =
    val before = stateWith(overview)
    before -> withOverview(before, "Squall hits\nLiz leaves")

  private val firstProse: (AppState, AppState) =
    val before = stateWith(overview, cursorLine = GhostLine)
    before -> typedInto(before)

  "The fixture" should "show the ghost on the blank lines under the empty chapter" in {
    val (before, _) = typing
    val scene       = RenderCaches.create().authoritativeScene.forState(before, viewport, codeFont, textFont)

    scene.textSnapshot(paneId).map(_.visualLines.map(_.bufferLine).take(4)) shouldBe Some(Vector(0, 1, 2, 3))
    before.persisted.buffers(manuscriptId).annotations.notes.keySet shouldBe Set(storm)
  }

  "A typing frame in a pane showing chapter ghosts" should "redraw the typed row and keep the rest" in
    keepsUnaffectedRows(typing)

  "A rewritten chapter note" should "redraw only the rows of the ghost that changed" in
    keepsUnaffectedRows(rewrittenNote)

  "The first prose under an empty chapter" should "redraw only the rows the ghost left" in
    keepsUnaffectedRows(firstProse)

  private def keepsUnaffectedRows(transition: (AppState, AppState)): Unit =
    val (before, after) = transition
    val frames          = PersistingImage()
    val (_, first)      = frames.render(before, Damage.Everything)

    val (_, changed) = frames.render(after, DamageProducer.forTransition(before, after))

    withClue(s"first ${describe(first)}; changed ${describe(changed)}: ") {
      changed(FrameCounter.RowsDrawn) should be >= 1L
      changed(FrameCounter.RowsDrawn) should be < (first(FrameCounter.RowsDrawn) / 4)
      changed(FrameCounter.RowsDrawn) + changed(FrameCounter.RowsPreserved) shouldBe first(FrameCounter.RowsDrawn)
    }

  "Frames of a pane showing chapter ghosts" should "equal frames drawn from scratch, there and back" in
    List(typing, rewrittenNote, firstProse).foreach { (before, after) =>
      val frames = PersistingImage()
      val _      = frames.render(before, Damage.Everything)

      val (there, _) = frames.render(after, DamageProducer.forTransition(before, after))
      val thereDiffs = differing(there, after)
      val (back, _)  = frames.render(before, DamageProducer.forTransition(after, before))
      val backDiffs  = differing(back, before)

      withClue(s"the frame changed into, ${summary(thereDiffs)}: ")(thereDiffs shouldBe empty)
      withClue(s"the frame changed back out of, ${summary(backDiffs)}: ")(backDiffs shouldBe empty)
    }

  private def withSpacedHeadings(state: AppState): AppState =
    state.copy(persisted =
      state.persisted.copy(config =
        state.persisted.config
          .withMarkdownViewMode(MarkdownViewMode.LivePreview)
          .withHeadingsConfig(
            HeadingsConfig().withLevel(1)(_.copy(spaceBefore = Some(1.0f), spaceAfter = Some(0.5f)))
          )
      )
    )

  private def spaced(transition: (AppState, AppState)): (AppState, AppState) =
    withSpacedHeadings(transition._1) -> withSpacedHeadings(transition._2)

  "The fixture with spaced headings" should "keep the ghost on the blank lines under the heading, below its space" in {
    val (before, _) = spaced(typing)
    val scene       = RenderCaches.create().authoritativeScene.forState(before, viewport, codeFont, textFont)
    val rows        = scene.textSnapshot(paneId).map(_.visualLines).getOrElse(fail("expected a snapshot"))

    rows.map(_.bufferLine).take(4) shouldBe Vector(0, 1, 2, 3)
    rows.find(_.bufferLine == 4).map(_.spaceBeforePx).getOrElse(0) should be > 0
    rows.find(_.bufferLine == 0).map(_.spaceAfterPx).getOrElse(0) should be > 0
  }

  "A typing frame under spaced headings" should "redraw the typed row and keep the rest" in
    keepsUnaffectedRows(spaced(typing))

  "A rewritten chapter note under spaced headings" should "redraw only the rows of the ghost that changed" in
    keepsUnaffectedRows(spaced(rewrittenNote))

  "Frames of a pane showing chapter ghosts under spaced headings" should
    "equal frames drawn from scratch, there and back" in
    List(typing, rewrittenNote, firstProse).map(spaced).foreach { (before, after) =>
      val frames = PersistingImage()
      val _      = frames.render(before, Damage.Everything)

      val (there, _) = frames.render(after, DamageProducer.forTransition(before, after))
      val thereDiffs = differing(there, after)
      val (back, _)  = frames.render(before, DamageProducer.forTransition(after, before))
      val backDiffs  = differing(back, before)

      withClue(s"the frame changed into, ${summary(thereDiffs)}: ")(thereDiffs shouldBe empty)
      withClue(s"the frame changed back out of, ${summary(backDiffs)}: ")(backDiffs shouldBe empty)
    }
