package com.serenity.ui.renderer

import java.nio.file.Path

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.state.reducers.{CompositionPreview, CompositionRevisions}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme
import com.serenity.{FrameBackend, FramePixels, SkiaFrames, TestWorkspaceTrees}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389: the separator between two tabs is a bar on every renderer. It used to be the glyph `│` set in the UI
  * font, which Java2D drew as a missing-glyph box wherever the face the app resolves for the UI font has no U+2502 (it
  * does not fall back as Skia does), so the bar is drawn as pixels and does not depend on the font.
  */
abstract class TabSeparatorRenderBehaviour extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  protected def frames: FrameBackend
  protected def scale: Int

  private val columns = 90
  private val rows    = 12
  private val paneId  = PaneId(0)

  private def stateWith(config: AppConfig): AppState =
    val buffers = List("README.md", "notes.txt").zipWithIndex.map((name, i) =>
      Buffer.fromFile(BufferId(i + 1), Path.of("/tmp", name), "text\n")
    )
    val base = AppState.initial(config)
    base.copy(
      persisted = base.persisted.copy(
        buffers = buffers.map(b => b.id -> b).toMap,
        bufferOrder = buffers.map(_.id),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffers.head.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = Theme.light,
        focus = Focus.EditorPane(paneId)
      ),
      runtime = base.runtime.copy(viewportSize = Some(ViewportSize(columns, rows)))
    )

  /** The ink in the cell after the first tab: how many pixel columns and rows have any, how many colours it is drawn
    * in, and the margins above and below it.
    */
  final private case class Ink(
      columns: Int,
      rows: Int,
      colours: Int,
      above: Int,
      below: Int,
      lineHeight: Int,
      cell: Int = 0,
      picture: String = ""
  )

  private def separatorInk(): Ink =
    val base = AppConfig.conventional
    val config =
      base.withFontConfig(base.editorConfig.fontConfig.copy(fontSize = 14.0f * scale, uiFontSize = 12.0f * scale))
    val state    = stateWith(config)
    val codeFont = FontLoader.previewCodeFont(config.editorConfig.fontConfig)
    val uiFont   = FontLoader.previewUiFont(config.editorConfig.fontConfig)
    val metrics  = CellMetrics.fromFont(codeFont)
    val widthPx  = columns * metrics.charWidth
    val heightPx = rows * metrics.lineHeight
    val frame    = frames.newFrame(widthPx, heightPx)
    val caches   = RenderCaches.create()
    val surface  = frames.surfaceOver(frame, metrics, codeFont, widthPx, heightPx, 1.0, persists = false, caches)
    RendererEntryPoints.render(
      CompositionPreview.displayed(state, new CompositionRevisions),
      cursorVisible = true,
      surface,
      ViewportSize(columns, rows),
      FontSpec.fromAwt(codeFont),
      FontSpec.fromAwt(codeFont),
      FontSpec.fromAwt(uiFont),
      metrics,
      CellMetrics.fromFont(uiFont),
      None,
      caches
    )
    surface.flush()
    val strip = LayoutEngine.calculateLayout(state, ViewportSize(columns, rows)).tabBarRect.getOrElse(fail("no strip"))
    val settings = TabBarLayoutSettings.of(state)
    val composition = state.tabBarSurface.map(_.content) match
      case Some(SurfaceContent.TabBar(entries, active)) =>
        TabBarSurfaceComposition.forTabBar(entries, active, strip, settings)
      case other => fail(s"no tab bar content: $other")
    // The separator sits in the cell just before the second tab's rect.
    val secondTab = composition.hitRegions.lift(1).getOrElse(fail("no second tab")).rect.x.toInt
    inkIn(frame, secondTab - 1, strip.y, metrics)

  private def inkIn(frame: FramePixels, cell: Int, row: Int, metrics: CellMetrics): Ink =
    val left = cell * metrics.charWidth
    val top  = row * metrics.lineHeight
    val pixels = for y <- 0 until metrics.lineHeight; x <- 0 until metrics.charWidth
    yield (x, y, frame.rgbAt(left + x, top + y) & 0xffffff)
    val background = pixels.groupBy(_._3).maxBy(_._2.size)._1
    val ink        = pixels.filter(_._3 != background)
    val inkRows    = ink.map(_._2)
    Ink(
      columns = ink.map(_._1).distinct.size,
      rows = inkRows.distinct.size,
      colours = ink.map(_._3).distinct.size,
      above = inkRows.minOption.getOrElse(metrics.lineHeight),
      below = metrics.lineHeight - 1 - inkRows.maxOption.getOrElse(-1),
      lineHeight = metrics.lineHeight,
      cell = cell,
      picture = (0 until metrics.lineHeight)
        .map(y =>
          (0 until metrics.charWidth)
            .map(x => if pixels(y * metrics.charWidth + x)._3 == background then '.' else 'X')
            .mkString
        )
        .mkString("\n", "\n", "")
    )

  "The separator between tabs" should "be a solid bar, centred in the row, whatever the UI font draws" in {
    val ink = separatorInk()

    withClue(s"$ink: one colour, so a fill and not a glyph: ")(ink.colours shouldBe 1)
    withClue(s"$ink: one pixel column wide per 16 px of row height: ")(
      ink.columns shouldBe math.max(1, ink.lineHeight / 16)
    )
    withClue(s"$ink: about four fifths of the row: ")(
      ink.rows.toDouble should (be >= 0.7 * ink.lineHeight and be <= 0.9 * ink.lineHeight)
    )
    withClue(s"$ink: centred: ")((ink.above - ink.below).abs should be <= 1)
  }

class SkiaTabSeparatorScale1Spec extends TabSeparatorRenderBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
  override protected def scale: Int           = 1

class SkiaTabSeparatorScale2Spec extends TabSeparatorRenderBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
  override protected def scale: Int           = 2
