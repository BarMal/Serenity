package com.serenity.ui.renderer

import java.awt.Font
import java.nio.file.Paths

import com.serenity.config.{AppConfig, DockedCornerStyle, PaneHeaderCorners}
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.state.reducers.{CompositionPreview, CompositionRevisions}
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme
import com.serenity.{FrameBackend, FramePixels, Java2DFrames, SkiaFrames, TestWorkspaceTrees}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389: the tab strip's active tab takes the strip's rounded shape, and its trailing `+` is whole rather than
  * cut by the strip's edge, on both backends at scale 1 and 2.
  */
abstract class RoundedTabStripRenderBehaviour extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  protected def frames: FrameBackend
  protected def scale: Int

  private val columns  = 90
  private val rowCount = 18
  private val paneId   = PaneId(0)
  private val tabs     = List(BufferId(1), BufferId(2))

  private val baseFont =
    Font.createFont(
      Font.TRUETYPE_FONT,
      Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile
    )

  private def codeFont = baseFont.deriveFont(Font.PLAIN, 14.0f * scale)
  private def metrics  = CellMetrics.fromFont(codeFont)

  private def stateWith(config: AppConfig): AppState =
    val buffers = tabs.map(id => Buffer.fromString(id, "The quick brown fox.\n" * 20))
    val base    = AppState.initial(config)
    base.copy(persisted =
      base.persisted.copy(
        buffers = buffers.map(b => b.id -> b).toMap,
        bufferOrder = tabs,
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, tabs(1))),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = Theme.light,
        focus = Focus.EditorPane(paneId)
      )
    )

  final private case class Strip(frame: FramePixels, rect: LayoutRect, activeTab: LogicalPixelRect)

  private def strip(radius: Int): Strip =
    val base = AppConfig.conventional
    val config = base
      .withUiCornerRadiusPx(radius)
      .withDockedCornerStyle(DockedCornerStyle.Floating)
      .withPaneHeaderCorners(PaneHeaderCorners.Rounded)
      .withFontConfig(base.editorConfig.fontConfig.copy(uiFontSize = 12.0f * scale))
    val state    = stateWith(config)
    val widthPx  = columns * metrics.charWidth
    val heightPx = rowCount * metrics.lineHeight
    val frame    = frames.newFrame(widthPx, heightPx)
    val caches   = RenderCaches.create()
    val surface  = frames.surfaceOver(frame, metrics, codeFont, widthPx, heightPx, 1.0, persists = false, caches)
    RendererEntryPoints.render(
      CompositionPreview.displayed(state, new CompositionRevisions),
      cursorVisible = true,
      surface,
      ViewportSize(columns, rowCount),
      FontSpec.fromAwt(codeFont),
      FontSpec.fromAwt(codeFont),
      metrics,
      None,
      caches
    )
    surface.flush()
    val rect = LayoutEngine
      .calculateLayout(state, ViewportSize(columns, rowCount))
      .tabBarRect
      .getOrElse(fail("no tab strip"))
    val entries = tabs.map(id => TabListEntry(id, s"Buffer ${id.value}", isDirty = false))
    val active = TabBarSurfaceComposition
      .forTabBar(entries, Some(tabs(1)), rect)
      .hitRegions
      .lift(1)
      .getOrElse(fail("no active tab region"))
      .rect
    Strip(frame, rect, active)

  private def rgb(frame: FramePixels, x: Int, y: Int): Int = frame.rgbAt(x, y) & 0xffffff

  private def bandOf(s: Strip): Range =
    s.rect.y * metrics.lineHeight until (s.rect.y + s.rect.height) * metrics.lineHeight
  private def stripLeft(s: Strip): Int  = s.rect.x * metrics.charWidth
  private def stripRight(s: Strip): Int = (s.rect.x + s.rect.width) * metrics.charWidth

  /** The active tab's fill colour, read from the open stretch between its title and its close glyph. */
  private def highlight(s: Strip): Int =
    val x = ((s.activeTab.x + s.activeTab.width * 0.6) * metrics.charWidth).toInt
    rgb(s.frame, x, bandOf(s).start + bandOf(s).size / 2)

  /** The smallest box holding every pixel of `color` in the strip, as (left, top, right, bottom). */
  private def boundsOf(s: Strip, color: Int): (Int, Int, Int, Int) =
    val hits = for
      y <- bandOf(s)
      x <- stripLeft(s) until stripRight(s)
      if rgb(s.frame, x, y) == color
    yield (x, y)
    withClue("no pixel of the active tab's colour: ")(hits should not be empty)
    (hits.map(_._1).min, hits.map(_._2).min, hits.map(_._1).max, hits.map(_._2).max)

  private def cornersOf(b: (Int, Int, Int, Int)): List[(Int, Int)] =
    List((b._1, b._2), (b._3, b._2), (b._3, b._4), (b._1, b._4))

  "A rounded tab strip" should "round all four corners of its active tab" in {
    val s = strip(radius = 12)
    val h = highlight(s)
    cornersOf(boundsOf(s, h)).foreach {
      case (x, y) => withClue(s"active tab corner ($x,$y): ")(rgb(s.frame, x, y) should not be h)
    }
  }

  it should "leave the active tab's corners square at a zero radius" in {
    val s = strip(radius = 0)
    val h = highlight(s)
    cornersOf(boundsOf(s, h)).foreach {
      case (x, y) => withClue(s"active tab corner ($x,$y): ")(rgb(s.frame, x, y) shouldBe h)
    }
  }

  it should "draw the whole of the trailing + inside the strip" in {
    // At a zero radius the strip's own corners are plain background, so any ink is the glyph.
    val s          = strip(radius = 0)
    val colours    = for y <- bandOf(s); x <- stripLeft(s) until stripRight(s) yield rgb(s.frame, x, y)
    val background = colours.groupBy(identity).maxBy(_._2.size)._1
    val tail       = stripRight(s) - metrics.charWidth * 4 until stripRight(s)
    val ink        = for y <- bandOf(s); x <- tail if rgb(s.frame, x, y) != background yield (x, y)
    withClue("no ink at the strip's trailing end: ")(ink should not be empty)
    // The rightmost run of inked columns is the +; the previous tab's close glyph sits further left, past a gap.
    val columns = ink.map(_._1).distinct.sorted.reverse
    val left    = columns.tail.foldLeft(columns.head)((edge, column) => if edge - column <= 2 then column else edge)
    val glyph   = ink.filter((x, _) => x >= left)
    val width   = glyph.map(_._1).max - glyph.map(_._1).min + 1
    val height  = glyph.map(_._2).max - glyph.map(_._2).min + 1
    withClue("the + reaches the strip's edge: ")(glyph.map(_._1).max should be <= stripRight(s) - 3)
    withClue(s"the + is $width wide and $height tall: ")(width * 10 should be >= height * 7)
  }

class Java2DRoundedTabStripScale1Spec extends RoundedTabStripRenderBehaviour:
  override protected def frames: FrameBackend = Java2DFrames
  override protected def scale: Int           = 1

class Java2DRoundedTabStripScale2Spec extends RoundedTabStripRenderBehaviour:
  override protected def frames: FrameBackend = Java2DFrames
  override protected def scale: Int           = 2

class SkiaRoundedTabStripScale1Spec extends RoundedTabStripRenderBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
  override protected def scale: Int           = 1

class SkiaRoundedTabStripScale2Spec extends RoundedTabStripRenderBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
  override protected def scale: Int           = 2
