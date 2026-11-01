package com.serenity.ui.renderer

import java.awt.Font
import java.awt.image.BufferedImage
import java.nio.file.{Files, Paths}
import javax.imageio.ImageIO

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

  final private case class Strip(
      frame: FramePixels,
      rect: LayoutRect,
      activeTab: LogicalPixelRect,
      newTab: LogicalPixelRect
  )

  private def save(frame: FramePixels, rect: LayoutRect, radius: Int): Unit =
    val top    = rect.y * metrics.lineHeight
    val height = rect.height * metrics.lineHeight
    val image  = BufferedImage(frame.widthPx, height, BufferedImage.TYPE_INT_RGB)
    for y <- 0 until height; x <- 0 until frame.widthPx do image.setRGB(x, y, frame.rgbAt(x, top + y) & 0xffffff)
    val directory = Paths.get("target", "test-reports", "tab-strip")
    Files.createDirectories(directory)
    val _ = ImageIO.write(image, "png", directory.resolve(s"${getClass.getSimpleName}-r$radius.png").toFile)

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
    val newTab = TabBarSurfaceComposition
      .newTabAffordance(entries, rect, Some(tabs(1)))
      .getOrElse(fail("no new-tab region"))
      .rect
    save(frame, rect, radius)
    Strip(frame, rect, active, newTab)

  private def rgb(frame: FramePixels, x: Int, y: Int): Int = frame.rgbAt(x, y) & 0xffffff

  private def bandOf(s: Strip): Range =
    s.rect.y * metrics.lineHeight until (s.rect.y + s.rect.height) * metrics.lineHeight
  private def stripLeft(s: Strip): Int  = s.rect.x * metrics.charWidth
  private def stripRight(s: Strip): Int = (s.rect.x + s.rect.width) * metrics.charWidth

  /** The strip's own fill: the colour most of its pixels have. */
  private def stripBackground(s: Strip): Int =
    val colours = for y <- bandOf(s); x <- stripLeft(s) until stripRight(s) yield rgb(s.frame, x, y)
    colours.groupBy(identity).maxBy(_._2.size)._1

  /** The active tab's fill: the commonest colour across the tab's span that is not the strip's own. Text ink is a few
    * pixels of many colours, so a missing fill leaves nothing here to reach the size the fill must have.
    */
  private def highlight(s: Strip): Int =
    val background = stripBackground(s)
    val from       = (s.activeTab.x * metrics.charWidth).toInt
    val until      = ((s.activeTab.x + s.activeTab.width) * metrics.charWidth).toInt.min(stripRight(s))
    val others     = for y <- bandOf(s); x <- from until until yield rgb(s.frame, x, y)
    val fill       = others.filter(_ != background).groupBy(identity).maxByOption(_._2.size)
    withClue("no fill other than the strip's own in the active tab: ")(fill should not be empty)
    val (colour, pixels) = fill.map((c, ps) => (c, ps.size)).getOrElse((background, 0))
    val needed           = 3 * metrics.charWidth * metrics.lineHeight
    withClue(s"the active tab's fill is only $pixels pixels, under $needed: ")(pixels should be >= needed)
    colour

  /** The smallest box holding every pixel of `color` in the strip, as (left, top, right, bottom). */
  private def boundsOf(s: Strip, color: Int): (Int, Int, Int, Int) =
    val hits = for
      y <- bandOf(s)
      x <- stripLeft(s) until stripRight(s)
      if rgb(s.frame, x, y) == color
    yield (x, y)
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

  it should "size a tab to its title and cover all of it, close box included, with the active tab's fill" in {
    val s                   = strip(radius = 0)
    val h                   = highlight(s)
    val (left, _, right, _) = boundsOf(s, h)
    val title               = "Buffer 1"
    s.activeTab.width shouldBe (title.length + 4).toDouble
    val tabLeft  = (s.activeTab.x * metrics.charWidth).toInt
    val tabRight = ((s.activeTab.x + s.activeTab.width) * metrics.charWidth).toInt
    withClue(s"fill spans $left..$right, tab $tabLeft..$tabRight: ") {
      left should be <= tabLeft + metrics.charWidth
      right should be >= tabRight - metrics.charWidth
    }
  }

  it should "draw the whole of the + in its own cells, right after the last tab" in {
    // At a zero radius the strip's own corners are plain background, so any ink in the + cells is the glyph.
    val s          = strip(radius = 0)
    val background = stripBackground(s)
    val from       = (s.newTab.x * metrics.charWidth).toInt
    val until      = ((s.newTab.x + s.newTab.width) * metrics.charWidth).toInt.min(stripRight(s))
    val ink        = for y <- bandOf(s); x <- from until until if rgb(s.frame, x, y) != background yield (x, y)
    withClue("no ink in the + cells: ")(ink should not be empty)
    val width  = ink.map(_._1).max - ink.map(_._1).min + 1
    val height = ink.map(_._2).max - ink.map(_._2).min + 1
    val raster =
      bandOf(s).map(y => (from until until).map(x => if rgb(s.frame, x, y) != background then '#' else '.').mkString)
    val picture = s"cell ${metrics.charWidth}px, the + cells:\n${raster.mkString("\n")}\n"
    withClue(s"the + is $width wide and $height tall. $picture")(width * 10 should be >= height * 7)
    withClue(s"the + starts at its cell. $picture")(ink.map(_._1).min should be > from)
    withClue(s"the + ends inside the strip. $picture")(ink.map(_._1).max should be < stripRight(s))
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
