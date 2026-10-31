package com.serenity.ui.renderer

import java.awt.geom.Path2D
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Dimension, Font, RenderingHints}
import java.nio.file.{Files, Path, Paths}
import javax.imageio.ImageIO
import javax.swing.JPanel

import scala.jdk.CollectionConverters.*

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.state.reducers.{CompositionPreview, CompositionRevisions}
import com.serenity.ui.color.RenderColor.toAwt
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389, throwaway: how rounded docked panels, the tab strip and the status bar meet the window's edges, for the
  * owner to decide on. NOT the feature. The real renderer draws the window; each docked rectangle is then re-cut with
  * rounded corners on a mock-up basis (the corner pixels take the window background colour; panels get their arc
  * stroked in the border colour), at 12 logical px (the owner's default). The real renderer's anti-aliasing will differ.
  */
class DockedCornerCandidatesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val columns  = 90
  private val rows     = 18
  private val bufferId = BufferId(1)
  private val paneId   = PaneId(0)
  private val scale    = 1
  private val radiusPx = 12 * scale

  final private case class Box(x: Int, y: Int, width: Int, height: Int, panel: Boolean)

  private val baseFont =
    Font.createFont(
      Font.TRUETYPE_FONT,
      Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile
    )

  private val revisions = new CompositionRevisions

  private def dockedState: AppState =
    val buffer = Buffer
      .fromString(bufferId, "The quick brown fox jumps over the lazy dog.\n" * 20)
      .copy(
        editing = EditingState(List(CursorPosition(0, 10))),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = rows, visibleColumns = columns)
      )
    val base = AppState.initial(AppConfig.conventional)
    val state = base.copy(
      persisted = base.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(com.serenity.TestWorkspaceTrees.linear(paneId))
        ),
        theme = Theme.light,
        focus = Focus.EditorPane(paneId)
      )
    )
    val preset = UiPreset(
      name = "Docked",
      config = AppConfig.conventional,
      themeName = Some(Theme.light.name),
      dockedPanels = List(
        SessionDockedPanel("surface-a", SessionPinnedPanel(PanelPosition.Left, 22, SessionPanelContent.Outline(Nil))),
        SessionDockedPanel(
          "surface-b",
          SessionPinnedPanel(PanelPosition.Right, 22, SessionPanelContent.Diagnostics(Nil))
        )
      )
    )
    UiPreset.applyToState(preset, state, Some(Theme.light))

  private def render(state: AppState, metrics: CellMetrics, codeFont: Font): BufferedImage =
    val widthPx  = columns * metrics.charWidth
    val heightPx = rows * metrics.lineHeight
    val image    = new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_RGB)
    val panel    = new JPanel
    panel.setPreferredSize(new Dimension(widthPx, heightPx))
    panel.setSize(new Dimension(widthPx, heightPx))
    val surface = Java2DRenderSurface.forImage(image, metrics, codeFont, panel, _ => (), SurfaceTextCaches.fresh())
    RendererEntryPoints.render(
      CompositionPreview.displayed(state, revisions),
      cursorVisible = true,
      surface,
      ViewportSize(columns, rows),
      FontSpec.fromAwt(codeFont),
      FontSpec.fromAwt(codeFont),
      metrics,
      None,
      RenderCaches.create()
    )
    surface.flush()
    image

  private def boxes(state: AppState, metrics: CellMetrics): List[Box] =
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(columns, rows))
    def px(rect: LayoutRect, panel: Boolean) =
      Box(
        rect.x * metrics.charWidth,
        rect.y * metrics.lineHeight,
        rect.width * metrics.charWidth,
        rect.height * metrics.lineHeight,
        panel
      )
    layout.pinnedPanelRects.values.toList.map(px(_, panel = true)) ++
      layout.tabBarRect.map(px(_, panel = false)) ++ layout.gutterRect.map(px(_, panel = false))

  /** Which corners round: `all` is every corner of every rectangle; `inner` leaves square any corner that lies on the
    * window's edge, so only corners that face the workspace round.
    */
  private def corners(box: Box, width: Int, height: Int, all: Boolean): (Boolean, Boolean, Boolean, Boolean) =
    if all then (true, true, true, true)
    else
      val left   = box.x > 0
      val right  = box.x + box.width < width
      val top    = box.y > 0
      val bottom = box.y + box.height < height
      (left && top, right && top, right && bottom, left && bottom)

  private def roundedPath(box: Box, r: Double, c: (Boolean, Boolean, Boolean, Boolean)): Path2D.Double =
    val (tl, tr, br, bl) = c
    val k                = 0.5522847498 * r
    val x                = box.x.toDouble
    val y                = box.y.toDouble
    val w                = box.width.toDouble
    val h                = box.height.toDouble
    val p                = new Path2D.Double()
    p.moveTo(x + (if tl then r else 0), y)
    p.lineTo(x + w - (if tr then r else 0), y)
    if tr then p.curveTo(x + w - r + k, y, x + w, y + r - k, x + w, y + r)
    p.lineTo(x + w, y + h - (if br then r else 0))
    if br then p.curveTo(x + w, y + h - r + k, x + w - r + k, y + h, x + w - r, y + h)
    p.lineTo(x + (if bl then r else 0), y + h)
    if bl then p.curveTo(x + r - k, y + h, x, y + h - r + k, x, y + h - r)
    p.lineTo(x, y + (if tl then r else 0))
    if tl then p.curveTo(x, y + r - k, x + r - k, y, x + r, y)
    p.closePath()
    p

  private def borderThickness(image: BufferedImage, box: Box): Int =
    val border = Theme.light.border.toAwt.getRGB & 0xffffff
    val y      = box.y + box.height / 2
    (box.x until box.x + box.width).takeWhile(x => (image.getRGB(x, y) & 0xffffff) == border).size.max(1)

  private def recut(source: BufferedImage, all: Boolean, rects: List[Box]): BufferedImage =
    val out = new BufferedImage(source.getWidth, source.getHeight, BufferedImage.TYPE_INT_RGB)
    val g0  = out.createGraphics()
    g0.drawImage(source, 0, 0, null)
    g0.dispose()
    val background = Theme.light.background.toAwt
    rects.foreach { box =>
      val mask = new BufferedImage(source.getWidth, source.getHeight, BufferedImage.TYPE_INT_ARGB)
      val mg   = mask.createGraphics()
      mg.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      mg.setColor(Color.WHITE)
      val c = corners(box, source.getWidth, source.getHeight, all)
      mg.fill(roundedPath(box, radiusPx.toDouble, c))
      mg.dispose()
      for
        y <- box.y until (box.y + box.height).min(source.getHeight)
        x <- box.x until (box.x + box.width).min(source.getWidth)
      do
        val a = (mask.getRGB(x, y) >>> 24) / 255.0
        val s = source.getRGB(x, y)
        def mix(shift: Int, bg: Int): Int =
          math.round(((s >> shift) & 0xff) * a + bg * (1 - a)).toInt
        val rgb = (mix(16, background.getRed) << 16) | (mix(8, background.getGreen) << 8) | mix(0, background.getBlue)
        out.setRGB(x, y, rgb)
      if box.panel then
        val t = borderThickness(source, box).toFloat
        val g = out.createGraphics()
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
        g.setStroke(new BasicStroke(t))
        g.setColor(Theme.light.border.toAwt)
        val inner = Box(box.x + (t / 2).toInt, box.y + (t / 2).toInt, box.width - t.toInt, box.height - t.toInt, true)
        g.draw(roundedPath(inner, radiusPx.toDouble - t / 2.0, c))
        g.dispose()
    }
    out

  private def sheet(title: String, tiles: List[(String, BufferedImage)]): BufferedImage =
    val pad     = 24
    val caption = 34
    val tileW   = tiles.map(_._2.getWidth).max
    val tileH   = tiles.map(_._2.getHeight).max
    val width   = tileW + 2 * pad
    val height  = caption + tiles.size * (tileH + caption + pad)
    val out     = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g       = out.createGraphics()
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, width, height)
    g.setColor(Color.BLACK)
    g.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 20))
    g.drawString(title, pad, 26)
    g.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 18))
    tiles.zipWithIndex.foreach {
      case ((label, tile), index) =>
        val y = caption + index * (tileH + caption + pad) + caption
        g.drawString(label, pad, y - 8)
        g.drawImage(tile, pad, y, null)
    }
    g.dispose()
    out

  "Rounded docked panels" should "be drawn square, with the inner corners rounded, and with every corner rounded" in {
    val directory: Path = Paths.get("target", "test-reports", "corner-candidates")
    Files.createDirectories(directory)
    val codeFont = baseFont.deriveFont(Font.PLAIN, 14.0f * scale)
    val metrics  = CellMetrics.fromFont(codeFont)
    val state    = dockedState
    val image    = render(state, metrics, codeFont)
    val rects    = boxes(state, metrics)
    rects.size should be >= 3
    val variants = List(
      "A. Square, as today" -> image,
      "B. Only the corners that face the workspace (window-edge corners stay square)" ->
        recut(image, all = false, rects),
      "C. Every corner of every panel and bar; the notch shows the window background" ->
        recut(image, all = true, rects)
    )
    variants.zipWithIndex.foreach {
      case ((_, tile), index) => ImageIO.write(tile, "png", directory.resolve(s"docked-${'a' + index}.png").toFile)
    }
    ImageIO.write(
      sheet("Docked panels, tab strip and status bar at 12 px (mock-up)", variants),
      "png",
      directory.resolve("sheet-docked.png").toFile
    ) shouldBe true
    val facts = rects.map(b => s"${if b.panel then "panel" else "bar"} ${b.width}x${b.height}px at (${b.x},${b.y})")
    Files.write(directory.resolve("docked-boxes.txt"), facts.asJava)
    facts should not be empty
  }
