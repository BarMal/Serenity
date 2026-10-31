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

/** Issue #2389, throwaway: a 2x2 of how docked panels, the status bar and the editor pane's header band meet the window's
  * edges. Columns: B (corners on the window edge stay square; the layout is today's) and D (the panels and the status bar
  * are inset from the window edges and from the workspace by one cell, so every corner floats). Rows: the header band's
  * top corners square, and rounded. NOT the feature. D is built from a real render of a window one cell narrower on each
  * side and three rows shorter, re-assembled with the gaps; the corner pixels take the window background colour. The
  * real renderer's anti-aliasing and a real layout change will differ. Radius 12 px, the owner's default.
  */
class DockedCornerGridSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val columns  = 90
  private val rows     = 18
  private val gapCells = 1
  private val radiusPx = 12.0
  private val bufferId = BufferId(1)
  private val paneId   = PaneId(0)

  final private case class Box(x: Int, y: Int, width: Int, height: Int)

  final private case class Cut(box: Box, tl: Boolean, tr: Boolean, br: Boolean, bl: Boolean, stroke: Boolean)

  private val baseFont =
    Font.createFont(
      Font.TRUETYPE_FONT,
      Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile
    )

  private val revisions = new CompositionRevisions

  private def stateFor(cols: Int, rowCount: Int): AppState =
    val buffer = Buffer
      .fromString(bufferId, "The quick brown fox jumps over the lazy dog.\n" * 20)
      .copy(
        editing = EditingState(List(CursorPosition(0, 10))),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = rowCount, visibleColumns = cols)
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

  private def render(cols: Int, rowCount: Int, metrics: CellMetrics, codeFont: Font): (AppState, BufferedImage) =
    val state    = stateFor(cols, rowCount)
    val widthPx  = cols * metrics.charWidth
    val heightPx = rowCount * metrics.lineHeight
    val image    = new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_RGB)
    val panel    = new JPanel
    panel.setPreferredSize(new Dimension(widthPx, heightPx))
    panel.setSize(new Dimension(widthPx, heightPx))
    val surface = Java2DRenderSurface.forImage(image, metrics, codeFont, panel, _ => (), SurfaceTextCaches.fresh())
    RendererEntryPoints.render(
      CompositionPreview.displayed(state, revisions),
      cursorVisible = true,
      surface,
      ViewportSize(cols, rowCount),
      FontSpec.fromAwt(codeFont),
      FontSpec.fromAwt(codeFont),
      metrics,
      None,
      RenderCaches.create()
    )
    surface.flush()
    (state, image)

  final private case class Layouts(left: Box, right: Box, status: Box, band: Box)

  private def layouts(state: AppState, cols: Int, rowCount: Int, metrics: CellMetrics, image: BufferedImage): Layouts =
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(cols, rowCount))
    def px(rect: LayoutRect) =
      Box(rect.x * metrics.charWidth, rect.y * metrics.lineHeight, rect.width * metrics.charWidth, rect.height * metrics.lineHeight)
    val left   = px(layout.pinnedPanelRects.getOrElse(PanelPosition.Left, fail("no left panel")))
    val right  = px(layout.pinnedPanelRects.getOrElse(PanelPosition.Right, fail("no right panel")))
    val status = px(layout.gutterRect.getOrElse(fail("no status bar")))
    // The header band is the run of one colour along the first pixel row of the workspace, right of the left panel.
    val startX = left.x + left.width + metrics.charWidth + 3
    val colour = image.getRGB(startX, 1)
    val bandLeft =
      (startX to 0 by -1).takeWhile(x => image.getRGB(x, 1) == colour).lastOption.getOrElse(startX)
    val bandRight =
      (startX until image.getWidth).takeWhile(x => image.getRGB(x, 1) == colour).lastOption.getOrElse(startX)
    val bandBottom =
      (1 until image.getHeight).takeWhile(y => image.getRGB(startX, y) == colour).lastOption.getOrElse(1)
    val band = Box(bandLeft, 0, bandRight - bandLeft + 1, bandBottom + 1)
    Layouts(left, right, status, band)

  private def roundedPath(box: Box, r: Double, cut: Cut): Path2D.Double =
    val k = 0.5522847498 * r
    val x = box.x.toDouble
    val y = box.y.toDouble
    val w = box.width.toDouble
    val h = box.height.toDouble
    val p = new Path2D.Double()
    p.moveTo(x + (if cut.tl then r else 0), y)
    p.lineTo(x + w - (if cut.tr then r else 0), y)
    if cut.tr then p.curveTo(x + w - r + k, y, x + w, y + r - k, x + w, y + r)
    p.lineTo(x + w, y + h - (if cut.br then r else 0))
    if cut.br then p.curveTo(x + w, y + h - r + k, x + w - r + k, y + h, x + w - r, y + h)
    p.lineTo(x + (if cut.bl then r else 0), y + h)
    if cut.bl then p.curveTo(x + r - k, y + h, x, y + h - r + k, x, y + h - r)
    p.lineTo(x, y + (if cut.tl then r else 0))
    if cut.tl then p.curveTo(x, y + r - k, x + r - k, y, x + r, y)
    p.closePath()
    p

  private def borderThickness(image: BufferedImage, box: Box): Int =
    val border = Theme.light.border.toAwt.getRGB & 0xffffff
    val y      = box.y + box.height / 2
    (box.x until box.x + box.width).takeWhile(x => (image.getRGB(x, y) & 0xffffff) == border).size.max(1)

  private def recut(source: BufferedImage, cuts: List[Cut]): BufferedImage =
    val out = new BufferedImage(source.getWidth, source.getHeight, BufferedImage.TYPE_INT_RGB)
    val g0  = out.createGraphics()
    g0.drawImage(source, 0, 0, null)
    g0.dispose()
    val background = Theme.light.background.toAwt
    cuts.foreach { cut =>
      val box  = cut.box
      val mask = new BufferedImage(source.getWidth, source.getHeight, BufferedImage.TYPE_INT_ARGB)
      val mg   = mask.createGraphics()
      mg.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      mg.setColor(Color.WHITE)
      mg.fill(roundedPath(box, radiusPx, cut))
      mg.dispose()
      for
        y <- box.y until (box.y + box.height).min(source.getHeight)
        x <- box.x until (box.x + box.width).min(source.getWidth)
      do
        val a = (mask.getRGB(x, y) >>> 24) / 255.0
        val s = source.getRGB(x, y)
        def mix(shift: Int, bg: Int): Int = math.round(((s >> shift) & 0xff) * a + bg * (1 - a)).toInt
        out.setRGB(x, y, (mix(16, background.getRed) << 16) | (mix(8, background.getGreen) << 8) | mix(0, background.getBlue))
      if cut.stroke then
        val t = borderThickness(source, box).toFloat
        val g = out.createGraphics()
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
        g.setStroke(new BasicStroke(t))
        g.setColor(Theme.light.border.toAwt)
        val inner = Box(box.x + (t / 2).toInt, box.y + (t / 2).toInt, box.width - t.toInt, box.height - t.toInt)
        g.draw(roundedPath(inner, radiusPx - t / 2.0, cut))
        g.dispose()
    }
    out

  private def headerCut(band: Box, rounded: Boolean): List[Cut] =
    if rounded then List(Cut(band, tl = true, tr = true, br = false, bl = false, stroke = false)) else Nil

  /** B: today's layout; a corner on the window edge stays square, except the header band's two top corners when asked. */
  private def variantB(image: BufferedImage, l: Layouts, headerRounded: Boolean): BufferedImage =
    val w = image.getWidth
    val panelCut = (box: Box) =>
      val left  = box.x > 0
      val right = box.x + box.width < w
      Cut(box, tl = false, tr = right, br = right, bl = left, stroke = true)
    // A panel flush on the top edge keeps its top corners square; on the bottom it sits on the status bar, so its
    // bottom corners facing the workspace round.
    val leftPanel  = Cut(l.left, tl = false, tr = false, br = true, bl = false, stroke = true)
    val rightPanel = Cut(l.right, tl = false, tr = false, br = false, bl = true, stroke = true)
    val _          = panelCut
    recut(image, List(leftPanel, rightPanel) ++ headerCut(l.band, headerRounded))

  /** D: the window is assembled from a render one cell narrower on each side and three rows shorter. */
  private def variantD(
    reduced: BufferedImage,
    l: Layouts,
    metrics: CellMetrics,
    headerRounded: Boolean
  ): BufferedImage =
    val gx     = gapCells * metrics.charWidth
    val gy     = gapCells * metrics.lineHeight
    val out    = new BufferedImage(columns * metrics.charWidth, rows * metrics.lineHeight, BufferedImage.TYPE_INT_RGB)
    val g      = out.createGraphics()
    val bg     = Theme.light.background.toAwt
    g.setColor(bg)
    g.fillRect(0, 0, out.getWidth, out.getHeight)
    val contentHeight = l.status.y
    g.drawImage(reduced.getSubimage(0, 0, reduced.getWidth, contentHeight), gx, gy, null)
    g.drawImage(
      reduced.getSubimage(0, l.status.y, reduced.getWidth, l.status.height),
      gx,
      gy + contentHeight + gy,
      null
    )
    g.dispose()
    def shifted(b: Box, extraY: Int = 0) = Box(b.x + gx, b.y + gy + extraY, b.width, b.height)
    val left   = shifted(l.left)
    val right  = shifted(l.right)
    val status = shifted(l.status, extraY = gy)
    val band   = shifted(l.band)
    val all    = (box: Box, stroke: Boolean) => Cut(box, tl = true, tr = true, br = true, bl = true, stroke = stroke)
    recut(out, List(all(left, true), all(right, true), all(status, false)) ++ headerCut(band, headerRounded))

  private def sheet(title: String, tiles: List[(String, BufferedImage)]): BufferedImage =
    val pad     = 24
    val caption = 34
    val tileW   = tiles.map(_._2.getWidth).max
    val tileH   = tiles.map(_._2.getHeight).max
    val cols2   = 2
    val rows2   = (tiles.size + cols2 - 1) / cols2
    val width   = pad + cols2 * (tileW + pad)
    val height  = caption + rows2 * (tileH + caption + pad)
    val out     = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g       = out.createGraphics()
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, width, height)
    g.setColor(Color.BLACK)
    g.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 20))
    g.drawString(title, pad, 26)
    g.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 17))
    tiles.zipWithIndex.foreach {
      case ((label, tile), index) =>
        val x = pad + (index % cols2) * (tileW + pad)
        val y = caption + (index / cols2) * (tileH + caption + pad) + caption
        g.drawString(label, x, y - 8)
        g.drawImage(tile, x, y, null)
    }
    g.dispose()
    out

  "The docked-panel grid" should "be drawn for B and D with the header band square and rounded" in {
    val directory: Path = Paths.get("target", "test-reports", "corner-candidates")
    Files.createDirectories(directory)
    val codeFont = baseFont.deriveFont(Font.PLAIN, 14.0f)
    val metrics  = CellMetrics.fromFont(codeFont)

    val (flushState, flush) = render(columns, rows, metrics, codeFont)
    val flushLayouts        = layouts(flushState, columns, rows, metrics, flush)
    val reducedColumns      = columns - 2 * gapCells
    val reducedRows         = rows - 3 * gapCells
    val (reducedState, reduced) = render(reducedColumns, reducedRows, metrics, codeFont)
    val reducedLayouts          = layouts(reducedState, reducedColumns, reducedRows, metrics, reduced)

    val tiles = List(
      "B. header square" -> variantB(flush, flushLayouts, headerRounded = false),
      "D. header square" -> variantD(reduced, reducedLayouts, metrics, headerRounded = false),
      "B. header rounded" -> variantB(flush, flushLayouts, headerRounded = true),
      "D. header rounded" -> variantD(reduced, reducedLayouts, metrics, headerRounded = true)
    )
    tiles.zipWithIndex.foreach {
      case ((label, tile), index) =>
        ImageIO.write(tile, "png", directory.resolve(s"grid-$index.png").toFile)
        label should not be empty
    }
    ImageIO.write(
      sheet("Docked panels, status bar and pane header at 12 px (mock-up)", tiles),
      "png",
      directory.resolve("sheet-docked-grid.png").toFile
    ) shouldBe true
    val facts = List(
      s"window ${columns}x$rows cells = ${columns * metrics.charWidth}x${rows * metrics.lineHeight} px; cell ${metrics.charWidth}x${metrics.lineHeight} px",
      s"D renders ${reducedColumns}x$reducedRows cells and adds a $gapCells-cell gap at the left, right, top, between the workspace and the status bar, and below it",
      s"flush band ${flushLayouts.band}; reduced band ${reducedLayouts.band}"
    )
    Files.write(directory.resolve("grid-facts.txt"), facts.asJava)
    facts should have size 3
  }
