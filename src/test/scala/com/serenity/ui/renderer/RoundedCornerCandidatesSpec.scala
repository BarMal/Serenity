package com.serenity.ui.renderer

import java.awt.geom.RoundRectangle2D
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Dimension, Font, RenderingHints}
import java.nio.file.{Files, Path, Paths}
import javax.imageio.ImageIO
import javax.swing.JPanel

import scala.jdk.CollectionConverters.*

import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.state.reducers.{CompositionPreview, CompositionRevisions}
import com.serenity.ui.color.RenderColor.toAwt
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389, throwaway: what a floating surface looks like with its corners rounded to a few candidate radii, for the
  * owner to pick a default from. NOT the feature. Each surface is drawn twice by the real renderer, with and without
  * it; the rounded variant takes the surface's pixels inside an anti-aliased rounded rectangle and the page's pixels
  * outside it, and strokes the arc in the theme's border colour. The real renderer's anti-aliasing will differ a little.
  * Radii are logical pixels at scale 1; scale 2 is the same layout drawn with a font twice the size and the radius
  * doubled, as the pre-#1811 `ui.corner_radius` was scaled by the UI chrome scale.
  */
class RoundedCornerCandidatesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  final private case class Candidate(radius: Int, caption: String)

  private val candidates = List(
    Candidate(0, "0 px (square, as today)"),
    Candidate(4, "4 px"),
    Candidate(8, "8 px (the pre-#1811 default)"),
    Candidate(12, "12 px (about half a row)")
  )

  final private case class Scene(name: String, surface: SurfaceContent)

  private val rows     = 14
  private val columns  = 60
  private val bufferId = BufferId(1)
  private val paneId   = PaneId(0)
  private val baseSize = 18.0f

  private val scenes = List(
    Scene(
      "context-menu",
      SurfaceContent.ContextMenu(
        ContextMenu(
          title = "editor",
          targetFocus = Focus.EditorPane(paneId),
          items = List("Save", "Cut", "Copy", "Paste").map { label =>
            val command = Command.typed(label, label, CommandIntent.Edit(EditIntent.Copy), label = label)
            ContextMenuItem(command.name, label, command)
          }
        )
      )
    ),
    Scene("quick-info", SurfaceContent.QuickInfo("fox: a small omnivorous mammal\nSee also: dog"))
  )

  private val revisions = new CompositionRevisions

  private val baseFont =
    Font.createFont(
      Font.TRUETYPE_FONT,
      Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile
    )

  private def stateFor(scene: Scene, viewport: ViewportSize, withSurface: Boolean): AppState =
    val buffer = Buffer
      .fromString(bufferId, "The quick brown fox jumps over the lazy dog.\n" * 6)
      .copy(
        editing = EditingState(List(CursorPosition(0, 10))),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = rows, visibleColumns = viewport.width)
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
    if !withSurface then state
    else
      state.copy(runtime =
        state.runtime.copy(uiSurfaces =
          List(
            UiSurface(
              SurfaceId(scene.name),
              scene.surface,
              SurfacePresentation.Floating(Some(CursorPosition(0, 10)), SurfacePlacement.BelowCursor)
            )
          )
        )
      )

  private def render(state: AppState, scale: Int): BufferedImage =
    val codeFont = baseFont.deriveFont(Font.PLAIN, baseSize * scale)
    val metrics  = CellMetrics.fromFont(codeFont)
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

  final private case class Box(x: Int, y: Int, width: Int, height: Int)

  /** Where the two renders differ: the surface and nothing else. */
  private def differingBox(with_ : BufferedImage, without: BufferedImage): Option[Box] =
    val points = for
      y <- 0 until with_.getHeight
      x <- 0 until with_.getWidth
      if with_.getRGB(x, y) != without.getRGB(x, y)
    yield (x, y)
    Option.when(points.nonEmpty)(
      Box(points.map(_._1).min, points.map(_._2).min, points.map(_._1).max - points.map(_._1).min + 1,
        points.map(_._2).max - points.map(_._2).min + 1)
    )

  private def rounded(with_ : BufferedImage, without: BufferedImage, box: Box, radius: Int, scale: Int): BufferedImage =
    if radius == 0 then with_
    else
      val out = new BufferedImage(with_.getWidth, with_.getHeight, BufferedImage.TYPE_INT_RGB)
      val mask = new BufferedImage(with_.getWidth, with_.getHeight, BufferedImage.TYPE_INT_ARGB)
      val arc  = 2.0 * radius * scale
      val mg   = mask.createGraphics()
      mg.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      mg.setColor(Color.WHITE)
      mg.fill(new RoundRectangle2D.Double(box.x, box.y, box.width, box.height, arc, arc))
      mg.dispose()
      for
        y <- 0 until out.getHeight
        x <- 0 until out.getWidth
      do
        val a = (mask.getRGB(x, y) >>> 24) / 255.0
        val s = with_.getRGB(x, y)
        val p = without.getRGB(x, y)
        def mix(shift: Int): Int =
          math.round(((s >> shift) & 0xff) * a + ((p >> shift) & 0xff) * (1 - a)).toInt
        out.setRGB(x, y, (mix(16) << 16) | (mix(8) << 8) | mix(0))
      val g = out.createGraphics()
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
      val thickness = scale.toFloat
      g.setStroke(new BasicStroke(thickness))
      g.setColor(Theme.light.border.toAwt)
      val inset = thickness / 2.0
      g.draw(
        new RoundRectangle2D.Double(
          box.x + inset,
          box.y + inset,
          box.width - thickness,
          box.height - thickness,
          arc - thickness,
          arc - thickness
        )
      )
      g.dispose()
      out

  private def crop(image: BufferedImage, box: Box, margin: Int): BufferedImage =
    val x = (box.x - margin).max(0)
    val y = (box.y - margin).max(0)
    val w = (box.width + 2 * margin).min(image.getWidth - x)
    val h = (box.height + 2 * margin).min(image.getHeight - y)
    image.getSubimage(x, y, w, h)

  private def sheet(title: String, tiles: List[(String, BufferedImage)]): BufferedImage =
    val pad      = 24
    val caption  = 34
    val tileW    = tiles.map(_._2.getWidth).max
    val tileH    = tiles.map(_._2.getHeight).max
    val columns2 = 2
    val rows2    = (tiles.size + columns2 - 1) / columns2
    val width    = pad + columns2 * (tileW + pad)
    val height   = caption + pad + rows2 * (tileH + caption + pad)
    val out      = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g        = out.createGraphics()
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, width, height)
    g.setColor(Color.BLACK)
    g.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 20))
    g.drawString(title, pad, 26)
    g.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 18))
    tiles.zipWithIndex.foreach {
      case ((label, tile), index) =>
        val x = pad + (index % columns2) * (tileW + pad)
        val y = caption + pad + (index / columns2) * (tileH + caption + pad)
        g.setColor(Color.BLACK)
        g.drawString(label, x, y - 8)
        g.drawImage(tile, x, y, null)
    }
    g.dispose()
    out

  "Rounded-corner candidates" should "be drawn for each surface at scale 1 and 2" in {
    val directory: Path = Paths.get("target", "test-reports", "corner-candidates")
    Files.createDirectories(directory)
    val drawn = for
      scene <- scenes
      scale <- List(1, 2)
    yield
      val viewport = ViewportSize(columns, rows)
      val with_    = render(stateFor(scene, viewport, withSurface = true), scale)
      val without  = render(stateFor(scene, viewport, withSurface = false), scale)
      val box = differingBox(with_, without).getOrElse(fail(s"${scene.name}: the surface drew nothing"))
      val tiles = candidates.map { candidate =>
        val image = rounded(with_, without, box, candidate.radius, scale)
        ImageIO.write(image, "png", directory.resolve(s"${scene.name}-r${candidate.radius}-scale$scale.png").toFile)
        candidate.caption -> crop(image, box, margin = 12 * scale)
      }
      ImageIO.write(
        sheet(s"${scene.name}, scale $scale (mock-up of the rounded frame)", tiles),
        "png",
        directory.resolve(s"sheet-${scene.name}-scale$scale.png").toFile
      ) shouldBe true
      s"${scene.name} scale $scale: surface ${box.width}x${box.height}px at (${box.x},${box.y})"
    Files.write(directory.resolve("boxes.txt"), drawn.asJava)
    drawn should have size (scenes.size * 2)
  }
