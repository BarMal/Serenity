package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.{Dimension, Font}
import java.io.File
import java.nio.file.Paths
import javax.imageio.ImageIO
import javax.swing.JPanel

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.state.reducers.{CompositionPreview, CompositionRevisions}
import com.serenity.ui.layout.{CellMetrics, Layout, ViewportSize}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The pre-edit through the real GUI renderer onto a pixel surface (#2008). With `IME_SCREENSHOTS_DIR=<dir>` set, each
  * scene is also written there as a PNG, for the owner to look at; the assertions hold either way.
  */
class ImePreEditScreenshotSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val revisions = new CompositionRevisions

  private val widthPx  = 760
  private val heightPx = 150
  private val bufferId = BufferId(1)

  private val codeFont =
    Font
      .createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile)
      .deriveFont(Font.PLAIN, 18.0f)

  private val metrics  = CellMetrics.fromFont(codeFont)
  private val viewport = ViewportSize(widthPx / metrics.charWidth, heightPx / metrics.lineHeight)

  private def stateWith(composition: Option[Composition]): AppState =
    val buffer = Buffer
      .fromString(bufferId, "The quick brown fox jumps over the lazy dog.")
      .copy(
        editing = EditingState(List(CursorPosition(0, 10))),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 5, visibleColumns = viewport.width)
      )
    val config = AppConfig.conventional
    val base =
      AppState.initial(config.withFontConfig(config.editorConfig.fontConfig.copy(fallbackFamilies = List("IPAGothic"))))
    val state = base.copy(
      persisted = base.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = Layout(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          workspaceTree = Some(com.serenity.TestWorkspaceTrees.linear(PaneId(0)))
        ),
        theme = Theme.light,
        focus = Focus.EditorPane(PaneId(0))
      )
    )
    state.copy(runtime = state.runtime.copy(composition = composition))

  private def composing(text: String, clauses: List[CompositionClause], caret: Option[Int] = None): Composition =
    Composition(PaneId(0), bufferId, CursorPosition(0, 10), 0L, text, clauses, caret)

  private def paint(state: AppState): BufferedImage =
    val image = new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_RGB)
    val panel = new JPanel
    panel.setPreferredSize(new Dimension(widthPx, heightPx))
    panel.setSize(new Dimension(widthPx, heightPx))
    val surface = Java2DRenderSurface.forImage(image, metrics, codeFont, panel, _ => (), SurfaceTextCaches.fresh())
    RendererEntryPoints.render(
      CompositionPreview.displayed(state, revisions),
      cursorVisible = true,
      surface,
      viewport,
      FontSpec.fromAwt(codeFont),
      FontSpec.fromAwt(codeFont),
      metrics,
      None,
      RenderCaches.create()
    )
    surface.flush()
    image

  private def save(name: String, image: BufferedImage): Unit =
    sys.env.get("IME_SCREENSHOTS_DIR").foreach { dir =>
      new File(dir).mkdirs()
      val _ = ImageIO.write(image, "png", new File(dir, s"$name.png"))
    }

  private def pixelsOf(image: BufferedImage, argb: Int): Int =
    (for y <- 0 until image.getHeight; x <- 0 until image.getWidth yield image.getRGB(x, y)).count(_ == argb)

  private val selection = Theme.light.selection.background.argb

  private val raw = composing("かんじ", List(CompositionClause(0, 3, selected = false)), Some(3))

  private val converted = composing(
    "漢字変換",
    List(CompositionClause(0, 2, selected = true), CompositionClause(2, 4, selected = false)),
    Some(2)
  )

  "The pre-edit through the real renderer" should "draw unconverted text inline, underlined" in {
    val before = paint(stateWith(None))
    val after  = paint(stateWith(Some(raw)))
    save("1-unconverted", after)
    save("0-no-composition", before)

    after.getRGB(0, 0) shouldBe before.getRGB(0, 0)
    (for y <- 0 until heightPx; x <- 0 until widthPx
    yield (x, y)).exists((x, y) => after.getRGB(x, y) != before.getRGB(x, y)) shouldBe true
  }

  it should "put the clause being converted on the selection background" in {
    val unconverted = paint(stateWith(Some(raw)))
    val converting  = paint(stateWith(Some(converted)))
    save("2-converting", converting)

    pixelsOf(converting, selection) should be > pixelsOf(unconverted, selection)
  }

  it should "move the text after the pre-edit to the right" in {
    val before = paint(stateWith(None))
    val after  = paint(stateWith(Some(converted)))
    save("3-text-moved", after)

    // The pixel columns of the last glyph on the text row (below the header, above the status line).
    def lastInkColumn(image: BufferedImage): Int =
      val rows = metrics.lineHeight until 2 * metrics.lineHeight
      (0 until widthPx).reverse
        .find(x => rows.exists(y => image.getRGB(x, y) != image.getRGB(widthPx - 1, y)))
        .getOrElse(0)

    lastInkColumn(after) should be > lastInkColumn(before)
  }
