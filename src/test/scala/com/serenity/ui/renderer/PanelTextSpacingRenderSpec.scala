package com.serenity.ui.renderer

import java.awt.Font
import java.awt.image.BufferedImage
import java.nio.file.{Files, Paths}
import javax.imageio.ImageIO

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.state.reducers.{CompositionPreview, CompositionRevisions}
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.theme.Theme
import com.serenity.{FrameBackend, FramePixels, Java2DFrames, SkiaFrames, TestWorkspaceTrees}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A docked panel draws its text in the proportional UI font, so a word must come out as one run of letters, not as
  * letters set on the monospace cell grid one by one, which leaves gaps inside words ("outl i ne") and lets a wide
  * letter's cell wipe the one before it ("problens"). The panel is wide enough to show the empty Diagnostics summary
  * "0 error | 0 warning | 0 info" whole, so its first content row has exactly eight words, which show up as eight
  * clusters of ink.
  */
abstract class PanelTextSpacingBehaviour extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  protected def frames: FrameBackend
  protected def scale: Int

  private val columns  = 90
  private val rowCount = 18
  private val bufferId = BufferId(1)
  private val paneId   = PaneId(0)
  private val message  = "0 error | 0 warning | 0 info"

  private val baseFont =
    Font.createFont(
      Font.TRUETYPE_FONT,
      Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile
    )

  private def codeFont = baseFont.deriveFont(Font.PLAIN, 14.0f * scale)
  private def metrics  = CellMetrics.fromFont(codeFont)

  private def state: AppState =
    val buffer = Buffer.fromString(bufferId, "The quick brown fox jumps over the lazy dog.\n" * 20)
    val config = AppConfig.conventional
    val base   = AppState.initial(config)
    val loaded = base.copy(persisted =
      base.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = Theme.light,
        focus = Focus.EditorPane(paneId)
      )
    )
    val preset = UiPreset(
      name = "Docked",
      config = config,
      themeName = Some(Theme.light.name),
      dockedPanels = List(
        SessionDockedPanel(
          "surface-a",
          SessionPinnedPanel(PanelPosition.Left, 40, SessionPanelContent.Diagnostics(Nil))
        )
      )
    )
    UiPreset.applyToState(preset, loaded, Some(Theme.light))

  final private case class Scene(frame: FramePixels, panel: LayoutRect)

  private def scene: Scene =
    val appState = state
    val widthPx  = columns * metrics.charWidth
    val heightPx = rowCount * metrics.lineHeight
    val frame    = frames.newFrame(widthPx, heightPx)
    val caches   = RenderCaches.create()
    val surface  = frames.surfaceOver(frame, metrics, codeFont, widthPx, heightPx, 1.0, persists = false, caches)
    RendererEntryPoints.render(
      CompositionPreview.displayed(appState, new CompositionRevisions),
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
    val layout = LayoutEngine.calculateLayout(appState, ViewportSize(columns, rowCount))
    Scene(frame, layout.pinnedPanelRects.getOrElse(PanelPosition.Left, fail("no left panel")))

  private def rgb(frame: FramePixels, x: Int, y: Int): Int = frame.rgbAt(x, y) & 0xffffff

  private def save(s: Scene): Unit =
    val image = BufferedImage(s.frame.widthPx, s.frame.heightPx, BufferedImage.TYPE_INT_RGB)
    for y <- 0 until s.frame.heightPx; x <- 0 until s.frame.widthPx do image.setRGB(x, y, rgb(s.frame, x, y))
    val directory = Paths.get("target", "test-reports", "panel-text-spacing")
    Files.createDirectories(directory)
    val _ = ImageIO.write(image, "png", directory.resolve(s"${getClass.getSimpleName}.png").toFile)

  /** The columns, left to right, where the row's pixels differ from the panel's own fill. */
  private def inkColumns(s: Scene, rows: Range): Vector[Int] =
    val left  = (s.panel.x + 1) * metrics.charWidth
    val right = (s.panel.right - 1) * metrics.charWidth
    val all   = for y <- rows; x <- left until right yield rgb(s.frame, x, y)
    val fill  = all.groupBy(identity).maxBy(_._2.size)._1
    (left until right).filter(x => rows.exists(y => rgb(s.frame, x, y) != fill)).toVector

  /** Runs of ink columns, a new run starting where the blank gap before a column is wider than `gap`. */
  private def clusters(columns: Vector[Int], gap: Int): List[(Int, Int)] =
    columns
      .foldLeft(List.empty[(Int, Int)]) {
        case ((start, end) :: rest, x) if x - end - 1 <= gap => (start, x) :: rest
        case (acc, x)                                        => (x, x) :: acc
      }
      .reverse

  "A docked panel's text" should "draw each word as one run of letters, with no gaps inside it" in {
    val s = scene
    save(s)
    // The title sits on the frame's top edge, so the message is on the first row below it that has any ink.
    val firstInked = (s.panel.y + 1 until s.panel.y + 5).find { row =>
      val rows = row * metrics.lineHeight until (row + 1) * metrics.lineHeight
      inkColumns(s, rows).nonEmpty
    }
    val row      = firstInked.getOrElse(fail("no text in the panel"))
    val rows     = row * metrics.lineHeight until (row + 1) * metrics.lineHeight
    val columns  = inkColumns(s, rows)
    val maxGap   = math.round(0.25 * codeFont.getSize2D).toInt
    val words    = clusters(columns, maxGap)
    val expected = message.split(" ").length
    withClue(s"$expected words in '$message', ink clusters ${words.map((a, b) => s"$a-$b").mkString(",")}: ") {
      words should have size expected
    }
  }

class Java2DPanelTextSpacingScale1Spec extends PanelTextSpacingBehaviour:
  override protected def frames: FrameBackend = Java2DFrames
  override protected def scale: Int           = 1

class Java2DPanelTextSpacingScale2Spec extends PanelTextSpacingBehaviour:
  override protected def frames: FrameBackend = Java2DFrames
  override protected def scale: Int           = 2

class SkiaPanelTextSpacingScale1Spec extends PanelTextSpacingBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
  override protected def scale: Int           = 1

class SkiaPanelTextSpacingScale2Spec extends PanelTextSpacingBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
  override protected def scale: Int           = 2
