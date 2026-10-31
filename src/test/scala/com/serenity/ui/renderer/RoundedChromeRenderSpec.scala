package com.serenity.ui.renderer

import java.awt.Font
import java.nio.file.Paths

import com.serenity.config.{AppConfig, DockedCornerStyle, PaneHeaderCorners}
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

/** Issue #2389: the real renderer's docked panels, status bar and pane header with their corners rounded or square, on
  * both backends and at scale 1 and 2 (scale 2 is the same layout with a font, cells and UI font twice the size, so the
  * radius doubles with the chrome scale). Pixel colours at the corners only: a rounded corner shows the page, a square
  * one shows the element's own colour or border.
  */
abstract class RoundedChromeRenderBehaviour extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  protected def frames: FrameBackend
  protected def scale: Int

  private val columns  = 90
  private val rowCount = 18
  private val bufferId = BufferId(1)
  private val paneId   = PaneId(0)

  private val baseFont =
    Font.createFont(
      Font.TRUETYPE_FONT,
      Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile
    )

  private def codeFont = baseFont.deriveFont(Font.PLAIN, 14.0f * scale)
  private def metrics  = CellMetrics.fromFont(codeFont)

  private def configWith(
    radius: Int,
    style: DockedCornerStyle,
    header: PaneHeaderCorners
  ): AppConfig =
    val base = AppConfig.conventional
    base
      .withUiCornerRadiusPx(radius)
      .withDockedCornerStyle(style)
      .withPaneHeaderCorners(header)
      .withFontConfig(base.editorConfig.fontConfig.copy(uiFontSize = 12.0f * scale))

  private def stateWith(config: AppConfig): AppState =
    val buffer = Buffer
      .fromString(bufferId, "The quick brown fox jumps over the lazy dog.\n" * 20)
      .copy(
        editing = EditingState(List(CursorPosition(0, 10))),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = rowCount, visibleColumns = columns)
      )
    val base = AppState.initial(config)
    val state = base.copy(
      persisted = base.persisted.copy(
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
        SessionDockedPanel("surface-a", SessionPinnedPanel(PanelPosition.Left, 22, SessionPanelContent.Outline(Nil))),
        SessionDockedPanel(
          "surface-b",
          SessionPinnedPanel(PanelPosition.Right, 22, SessionPanelContent.Diagnostics(Nil))
        )
      )
    )
    UiPreset.applyToState(preset, state, Some(Theme.light))

  final private case class Scene(frame: FramePixels, left: LayoutRect, status: LayoutRect, header: LayoutRect)

  private def scene(config: AppConfig): Scene =
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
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(columns, rowCount))
    val pane   = LayoutEngine.calculateEditorWorkspaceLayout(state, layout).paneLayouts(paneId)
    Scene(
      frame,
      layout.pinnedPanelRects.getOrElse(PanelPosition.Left, fail("no left panel")),
      layout.gutterRect.getOrElse(fail("no status bar")),
      pane.headerRect
    )

  private def corners(rect: LayoutRect): List[(Int, Int)] =
    val left   = rect.x * metrics.charWidth
    val top    = rect.y * metrics.lineHeight
    val right  = (rect.x + rect.width) * metrics.charWidth - 1
    val bottom = (rect.y + rect.height) * metrics.lineHeight - 1
    List((left, top), (right, top), (right, bottom), (left, bottom))

  private def rgb(frame: FramePixels, point: (Int, Int)): Int = frame.rgbAt(point._1, point._2) & 0xffffff

  private val defaultRadius = 12

  /** The same scene with every corner square: what each corner pixel looks like when nothing rounds. */
  private def square(style: DockedCornerStyle, header: PaneHeaderCorners): Scene =
    scene(configWith(0, style, header))

  private def rounded(style: DockedCornerStyle, header: PaneHeaderCorners): Scene =
    scene(configWith(defaultRadius, style, header))

  /** A rounded corner shows the page instead of the element, so its pixel differs from the square render's. */
  private def isRounded(rounded: Scene, square: Scene, point: (Int, Int)): Boolean =
    rgb(rounded.frame, point) != rgb(square.frame, point)

  "With floating docked chrome" should "round every corner of a docked panel and of the status bar" in {
    val r = rounded(DockedCornerStyle.Floating, PaneHeaderCorners.Rounded)
    val q = square(DockedCornerStyle.Floating, PaneHeaderCorners.Rounded)
    corners(r.left).foreach(p => withClue(s"panel corner $p: ")(isRounded(r, q, p) shouldBe true))
    corners(r.status).foreach(p => withClue(s"status corner $p: ")(isRounded(r, q, p) shouldBe true))
  }

  it should "round a pane header's two top corners when the header corners are rounded, and not its bottom ones" in {
    val r = rounded(DockedCornerStyle.Floating, PaneHeaderCorners.Rounded)
    val q = square(DockedCornerStyle.Floating, PaneHeaderCorners.Rounded)
    val c = corners(r.header)
    isRounded(r, q, c(0)) shouldBe true
    isRounded(r, q, c(1)) shouldBe true
    isRounded(r, q, c(2)) shouldBe false
    isRounded(r, q, c(3)) shouldBe false
  }

  it should "leave a pane header square when the header corners are square" in {
    val r = rounded(DockedCornerStyle.Floating, PaneHeaderCorners.Square)
    val q = square(DockedCornerStyle.Floating, PaneHeaderCorners.Square)
    corners(r.header).foreach(p => withClue(s"header corner $p: ")(isRounded(r, q, p) shouldBe false))
  }

  "With flush docked chrome" should "round only the left panel's bottom-right corner, and leave the status bar square" in {
    val r = rounded(DockedCornerStyle.Flush, PaneHeaderCorners.Rounded)
    val q = square(DockedCornerStyle.Flush, PaneHeaderCorners.Rounded)
    val c = corners(r.left)
    isRounded(r, q, c(2)) shouldBe true
    List(c(0), c(1), c(3)).foreach(p => withClue(s"panel corner $p: ")(isRounded(r, q, p) shouldBe false))
    corners(r.status).foreach(p => withClue(s"status corner $p: ")(isRounded(r, q, p) shouldBe false))
  }

class Java2DRoundedChromeScale1Spec extends RoundedChromeRenderBehaviour:
  override protected def frames: FrameBackend = Java2DFrames
  override protected def scale: Int           = 1

class Java2DRoundedChromeScale2Spec extends RoundedChromeRenderBehaviour:
  override protected def frames: FrameBackend = Java2DFrames
  override protected def scale: Int           = 2

class SkiaRoundedChromeScale1Spec extends RoundedChromeRenderBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
  override protected def scale: Int           = 1

class SkiaRoundedChromeScale2Spec extends RoundedChromeRenderBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
  override protected def scale: Int           = 2
