package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.nio.file.{Files, Path, Paths}
import javax.imageio.ImageIO

import com.serenity.command.*
import com.serenity.config.{AppConfig, DockedCornerStyle}
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.state.reducers.{CompositionPreview, CompositionRevisions}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.theme.Theme
import com.serenity.{FrameBackend, Java2DFrames, SkiaFrames, TestWorkspaceTrees}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389 / #2455, throwaway: the sign-off renders, every state through the real renderer on both backends (Java2D
  * and Skia raster), light and dark, at scale 1 and 2. NOT for merge.
  */
class SignOffSheetSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val columns  = 100
  private val paneId   = PaneId(0)
  private val revision = new CompositionRevisions

  private val text = "The quick brown fox jumps over the lazy dog.\n" * 30

  private def stateWith(
    config: AppConfig,
    theme: Theme,
    names: List[String],
    active: Int,
    rows: Int,
    panels: Boolean,
    menu: Boolean
  ): AppState =
    val buffers = names.zipWithIndex.map((name, i) => Buffer.fromFile(BufferId(i + 1), Path.of("/tmp", name), text))
    val cursor  = buffers(active).copy(editing = EditingState(List(CursorPosition(2, 10))))
    val all     = buffers.updated(active, cursor)
    val base    = AppState.initial(config)
    val state = base.copy(
      persisted = base.persisted.copy(
        buffers = all.map(b => b.id -> b).toMap,
        bufferOrder = all.map(_.id),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, all(active).id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = theme,
        focus = Focus.EditorPane(paneId)
      ),
      runtime = base.runtime.copy(viewportSize = Some(ViewportSize(columns, rows)))
    )
    val docked =
      if panels then
        List(
          SessionDockedPanel("outline", SessionPinnedPanel(PanelPosition.Left, 22, SessionPanelContent.Outline(Nil))),
          SessionDockedPanel(
            "diagnostics",
            SessionPinnedPanel(PanelPosition.Right, 22, SessionPanelContent.Diagnostics(Nil))
          )
        )
      else Nil
    val withDocks = UiPreset.applyToState(UiPreset("Sheet", config, Some(theme.name), dockedPanels = docked), state, Some(theme))
    if !menu then withDocks
    else
      val items = List("Save", "Save As", "Cut", "Copy", "Paste").map { label =>
        val command = Command.typed(label, label, CommandIntent.Edit(EditIntent.Copy), label = label)
        ContextMenuItem(command.name, label, command)
      }
      withDocks.copy(runtime =
        withDocks.runtime.copy(uiSurfaces =
          withDocks.runtime.uiSurfaces :+ UiSurface(
            SurfaceId("context-menu"),
            SurfaceContent.ContextMenu(ContextMenu("editor", Focus.EditorPane(paneId), items)),
            SurfacePresentation.Floating(Some(CursorPosition(2, 10)), SurfacePlacement.BelowCursor)
          )
        )
      )

  private def render(frames: FrameBackend, state: AppState, scale: Int, rows: Int): (BufferedImage, CellMetrics) =
    val fontConfig = state.persisted.config.editorConfig.fontConfig
    val codeFont   = FontLoader.previewCodeFont(fontConfig)
    val uiFont     = FontLoader.previewUiFont(fontConfig)
    val metrics    = CellMetrics.fromFont(codeFont)
    val widthPx    = columns * metrics.charWidth
    val heightPx   = rows * metrics.lineHeight
    val frame      = frames.newFrame(widthPx, heightPx)
    val caches     = RenderCaches.create()
    val surface    = frames.surfaceOver(frame, metrics, codeFont, widthPx, heightPx, 1.0, persists = false, caches)
    RendererEntryPoints.render(
      CompositionPreview.displayed(state, revision),
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
    (frame.toImage, metrics)

  private def zoomedStrip(image: BufferedImage, metrics: CellMetrics, state: AppState, rows: Int, scale: Int) =
    val strip = LayoutEngine.calculateLayout(state, ViewportSize(columns, rows)).tabBarRect.getOrElse(fail("no strip"))
    val crop  = image.getSubimage(0, strip.y * metrics.lineHeight, image.getWidth, strip.height * metrics.lineHeight)
    val zoom  = 3 / scale + 1
    val out   = new BufferedImage(crop.getWidth * zoom, crop.getHeight * zoom, BufferedImage.TYPE_INT_RGB)
    val g     = out.createGraphics()
    g.drawImage(crop, 0, 0, out.getWidth, out.getHeight, null)
    g.dispose()
    out

  private val directory = Paths.get("target", "test-reports", "media-signoff")

  private def write(name: String, image: BufferedImage): Unit =
    Files.createDirectories(directory)
    val _ = ImageIO.write(image, "png", directory.resolve(s"$name.png").toFile)

  private val windowNames = List("README.md", "notes.txt")

  "The real renderer" should "write the sign-off sheets on both backends" in {
    val backends = List("java2d" -> Java2DFrames, "skia" -> SkiaFrames)
    val themes   = List("light" -> Theme.light, "dark" -> Theme.dark)
    for
      (backendName, frames) <- backends
      (themeName, theme)    <- themes
      scale                 <- List(1, 2)
    do
      val floating = AppConfig.conventional.withFontConfig(
        AppConfig.conventional.editorConfig.fontConfig.copy(fontSize = 14.0f * scale, uiFontSize = 12.0f * scale)
      )
      val flush = floating.withDockedCornerStyle(DockedCornerStyle.Flush)
      def tag(scene: String) = s"$scene-$themeName-$backendName-scale$scale"

      val windows = List(
        "window-floating" -> stateWith(floating, theme, windowNames, 0, 26, panels = true, menu = false),
        "window-flush"    -> stateWith(flush, theme, windowNames, 0, 26, panels = true, menu = false),
        "context-menu"    -> stateWith(floating, theme, windowNames, 0, 26, panels = false, menu = true)
      )
      windows.foreach((scene, state) => write(tag(scene), render(frames, state, scale, 26)._1))

      val strips = List(
        "strip-two-long" -> (List("a-very-long-document-name-for-the-quarterly-planning-review.md", "notes.txt"), 0),
        "strip-ten"      -> (List.tabulate(10)(i => s"chapter-${i + 1}-draft-notes.md"), 4),
        "strip-24"       -> (List.tabulate(24)(i => s"file-$i.scala"), 11)
      )
      strips.foreach {
        case (scene, (names, active)) =>
          val state          = stateWith(floating, theme, names, active, 12, panels = false, menu = false)
          val (image, metrics) = render(frames, state, scale, 12)
          write(tag(scene), zoomedStrip(image, metrics, state, 12, scale))
      }
    Files.list(directory).count() shouldBe 48L
  }
