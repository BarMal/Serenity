package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.{Dimension, Font, GraphicsEnvironment}
import java.nio.file.{Files, Paths}
import javax.imageio.ImageIO
import javax.swing.JPanel

import scala.jdk.CollectionConverters.*

import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.state.reducers.{CompositionPreview, CompositionRevisions}
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389: the open context menu through the real GUI renderer onto a pixel surface, as a PNG per scene and scale
  * under `target/test-reports/context-menu-screenshots/`, with the menu's measured size in `layout.txt`. Scale 2 is the
  * same layout drawn with a font twice the size, so every pixel dimension doubles; it is not a HiDPI surface. This is
  * proof scaffolding for the PR: the assertions only say that something was drawn and laid out.
  */
class ContextMenuScreenshotSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  final private case class Scene(
      name: String,
      labels: List[String],
      columns: Int,
      fallbackFamilies: List[String] = Nil
  )

  private val rows     = 14
  private val bufferId = BufferId(1)
  private val paneId   = PaneId(0)
  private val baseSize = 18.0f

  private val cjkFamilies = List("Noto Sans CJK JP", "Noto Sans CJK SC", "IPAGothic", "WenQuanYi Zen Hei")

  private val longLabels =
    List("Open the file under the cursor in a new split", "Copy the selection as rich text", "Paste without formatting")

  private val scenes = List(
    Scene("1-short-labels", List("Save", "Cut", "Copy", "Paste"), 60),
    Scene("2-long-labels", longLabels, 60),
    Scene("3-cjk-labels", List("保存文件", "剪切", "复制", "粘贴"), 60, cjkFamilies),
    Scene("4-narrow-pane", longLabels, 28)
  )

  private val revisions = new CompositionRevisions

  private val baseFont =
    Font.createFont(
      Font.TRUETYPE_FONT,
      Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile
    )

  private def menuOf(scene: Scene): ContextMenu =
    ContextMenu(
      title = "editor",
      targetFocus = Focus.EditorPane(paneId),
      items = scene.labels.map { label =>
        val command = Command.typed(label, label, CommandIntent.Edit(EditIntent.Copy), label = label)
        ContextMenuItem(command.name, label, command)
      }
    )

  private def stateFor(scene: Scene, viewport: ViewportSize): AppState =
    val buffer = Buffer
      .fromString(bufferId, "The quick brown fox jumps over the lazy dog.")
      .copy(
        editing = EditingState(List(CursorPosition(0, 10))),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = rows, visibleColumns = viewport.width)
      )
    val config = AppConfig.conventional
    val withFonts =
      if scene.fallbackFamilies.isEmpty then config
      else config.withFontConfig(config.editorConfig.fontConfig.copy(fallbackFamilies = scene.fallbackFamilies))
    val base = AppState.initial(withFonts)
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
    state.copy(runtime =
      state.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("context-menu"),
            SurfaceContent.ContextMenu(menuOf(scene)),
            SurfacePresentation.Floating(Some(CursorPosition(0, 10)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  private def drewSomething(image: BufferedImage): Boolean =
    val background = image.getRGB(0, 0)
    (0 until image.getHeight).exists(y => (0 until image.getWidth).exists(x => image.getRGB(x, y) != background))

  "The open context menu through the real renderer" should "draw every scene at scale 1 and 2 and record its size" in {
    val directory = Paths.get("target", "test-reports", "context-menu-screenshots")
    Files.createDirectories(directory)
    val installed = GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames.toSet
    val facts =
      for
        scene <- scenes
        scale <- List(1, 2)
      yield
        val codeFont = baseFont.deriveFont(Font.PLAIN, baseSize * scale)
        val metrics  = CellMetrics.fromFont(codeFont)
        val widthPx  = scene.columns * metrics.charWidth
        val heightPx = rows * metrics.lineHeight
        val viewport = ViewportSize(scene.columns, rows)
        val state    = stateFor(scene, viewport)

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

        val layout      = LayoutEngine.calculateLayout(state, viewport)
        val rect        = layout.belowCursorOverlayRect.getOrElse(fail(s"${scene.name}: no menu was laid out"))
        val paneRect    = LayoutEngine.calculatePaneLayouts(state, layout)(paneId)
        val contentRect = CursorLayout.contentRectForPane(paneRect)

        drewSomething(image) shouldBe true
        ImageIO.write(image, "png", directory.resolve(s"${scene.name}-scale$scale.png").toFile) shouldBe true
        s"${scene.name} scale $scale: pane content ${contentRect.width}x${contentRect.height} cells, " +
          s"menu ${rect.width}x${rect.height} cells at (${rect.x},${rect.y}), image ${widthPx}x${heightPx}px"

    val cjkFound = cjkFamilies.filter(installed.contains)
    val report   = facts ++ List(s"CJK families installed on this machine: ${cjkFound.mkString(", ")}")
    Files.write(directory.resolve("layout.txt"), report.asJava)
    facts should have size (scenes.size * 2)
  }
