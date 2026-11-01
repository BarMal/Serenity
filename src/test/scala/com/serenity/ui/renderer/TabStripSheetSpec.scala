package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.{Dimension, Font}
import java.nio.file.{Files, Path, Paths}
import javax.imageio.ImageIO
import javax.swing.JPanel

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.state.reducers.{CompositionPreview, CompositionRevisions}
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389, throwaway: the content-sized tab strip with 2 tabs and with many, at scale 1 and 2. NOT for merge. */
class TabStripSheetSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val columns  = 100
  private val rows     = 12
  private val paneId   = PaneId(0)
  private val revision = new CompositionRevisions

  private val baseFont =
    Font.createFont(
      Font.TRUETYPE_FONT,
      Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile
    )

  private def stateFor(config: AppConfig, names: List[String], active: Int): AppState =
    val buffers = names.zipWithIndex.map((name, i) => Buffer.fromFile(BufferId(i + 1), Path.of("/tmp", name), "text\n"))
    val base    = AppState.initial(config)
    base.copy(
      persisted = base.persisted.copy(
        buffers = buffers.map(b => b.id -> b).toMap,
        bufferOrder = buffers.map(_.id),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffers(active).id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(com.serenity.TestWorkspaceTrees.linear(paneId))
        ),
        theme = Theme.light,
        focus = Focus.EditorPane(paneId)
      ),
      runtime = base.runtime.copy(viewportSize = Some(ViewportSize(columns, rows)))
    )

  private def render(state: AppState, scale: Int): BufferedImage =
    val codeFont = baseFont.deriveFont(Font.PLAIN, 14.0f * scale)
    val metrics  = CellMetrics.fromFont(codeFont)
    val widthPx  = columns * metrics.charWidth
    val heightPx = rows * metrics.lineHeight
    val image    = new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_RGB)
    val panel    = new JPanel
    panel.setPreferredSize(new Dimension(widthPx, heightPx))
    panel.setSize(new Dimension(widthPx, heightPx))
    val surface = Java2DRenderSurface.forImage(image, metrics, codeFont, panel, _ => (), SurfaceTextCaches.fresh())
    RendererEntryPoints.render(
      CompositionPreview.displayed(state, revision),
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
    val strip = LayoutEngine.calculateLayout(state, ViewportSize(columns, rows)).tabBarRect.getOrElse(fail("no strip"))
    val top   = strip.y * metrics.lineHeight
    val high  = strip.height * metrics.lineHeight
    val crop  = image.getSubimage(0, top, widthPx, high)
    val zoom  = 3 / scale + 1
    val out   = new BufferedImage(widthPx * zoom, high * zoom, BufferedImage.TYPE_INT_RGB)
    val g     = out.createGraphics()
    g.drawImage(crop, 0, 0, widthPx * zoom, high * zoom, null)
    g.dispose()
    out

  private val directory = Paths.get("target", "test-reports", "media-tabstrips")

  "The tab strip" should "write sheets with two tabs and with many" in {
    Files.createDirectories(directory)
    val scenes = List(
      "two-tabs"       -> (List("README.md", "notes.txt"), 1),
      "two-long"       -> (List("a-very-long-document-name-for-the-quarterly-planning-review.md", "notes.txt"), 0),
      "ten-tabs"       -> (List.tabulate(10)(i => s"chapter-${i + 1}-draft-notes.md"), 4),
      "twentyfour-tabs" -> (List.tabulate(24)(i => s"file-$i.scala"), 11)
    )
    List(1, 2).foreach { scale =>
      scenes.foreach {
        case (name, (names, active)) =>
          val config = AppConfig.conventional.withFontConfig(
            AppConfig.conventional.editorConfig.fontConfig.copy(uiFontSize = 12.0f * scale)
          )
          ImageIO.write(
            render(stateFor(config, names, active), scale),
            "png",
            directory.resolve(s"tabstrip-$name-scale$scale.png").toFile
          )
      }
    }
    Files.list(directory).count() shouldBe 8L
  }
