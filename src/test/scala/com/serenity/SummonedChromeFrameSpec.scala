package com.serenity

import java.awt.Font
import java.awt.image.BufferedImage

import com.serenity.config.{AppConfig, ChromeElement}
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.layout.{
  CellMetrics,
  Layout,
  LayoutEngine,
  ViewportSize,
  WorkspaceNode,
  WorkspaceNodeId,
  WorkspaceTree
}
import com.serenity.ui.renderer.{FontSpec, Java2DRenderSurface, RendererEntryPoints}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SummonedChromeFrameSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(60, 12)
  private val metrics  = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 12)
  private val font     = Font(Font.MONOSPACED, Font.PLAIN, 12)

  private def paint(state: AppState, caches: RenderCaches): BufferedImage =
    val width  = viewport.width * metrics.charWidth
    val height = viewport.height * metrics.lineHeight
    val image  = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val surface =
      new Java2DRenderSurface(image, metrics, font, _ => (), width, height, 1.0, 1.0, textCaches = caches.surfaceText)
    RendererEntryPoints.render(
      state,
      cursorVisible = true,
      surface,
      viewport,
      FontSpec.fromAwt(font),
      FontSpec.fromAwt(font),
      FontSpec.fromAwt(font),
      metrics,
      metrics,
      cursorColor = None,
      caches
    )
    image

  "A frame rendered after only the summons changed" should "paint the line-number gutter the held chord asks for" in {
    val buffer = Buffer.fromString(BufferId(1), "alpha\nbeta\ngamma")
    val paneId = PaneId(0)
    val base   = AppState.initial(AppConfig.default)
    val hidden = base.copy(persisted =
      base.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), paneId)))
        ),
        focus = Focus.EditorPane(paneId)
      )
    )
    val held =
      hidden.copy(runtime = hidden.runtime.copy(summons = hidden.runtime.summons.pressed(ChromeElement.LineNumbers)))
    val caches = RenderCaches.create()

    paint(hidden, caches)
    val gutter = LayoutEngine.calculateLayoutWithUI(held, viewport).lineNumberRect.getOrElse(fail("no gutter reserved"))
    val before = paint(hidden, caches)
    val after  = paint(held, caches)

    val inked = for
      column <- gutter.x * metrics.charWidth until (gutter.x + gutter.width) * metrics.charWidth
      row    <- gutter.y * metrics.lineHeight until (gutter.y + gutter.height) * metrics.lineHeight
    yield after.getRGB(column, row) != before.getRGB(column, row)
    inked.exists(identity) shouldBe true
  }
