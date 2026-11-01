package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.{Dimension, Font}
import java.nio.file.{Files, Paths}
import javax.imageio.ImageIO
import javax.swing.JPanel

import com.serenity.command.*
import com.serenity.config.{AppConfig, DockedCornerStyle}
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.state.reducers.{CompositionPreview, CompositionRevisions}
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389 / #2454, throwaway: real renders for the owner's sign-off. The conventional layout at its default
  * settings (floating docked chrome, 12 px corners, rounded pane header) and with flush chrome for comparison, and the
  * context menu at the new one-row pitch, at scale 1 and 2. NOT for merge.
  */
class RealRenderSheetSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val columns  = 100
  private val rows     = 26
  private val paneId   = PaneId(0)
  private val revision = new CompositionRevisions

  private val baseFont =
    Font.createFont(
      Font.TRUETYPE_FONT,
      Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile
    )

  private val text = "The quick brown fox jumps over the lazy dog.\n" * 30

  private def stateFor(config: AppConfig, withPanels: Boolean, menu: Boolean): AppState =
    val first  = Buffer.fromString(BufferId(1), text).copy(editing = EditingState(List(CursorPosition(2, 10))))
    val second = Buffer.fromString(BufferId(2), "Notes\n")
    val base   = AppState.initial(config)
    val state = base.copy(
      persisted = base.persisted.copy(
        buffers = Map(first.id -> first, second.id -> second),
        bufferOrder = List(first.id, second.id),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, first.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(com.serenity.TestWorkspaceTrees.linear(paneId))
        ),
        theme = Theme.light,
        focus = Focus.EditorPane(paneId)
      ),
      runtime = base.runtime.copy(viewportSize = Some(ViewportSize(columns, rows)))
    )
    val docked =
      if withPanels then
        List(
          SessionDockedPanel("outline", SessionPinnedPanel(PanelPosition.Left, 22, SessionPanelContent.Outline(Nil))),
          SessionDockedPanel(
            "diagnostics",
            SessionPinnedPanel(PanelPosition.Right, 22, SessionPanelContent.Diagnostics(Nil))
          )
        )
      else Nil
    val withDocks = UiPreset.applyToState(
      UiPreset("Sheet", config, Some(Theme.light.name), dockedPanels = docked),
      state,
      Some(Theme.light)
    )
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
    image

  private val directory = Paths.get("target", "test-reports", "media-2389-final")

  private def write(name: String, image: BufferedImage): Unit =
    Files.createDirectories(directory)
    ImageIO.write(image, "png", directory.resolve(s"$name.png").toFile)

  private def uiFontScaled(config: AppConfig, scale: Int): AppConfig =
    config.withFontConfig(config.editorConfig.fontConfig.copy(uiFontSize = 12.0f * scale))

  "The real renderer" should "write the sign-off sheets" in {
    List(1, 2).foreach { scale =>
      val floating = uiFontScaled(AppConfig.conventional, scale)
      val flush    = floating.withDockedCornerStyle(DockedCornerStyle.Flush)
      write(s"conventional-default-scale$scale", render(stateFor(floating, withPanels = true, menu = false), scale))
      write(s"conventional-flush-scale$scale", render(stateFor(flush, withPanels = true, menu = false), scale))
      write(s"context-menu-scale$scale", render(stateFor(floating, withPanels = false, menu = true), scale))
    }
    Files.list(directory).count() shouldBe 6L
  }
