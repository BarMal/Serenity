package com.serenity.ui.tui

import java.awt.Font
import java.io.StringWriter

import com.serenity.TestWorkspaceTrees
import com.serenity.config.AppConfig
import com.serenity.frontend.FrontendCapabilities
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.{AppState, Buffer, BufferId, EditorPane, Focus, PaneId}
import com.serenity.ui.layout.{CellMetrics, Layout, LayoutEngine, LayoutRect, ViewportSize}
import com.serenity.ui.renderer.{FontSpec, RendererEntryPoints}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The terminal scrollbar as the terminal shows it: the pane's last column, drawn in thin glyphs, with the text wrapped
  * one column short of it.
  */
class TerminalScrollbarRenderSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val font     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val viewport = ViewportSize(60, 16)
  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val glyphs   = "│┃╿╽"

  private def stateOf(text: String): AppState =
    val initial = AppState.initial(AppConfig.conventional)
    initial.copy(
      runtime = initial.runtime.copy(capabilities = FrontendCapabilities.tui(), viewportSize = Some(viewport)),
      persisted = initial.persisted.copy(
        buffers = Map(bufferId -> Buffer.fromString(bufferId, text)),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId)
      )
    )

  private def render(state: AppState): TerminalEmulator =
    val writer  = new StringWriter()
    val surface = new TerminalRenderSurface(viewport.width, viewport.height, writer, CellMetrics.cellUnit)
    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      FontSpec.fromAwt(font),
      FontSpec.fromAwt(font),
      CellMetrics.cellUnit,
      None,
      RenderCaches.create()
    )
    surface.flush()
    TerminalEmulator.blank(viewport.width, viewport.height).consume(writer.toString)

  private def column(state: AppState): LayoutRect =
    val synced = LayoutEngine.syncViewportDimensions(state, viewport)
    LayoutEngine
      .calculateEditorPaneLayouts(synced, LayoutEngine.calculateLayout(synced, viewport))(paneId)
      .scrollbarRect

  private def cells(screen: TerminalEmulator, rect: LayoutRect): String =
    (rect.y until rect.bottom).map(row => screen.rowText(row).lift(rect.x).getOrElse(' ')).mkString

  private val longDocument = (0 until 200).map(line => s"line $line").mkString("\n")

  "A long document in a terminal" should "show its thumb in the pane's last column, in thin glyphs" in {
    val state  = stateOf(longDocument)
    val screen = render(state)
    val shown  = cells(screen, column(state))

    shown.forall(glyphs.contains(_)) shouldBe true
    shown.exists("┃╿╽".contains(_)) shouldBe true
    shown.exists(_ == '│') shouldBe true
    screen.find("line 0") should not be empty
  }

  it should "put the thumb at the top while the view is at the top of the document" in {
    val state = stateOf(longDocument)
    val shown = cells(render(state), column(state))

    shown.head should (be('┃') or be('╿'))
    shown.last shouldBe '│'
  }

  "A long document with the scrollbar turned off" should "leave the last column to the text" in {
    val config =
      AppConfig.conventional.copy(surfaceConfig = AppConfig.conventional.surfaceConfig.copy(scrollbarEnabled = false))
    val base   = stateOf(longDocument)
    val state  = base.copy(persisted = base.persisted.copy(config = config))
    val screen = render(state)

    column(state).width shouldBe 0
    (0 until viewport.height).flatMap(row => screen.rowText(row)).exists(glyphs.contains(_)) shouldBe false
    screen.find("line 0") should not be empty
  }

  "A document that fits the pane" should "leave the column blank" in {
    val state = stateOf("one\ntwo\nthree")

    cells(render(state), column(state)).forall(_ == ' ') shouldBe true
  }
