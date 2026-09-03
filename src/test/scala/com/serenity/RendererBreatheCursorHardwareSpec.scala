package com.serenity

import com.serenity.config.CursorMode
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.{HardwareCursor, HardwareCursorStyle}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1170 left breathe mode as the documented app-painted exception to the hardware cursor -- correct for a GUI canvas,
  * which really does composite the alpha-modulated caret as content. A cell-addressed terminal surface has no such
  * content path (`TerminalRenderSurface.fillPixelRect` is necessarily a no-op, see #1012), so that exception left the
  * caret invisible there for the whole of breathe mode. These pin the TUI-specific fallback: thresholding the
  * already-computed breathe alpha into hardware-cursor show/hide, so the terminal's own cursor stands in for the
  * app-painted glyph GUI surfaces use instead.
  */
class RendererBreatheCursorHardwareSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val paneId      = PaneId(0)
  private val bufferId    = BufferId(1)
  private val viewport    = ViewportSize(80, 24)
  private val codeFont    = java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
  private val cellMetrics = com.serenity.ui.layout.CellMetrics.fromFont(codeFont)

  final private class FakeHardwareCursor extends HardwareCursor:
    private val presentCallsBuffer = scala.collection.mutable.ListBuffer.empty[(Int, Int, HardwareCursorStyle)]
    private val hideCallsCounter   = new java.util.concurrent.atomic.AtomicInteger(0)

    def present(cellX: Int, cellY: Int, style: HardwareCursorStyle): Unit =
      presentCallsBuffer += ((cellX, cellY, style))

    def hide(): Unit =
      val _ = hideCallsCounter.incrementAndGet()

    def presentCallCount: Int = presentCallsBuffer.size
    def hideCallCount: Int    = hideCallsCounter.get()

  private def breatheState(mode: CursorMode = CursorMode.Breathe): AppState =
    val buffer =
      Buffer.fromString(bufferId, "hello world").copy(editing = EditingState(cursors = List(CursorPosition(0, 3))))
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          paneOrder = List(paneId)
        ),
        focus = Focus.EditorPane(paneId),
        theme = Theme.light,
        config = AppState.initial.persisted.config.withCursorMode(mode)
      )
    )

  private def renderWith(hardwareCursor: HardwareCursor, cursorColor: Option[java.awt.Color]): Unit =
    val surface = new MockRenderSurface(80, 24, persistentContent = true, hardwareCursorOverride = Some(hardwareCursor))
    val _ = com.serenity.ui.renderer.Renderer.renderCursorOnly(
      breatheState(),
      cursorVisible = true,
      surface,
      viewport,
      codeFont,
      codeFont,
      codeFont,
      cellMetrics,
      cellMetrics,
      cursorColor
    )

  "Breathe mode on a surface with a hardware cursor" should "show the terminal cursor while the breathe alpha is at or above half brightness" in {
    val hardwareCursor = new FakeHardwareCursor
    renderWith(hardwareCursor, Some(new java.awt.Color(255, 255, 255, 200)))

    hardwareCursor.presentCallCount shouldBe 1
    hardwareCursor.hideCallCount shouldBe 0
  }

  it should "hide the terminal cursor while the breathe alpha is below half brightness" in {
    val hardwareCursor = new FakeHardwareCursor
    renderWith(hardwareCursor, Some(new java.awt.Color(255, 255, 255, 60)))

    hardwareCursor.presentCallCount shouldBe 0
    hardwareCursor.hideCallCount shouldBe 1
  }

  it should "hide the terminal cursor when no breathe color has been computed yet" in {
    val hardwareCursor = new FakeHardwareCursor
    renderWith(hardwareCursor, None)

    hardwareCursor.presentCallCount shouldBe 0
    hardwareCursor.hideCallCount shouldBe 1
  }
