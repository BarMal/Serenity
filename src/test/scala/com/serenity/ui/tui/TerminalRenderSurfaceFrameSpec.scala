package com.serenity.ui.tui

import java.awt.{Color, Font}
import java.io.StringWriter

import com.serenity.config.AppConfig
import com.serenity.state.models.UiSurface
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, LayoutRect, OverlayRow}
import com.serenity.ui.renderer.{PinnedPanelRenderer, TextOverlayRenderer, TextOverlayView, TextPanelRow, TextPanelView}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The rounded box-drawing frame `TerminalRenderSurface.strokeRect` draws in the border cell `SurfaceFrameLayout`
  * reserves around every framed floating surface and pinned panel.
  */
class TerminalRenderSurfaceFrameSpec extends AnyFlatSpec with Matchers:

  private val cellMetrics = CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0)
  private val font        = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val theme       = Theme.light

  private def surface(width: Int, height: Int): (TerminalRenderSurface, StringWriter) =
    val writer = new StringWriter()
    (new TerminalRenderSurface(width, height, writer, cellMetrics), writer)

  private def screenOf(rs: TerminalRenderSurface, writer: StringWriter, width: Int, height: Int): TerminalEmulator =
    rs.flush()
    TerminalEmulator.blank(width, height).consume(writer.toString)

  private def sameRgb(cell: Color, expected: RenderColor): Boolean =
    (cell.getRGB & 0xffffff) == (expected.argb & 0xffffff)

  private def borderCells(rect: LayoutRect): List[(Int, Int)] =
    val horizontal = (rect.x until rect.right).toList.flatMap(x => List((x, rect.y), (x, rect.bottom - 1)))
    val vertical   = (rect.y + 1 until rect.bottom - 1).toList.flatMap(y => List((rect.x, y), (rect.right - 1, y)))
    horizontal ++ vertical

  "strokeRect" should "draw a rounded box-drawing frame on the rect's outermost cells" in {
    val (rs, writer) = surface(6, 5)
    rs.panelOutlines.get.strokeRect(1, 1, 4, 3, RenderColor.fromAwt(Color.RED), 1f)

    val screen = screenOf(rs, writer, 6, 5)
    screen.rowText(0) shouldBe "      "
    screen.rowText(1) shouldBe " ╭──╮ "
    screen.rowText(2) shouldBe " │  │ "
    screen.rowText(3) shouldBe " ╰──╯ "
    screen.rowText(4) shouldBe "      "
    screen.cellAt(1, 1).fg shouldBe Color.RED
    screen.cellAt(4, 2).fg shouldBe Color.RED
  }

  it should "keep each frame cell's background and restore the colours it found" in {
    val (rs, writer) = surface(4, 3)
    rs.setBackgroundColor(RenderColor.fromAwt(Color.BLUE))
    rs.fillRect(0, 0, 4, 3, ' ')
    rs.setBackgroundColor(RenderColor.fromAwt(Color.GREEN))
    rs.fillRect(0, 1, 1, 1, ' ')
    rs.setForegroundColor(RenderColor.fromAwt(Color.YELLOW))
    rs.setBackgroundColor(RenderColor.fromAwt(Color.MAGENTA))

    rs.panelOutlines.get.strokeRect(0, 0, 4, 3, RenderColor.fromAwt(Color.RED), 1f)
    rs.putString(1, 1, "x")

    val screen = screenOf(rs, writer, 4, 3)
    screen.cellAt(0, 0).bg shouldBe Color.BLUE
    screen.cellAt(0, 1).bg shouldBe Color.GREEN
    screen.cellAt(0, 1).text shouldBe "│"
    screen.cellAt(1, 1).fg shouldBe Color.YELLOW
    screen.cellAt(1, 1).bg shouldBe Color.MAGENTA
  }

  it should "draw nothing for a rect narrower or shorter than two cells" in {
    val (rs, writer) = surface(4, 4)
    rs.flush()
    writer.getBuffer.setLength(0)

    rs.panelOutlines.get.strokeRect(0, 0, 1, 4, RenderColor.fromAwt(Color.RED), 1f)
    rs.panelOutlines.get.strokeRect(0, 0, 4, 1, RenderColor.fromAwt(Color.RED), 1f)
    rs.flush()

    writer.toString shouldBe ""
  }

  it should "respect an active clip" in {
    val (rs, writer) = surface(6, 3)
    rs.panelOutlines.get.withRectClip(0, 0, 3, 3) {
      rs.panelOutlines.get.strokeRect(0, 0, 6, 3, RenderColor.fromAwt(Color.RED), 1f)
    }

    val screen = screenOf(rs, writer, 6, 3)
    screen.rowText(0) shouldBe "╭──   "
    screen.rowText(1) shouldBe "│     "
    screen.rowText(2) shouldBe "╰──   "
  }

  "a floating overlay in the TUI" should "be framed in the theme's border colour without touching its content" in {
    val (rs, writer) = surface(20, 8)
    val rect         = LayoutRect(2, 1, 12, 5)
    val overlay      = TextOverlayView(rect = rect, rows = List(OverlayRow("alpha"), OverlayRow("beta")))

    TextOverlayRenderer.render(rs, overlay, theme, AppConfig.default, cursorVisible = false, font, cellMetrics)

    val screen = screenOf(rs, writer, 20, 8)
    screen.rowText(rect.y).substring(rect.x, rect.right) shouldBe "╭──────────╮"
    screen.rowText(rect.bottom - 1).substring(rect.x, rect.right) shouldBe "╰──────────╯"
    (rect.y + 1 until rect.bottom - 1).foreach { y =>
      screen.cellAt(rect.x, y).text shouldBe "│"
      screen.cellAt(rect.right - 1, y).text shouldBe "│"
    }
    borderCells(rect).foreach { (x, y) =>
      withClue(s"cell ($x, $y): ")(sameRgb(screen.cellAt(x, y).fg, theme.border) shouldBe true)
      withClue(s"cell ($x, $y): ")(screen.cellAt(x, y).bg shouldBe screen.cellAt(rect.x + 1, y).bg)
    }
    screen.rowText(rect.y + 1).substring(rect.x + 1, rect.right - 1) should startWith("alpha")
    screen.rowText(rect.y + 2).substring(rect.x + 1, rect.right - 1) should startWith("beta")
  }

  "the status line in the TUI" should "not be framed: it reserves no border for one" in {
    val (rs, writer) = surface(20, 4)
    val rect         = LayoutRect(0, 1, 20, 2)
    val overlay = TextOverlayView(
      rect = rect,
      borderCells = 0,
      rows = List(OverlayRow("NORMAL main.scala"), OverlayRow("second line")),
      surfaceId = Some(UiSurface.StatusLineSurfaceId)
    )

    TextOverlayRenderer.render(rs, overlay, theme, AppConfig.default, cursorVisible = false, font, cellMetrics)

    val screen = screenOf(rs, writer, 20, 4)
    screen.rowText(1) should startWith("NORMAL main.scala")
    screen.rowText(2) should startWith("second line")
    screen.rows.mkString should not include "╭"
    screen.rows.mkString should not include "│"
  }

  "the tab bar in the TUI" should "not be framed: it paints edge to edge" in {
    val (rs, writer) = surface(20, 3)
    val rect         = LayoutRect(0, 0, 20, 2)
    val overlay = TextOverlayView(
      rect = rect,
      rows = List(OverlayRow("tab")),
      surfaceId = Some(UiSurface.TabBarSurfaceId)
    )

    TextOverlayRenderer.render(rs, overlay, theme, AppConfig.default, cursorVisible = false, font, cellMetrics)

    val screen = screenOf(rs, writer, 20, 3)
    screen.rows.mkString should not include "╭"
    screen.rows.mkString should not include "╰"
  }

  "a pinned panel in the TUI" should "be framed, with its title on the top edge and its content untouched" in {
    val (rs, writer) = surface(20, 8)
    val rect         = LayoutRect(1, 1, 14, 5)
    val panel        = TextPanelView(rect = rect, title = "Outline", rows = List(TextPanelRow("main")))

    PinnedPanelRenderer.render(rs, panel, theme, AppConfig.default, cellMetrics)

    val screen = screenOf(rs, writer, 20, 8)
    screen.rowText(rect.y).substring(rect.x, rect.right) shouldBe "╭Outline─────╮"
    screen.rowText(rect.bottom - 1).substring(rect.x, rect.right) shouldBe "╰────────────╯"
    (rect.y + 1 until rect.bottom - 1).foreach { y =>
      screen.cellAt(rect.x, y).text shouldBe "│"
      screen.cellAt(rect.right - 1, y).text shouldBe "│"
    }
    sameRgb(screen.cellAt(rect.x, rect.y).fg, theme.border) shouldBe true
    screen.rowText(rect.y + 1).substring(rect.x + 1, rect.right - 1) should startWith("main")
  }
