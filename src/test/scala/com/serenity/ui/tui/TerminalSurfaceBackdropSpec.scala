package com.serenity.ui.tui

import java.awt.{Color, Font}
import java.io.StringWriter

import com.serenity.animation.{AnimatedCell, AnimationState, CharacterKey, EasingCurve, Tween}
import com.serenity.command.{Command, CommandIntent, CommandRegistry, CommandRunner, FileIntent}
import com.serenity.config.{AppConfig, MaterialPreset, StatusLineColors}
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.{
  AppState,
  Buffer,
  BufferId,
  CursorPosition,
  EditorPane,
  Focus,
  PaneId,
  SurfaceContent,
  SurfaceId,
  SurfacePlacement,
  SurfacePresentation,
  UiSurface
}
import com.serenity.ui.layout.{CellMetrics, Layout, LayoutRect, OverlayRow, OverlaySegment, OverlayTone, ViewportSize}
import com.serenity.ui.renderer.{
  PinnedPanelRenderer,
  RendererEntryPoints,
  TextOverlayRenderer,
  TextOverlayView,
  TextPanelRow,
  TextPanelView
}
import com.serenity.ui.theme.{DefaultThemes, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A terminal cannot paint a translucent panel, so under a theme whose background is the terminal's own (alpha 0, SGR
  * 49) a floating surface or pinned panel shows that same backdrop through its translucent material instead of a solid
  * block -- while its frame, text, selection highlight and tones keep their own colours.
  */
class TerminalSurfaceBackdropSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val cellMetrics = CellMetrics.cellUnit
  private val font        = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val transparent = DefaultThemes.transparent
  private val backdrop    = TerminalEmulator.TransparentBackground

  private def surface(width: Int, height: Int): (TerminalRenderSurface, StringWriter) =
    val writer = new StringWriter()
    (new TerminalRenderSurface(width, height, writer, cellMetrics), writer)

  private def screenOf(rs: TerminalRenderSurface, writer: StringWriter, width: Int, height: Int): TerminalEmulator =
    rs.flush()
    TerminalEmulator.blank(width, height).consume(writer.toString)

  private def sameRgb(a: Color, b: Color): Boolean = (a.getRGB & 0xffffff) == (b.getRGB & 0xffffff)

  private def isBackdrop(color: Color): Boolean = color == backdrop

  private def renderOverlay(
    overlay: TextOverlayView,
    theme: Theme,
    config: AppConfig = AppConfig.default
  ): TerminalEmulator =
    val (rs, writer) = surface(20, 9)
    TextOverlayRenderer.render(rs, overlay, theme, config, cursorVisible = false, font, cellMetrics)
    screenOf(rs, writer, 20, 9)

  private def rowCells(rect: LayoutRect, y: Int): List[(Int, Int)] = (rect.x until rect.right).toList.map(x => (x, y))

  private val rect = LayoutRect(2, 1, 14, 6)

  "a floating overlay in the TUI under a transparent theme" should
    "sit on the terminal's own background, frame and unselected rows included" in {
      val overlay = TextOverlayView(rect = rect, rows = List(OverlayRow("alpha", selected = true), OverlayRow("beta")))

      val screen = renderOverlay(overlay, transparent)

      val selectedRow   = rect.y + 1
      val unselected    = (rect.y until rect.bottom).filterNot(_ == selectedRow).toList.flatMap(rowCells(rect, _))
      val selectedFrame = List((rect.x, selectedRow), (rect.right - 1, selectedRow))
      (unselected ++ selectedFrame).foreach { (x, y) =>
        withClue(s"cell ($x, $y) '${screen.cellAt(x, y).text}':\n${screen.render}\n")(
          isBackdrop(screen.cellAt(x, y).bg) shouldBe true
        )
      }
      screen.rowText(rect.y + 2).substring(rect.x + 1, rect.right - 1) should startWith("beta")
      screen.cellAt(rect.x, rect.y).text shouldBe "╭"
    }

  it should "keep the selected row's highlight across its whole content width" in {
    val overlay = TextOverlayView(rect = rect, rows = List(OverlayRow("alpha", selected = true), OverlayRow("beta")))

    val screen = renderOverlay(overlay, transparent)

    (rect.x + 1 until rect.right - 1).foreach { x =>
      withClue(s"cell ($x, ${rect.y + 1}): ")(
        sameRgb(screen.cellAt(x, rect.y + 1).bg, transparent.highlighted.background) shouldBe true
      )
    }
    screen.rowText(rect.y + 1).substring(rect.x + 1, rect.right - 1) should startWith("alpha")
  }

  it should "keep an Error tone's own background" in {
    val overlay = TextOverlayView(
      rect = rect,
      rows = List(OverlayRow("oops", segments = List(OverlaySegment("oops", tone = OverlayTone.Error))))
    )

    val screen = renderOverlay(overlay, transparent)

    val (col, row) = screen.find("oops").getOrElse(fail(screen.render))
    sameRgb(screen.cellAt(col, row).bg, transparent.error.background) shouldBe true
    isBackdrop(screen.cellAt(rect.right - 2, row).bg) shouldBe true
  }

  it should "show the backdrop once its fade-in has settled on the panel colour" in {
    val panelBg = transparent.panel.background
    val settled = AnimationState(
      (0 until rect.height).map { row =>
        CharacterKey(0, row) -> AnimatedCell(
          content = None,
          backgroundAnimation = Some(Tween(panelBg, panelBg, EasingCurve.Linear, steps = 1))
        )
      }.toMap
    )
    val overlay = TextOverlayView(rect = rect, animationState = settled, rows = List(OverlayRow("beta")))

    val screen = renderOverlay(overlay, transparent)

    (rect.y until rect.bottom).flatMap(rowCells(rect, _)).foreach { (x, y) =>
      withClue(s"cell ($x, $y): ")(isBackdrop(screen.cellAt(x, y).bg) shouldBe true)
    }
  }

  it should "paint no glass sheen stripe, which would be a tint of a panel colour the terminal is not showing" in {
    val overlay = TextOverlayView(rect = rect, rows = List(OverlayRow("beta")))

    val screen = renderOverlay(overlay, transparent, AppConfig.default.withMaterialPreset(MaterialPreset.Crystal))

    (rect.x until rect.right).foreach { x =>
      withClue(s"cell ($x, ${rect.y + 1}): ")(isBackdrop(screen.cellAt(x, rect.y + 1).bg) shouldBe true)
    }
  }

  it should "keep the panel colour under a Solid material, which asks for an opaque panel" in {
    val overlay = TextOverlayView(rect = rect, rows = List(OverlayRow("beta")))

    val screen = renderOverlay(overlay, transparent, AppConfig.default.withMaterialPreset(MaterialPreset.Solid))

    sameRgb(screen.cellAt(rect.right - 2, rect.y + 1).bg, transparent.panel.background) shouldBe true
    sameRgb(screen.cellAt(rect.x, rect.y).bg, transparent.panel.background) shouldBe true
  }

  "a status line in the TUI under a transparent theme" should "keep a background it was explicitly configured with" in {
    val statusRect = LayoutRect(0, 1, 20, 1)
    val overlay = TextOverlayView(
      rect = statusRect,
      borderCells = 0,
      rows = List(OverlayRow("NORMAL")),
      surfaceId = Some(UiSurface.StatusLineSurfaceId)
    )
    val config = AppConfig.default.withStatusLineColors(StatusLineColors(background = Some(Color.BLUE)))

    val screen = renderOverlay(overlay, transparent, config)

    sameRgb(screen.cellAt(15, 1).bg, Color.BLUE) shouldBe true
  }

  "a floating overlay in the TUI under an opaque theme" should "keep painting the theme's panel colour" in {
    val theme   = Theme.dark
    val overlay = TextOverlayView(rect = rect, rows = List(OverlayRow("beta")))

    val screen = renderOverlay(overlay, theme)

    sameRgb(screen.cellAt(rect.right - 2, rect.y + 1).bg, theme.panel.background) shouldBe true
    sameRgb(screen.cellAt(rect.x, rect.y).bg, theme.panel.background) shouldBe true
  }

  "a pinned panel in the TUI under a transparent theme" should
    "sit on the terminal's own background while its selected row keeps the highlight" in {
      val (rs, writer) = surface(20, 9)
      val panelRect    = LayoutRect(1, 1, 14, 5)
      val panel = TextPanelView(
        rect = panelRect,
        title = "Outline",
        rows = List(TextPanelRow("main", selected = true), TextPanelRow("other"))
      )

      PinnedPanelRenderer.render(rs, panel, transparent, AppConfig.default, cellMetrics)

      val screen                     = screenOf(rs, writer, 20, 9)
      val (selectedCol, selectedRow) = screen.find("main").getOrElse(fail(screen.render))
      sameRgb(screen.cellAt(selectedCol, selectedRow).bg, transparent.highlighted.background) shouldBe true
      (panelRect.y until panelRect.bottom)
        .filterNot(_ == selectedRow)
        .flatMap(rowCells(panelRect, _))
        .foreach { (x, y) =>
          withClue(s"cell ($x, $y) '${screen.cellAt(x, y).text}':\n${screen.render}\n")(
            isBackdrop(screen.cellAt(x, y).bg) shouldBe true
          )
        }
    }

  "the command runner rendered in the TUI under a transparent theme" should
    "leave no solid panel-coloured block around its frame" in {
      val viewport     = ViewportSize(60, 20)
      val (rs, writer) = surface(viewport.width, viewport.height)

      RendererEntryPoints.render(
        commandRunnerState,
        cursorVisible = false,
        rs,
        viewport,
        font,
        font,
        cellMetrics,
        None,
        RenderCaches.create()
      )

      val screen        = screenOf(rs, writer, viewport.width, viewport.height)
      val (top, bottom) = framedRows(screen)
      val left          = screen.rowText(top).indexOf("╭")
      val right         = screen.rowText(top).indexOf("╮")
      val frameRect     = LayoutRect(left, top, right - left + 1, bottom - top + 1)
      val panelCells = (frameRect.y until frameRect.bottom).flatMap(rowCells(frameRect, _)).filter { (x, y) =>
        sameRgb(screen.cellAt(x, y).bg, transparent.panel.background)
      }
      withClue(s"panel-coloured cells $panelCells:\n${screen.render}\n")(panelCells shouldBe empty)
      List((frameRect.x, frameRect.y), (frameRect.right - 1, frameRect.bottom - 1)).foreach { (x, y) =>
        isBackdrop(screen.cellAt(x, y).bg) shouldBe true
      }
    }

  private def framedRows(screen: TerminalEmulator): (Int, Int) =
    val top    = screen.rowsContaining("╭").headOption.getOrElse(fail(s"no command runner frame:\n${screen.render}"))
    val bottom = screen.rowsContaining("╰").find(_ > top).getOrElse(fail(s"no frame bottom:\n${screen.render}"))
    (top, bottom)

  private def commandRunnerState: AppState =
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val registry = CommandRegistry(
      List(
        Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)),
        Command.typed("close", "Close current file", CommandIntent.File(FileIntent.CloseCurrentFile))
      )
    )
    val runner  = CommandRunner.empty.activate(registry, AppConfig.default)
    val initial = AppState.initial
    initial.copy(
      persisted = initial.persisted.copy(
        buffers = Map(bufferId -> Buffer.fromString(bufferId, "alpha\nbeta\ngamma")),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(com.serenity.TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.Surface(SurfaceId("command-runner")),
        theme = transparent
      ),
      runtime = initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

end TerminalSurfaceBackdropSpec
