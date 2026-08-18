package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.command.{CommandCategory, CommandRunner}
import com.serenity.config.InterfaceDensity
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerMouseSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  "Command runner mouse interaction" should "highlight the command row under the pointer" in {
    val stateManager = createStateManager("CommandRunnerMouseSpec")

    stateManager.applyEvent(ResizeEvent(ViewportSize(100, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val before = stateManager.getCurrentState.unsafeRunSync()
    val point  = commandRunnerItemPoint(before, 2)

    stateManager.applyEvent(MouseMove(point.x, point.y)).unsafeRunSync()

    runnerFrom(stateManager.getCurrentState.unsafeRunSync()).selectedIndex shouldBe 2
  }

  it should "ignore mouse movement over a configured blank command-item gap" in {
    val stateManager = createStateManager("CommandRunnerMouseSpec")

    stateManager.applyEvent(ResizeEvent(ViewportSize(100, 30))).unsafeRunSync()
    stateManager
      .updateState(state =>
        state.copy(
          config = state.config
            .withInterfaceDensity(InterfaceDensity.Compact)
            .withCommandRunnerItemGapRows(1)
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val before         = stateManager.getCurrentState.unsafeRunSync()
    val itemPoint      = commandRunnerItemPoint(before, 0)
    val selectedBefore = runnerFrom(before).selectedIndex

    stateManager.applyEvent(MouseMove(itemPoint.x, itemPoint.y + 1)).unsafeRunSync()

    runnerFrom(stateManager.getCurrentState.unsafeRunSync()).selectedIndex shouldBe selectedBefore
  }

  it should "execute the row at its fractional floating pixel offset" in {
    val stateManager = createStateManager("CommandRunnerMouseSpec")

    stateManager.applyEvent(ResizeEvent(ViewportSize(100, 30))).unsafeRunSync()
    stateManager
      .updateState(state => state.copy(config = state.config.withCommandRunnerCursorGapRows(Some(0.5))))
      .unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "toggle-line".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val before = stateManager.getCurrentState.unsafeRunSync()
    before.config.showLineNumbers shouldBe true
    val point = shiftedCommandRunnerItemPoint(before, 0)

    stateManager
      .applyEvent(MouseClick(point.x, point.y, pixelX = Some(point.pixelX), pixelY = Some(point.pixelY)))
      .unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.config.showLineNumbers shouldBe false
    after.commandRunnerSurface shouldBe None
  }

  it should "ignore a pixel click in a fractional shifted command-item gap" in {
    val stateManager = createStateManager("CommandRunnerMouseSpec")

    stateManager.applyEvent(ResizeEvent(ViewportSize(100, 30))).unsafeRunSync()
    stateManager
      .updateState(state =>
        state.copy(
          config = state.config
            .withCommandRunnerCursorGapRows(Some(0.5))
            .withCommandRunnerItemGapRows(0.5)
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()

    val before         = stateManager.getCurrentState.unsafeRunSync()
    val point          = shiftedCommandRunnerItemGapPoint(before, 0)
    val selectedBefore = runnerFrom(before).selectedIndex

    stateManager
      .applyEvent(MouseClick(point.x, point.y, pixelX = Some(point.pixelX), pixelY = Some(point.pixelY)))
      .unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    runnerFrom(after).selectedIndex shouldBe selectedBefore
    after.commandRunnerSurface shouldBe defined
  }

  it should "execute the command row clicked under the pointer" in {
    val stateManager = createStateManager("CommandRunnerMouseSpec")

    stateManager.applyEvent(ResizeEvent(ViewportSize(100, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "toggle-line".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val before = stateManager.getCurrentState.unsafeRunSync()
    before.config.showLineNumbers shouldBe true
    val point = commandRunnerItemPoint(before, 0)

    stateManager.applyEvent(MouseClick(point.x, point.y)).unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.config.showLineNumbers shouldBe false
    after.commandRunnerSurface shouldBe None
  }

  it should "switch categories when a category tab is clicked without submitting a command" in {
    val stateManager = createStateManager("CommandRunnerMouseSpec")

    stateManager.applyEvent(ResizeEvent(ViewportSize(100, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val before = stateManager.getCurrentState.unsafeRunSync()
    val point  = commandRunnerCategoryPoint(before, categoryIndex = 1)

    stateManager.applyEvent(MouseMove(point.x, point.y)).unsafeRunSync()
    runnerFrom(stateManager.getCurrentState.unsafeRunSync()).activeCategory shouldBe CommandCategory.File

    stateManager.applyEvent(MouseClick(point.x, point.y)).unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.commandRunnerSurface shouldBe defined
    runnerFrom(after).activeCategory shouldBe CommandCategory.File
  }

  it should "highlight the focused submenu row under the pointer" in {
    val stateManager = createStateManager("CommandRunnerMouseSpec")

    openLanguageSubmenu(stateManager)

    val before = stateManager.getCurrentState.unsafeRunSync()
    val point  = commandRunnerSubmenuItemPoint(before, 2)

    stateManager.applyEvent(MouseMove(point.x, point.y)).unsafeRunSync()

    runnerFrom(stateManager.getCurrentState.unsafeRunSync()).activeSubmenu.map(_.selectedIndex) shouldBe Some(2)
  }

  it should "keep submenu focus when hovering over the parent command runner" in {
    val stateManager = createStateManager("CommandRunnerMouseSpec")

    openLanguageSubmenu(stateManager)

    val before = stateManager.getCurrentState.unsafeRunSync()
    before.focus shouldBe Focus.Surface(before.commandRunnerSubmenuSurface.get.id)

    val point = commandRunnerItemPoint(before, 0)

    stateManager.applyEvent(MouseMove(point.x, point.y)).unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.focus shouldBe Focus.Surface(before.commandRunnerSubmenuSurface.get.id)
    runnerFrom(after).activeSubmenu.map(_.groupId) shouldBe Some("settings-language")
  }

  it should "focus a preview submenu row when the pointer moves into the preview" in {
    val stateManager = createStateManager("CommandRunnerMouseSpec")

    stateManager.applyEvent(ResizeEvent(ViewportSize(100, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    (1 to 5).foreach(_ => stateManager.applyEvent(TabKey).unsafeRunSync())

    val before = stateManager.getCurrentState.unsafeRunSync()
    before.commandRunnerSubmenuSurface.map(_.content) should matchPattern {
      case Some(SurfaceContent.CommandPaletteSubmenu(_, "settings-workspace-layout", true)) =>
    }

    val point = commandRunnerSubmenuItemPoint(before, 0)

    stateManager.applyEvent(MouseMove(point.x, point.y)).unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.focus shouldBe Focus.Surface(before.commandRunnerSubmenuSurface.get.id)
    runnerFrom(after).activeSubmenu.map(_.groupId) shouldBe Some("settings-workspace-layout")
    runnerFrom(after).activeSubmenu.map(_.selectedIndex) shouldBe Some(0)
  }

  it should "enter the preview submenu row clicked under the pointer" in {
    val stateManager = createStateManager("CommandRunnerMouseSpec")

    stateManager.applyEvent(ResizeEvent(ViewportSize(100, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    (1 to 5).foreach(_ => stateManager.applyEvent(TabKey).unsafeRunSync())

    val before = stateManager.getCurrentState.unsafeRunSync()
    val point  = commandRunnerSubmenuItemPoint(before, 0)

    stateManager.applyEvent(MouseClick(point.x, point.y)).unsafeRunSync()

    val after   = stateManager.getCurrentState.unsafeRunSync()
    val submenu = runnerFrom(after).activeSubmenu.getOrElse(fail("Expected focused submenu"))
    after.focus shouldBe Focus.Surface(before.commandRunnerSubmenuSurface.get.id)
    submenu.groupId shouldBe "settings-panel-pins"
    submenu.parentGroupId shouldBe Some("settings-workspace-layout")
  }

  it should "execute the focused submenu row clicked under the pointer" in {
    val stateManager = createStateManager("CommandRunnerMouseSpec")

    stateManager
      .updateState { state =>
        val bufferId = state.layout.activeEditorPaneId
          .flatMap(state.layout.editorPanes.get)
          .flatMap(_.bufferId)
          .getOrElse(fail("Expected focused buffer"))
        state.copy(buffers =
          state.buffers.updated(bufferId, state.buffers(bufferId).copy(language = Some(LanguageId.Scala)))
        )
      }
      .unsafeRunSync()
    openLanguageSubmenu(stateManager)

    val before = stateManager.getCurrentState.unsafeRunSync()
    val point  = commandRunnerSubmenuItemPoint(before, 0)

    stateManager.applyEvent(MouseClick(point.x, point.y)).unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = after.layout.activeEditorPaneId
      .flatMap(after.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .getOrElse(fail("Expected focused buffer"))
    after.buffers(bufferId).language shouldBe None
    after.commandRunnerSurface shouldBe None
  }

  final private case class Point(x: Int, y: Int, pixelX: Int = 0, pixelY: Int = 0)

  private def commandRunnerItemPoint(state: AppState, displayedItemRow: Int): Point =
    val surface = state.commandRunnerSurface.getOrElse(fail("Expected command runner surface"))
    overlayItemPoint(state, surface.id, displayedItemRow)

  private def commandRunnerSubmenuItemPoint(state: AppState, displayedItemRow: Int): Point =
    val surface = state.commandRunnerSubmenuSurface.getOrElse(fail("Expected command runner submenu surface"))
    overlayItemPoint(state, surface.id, displayedItemRow)

  private def commandRunnerCategoryPoint(state: AppState, categoryIndex: Int): Point =
    val surface  = state.commandRunnerSurface.getOrElse(fail("Expected command runner surface"))
    val viewport = state.viewportSize.getOrElse(fail("Expected viewport size"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val contract = EditorLayoutContract.from(state, viewport, layout)
    val contentRect = contract
      .overlayContentRect(surface.id)
      .getOrElse(fail("Expected command runner content rect"))
    val header = contract
      .overlayHeaderRect(surface.id)
      .getOrElse(fail("Expected command runner header rect"))
    Point(contentRect.x + (contentRect.width * categoryIndex) / CommandCategory.values.length + 1, header.y)

  private def overlayItemPoint(state: AppState, surfaceId: SurfaceId, displayedItemRow: Int): Point =
    val viewport = state.viewportSize.getOrElse(fail("Expected viewport size"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val contract = EditorLayoutContract.from(state, viewport, layout)
    val contentRect = contract
      .overlayContentRect(surfaceId)
      .getOrElse(fail(s"Expected overlay content rect for ${surfaceId.value}"))
    val rowY = contract
      .overlayRowSlots(surfaceId)
      .collectFirst { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(`displayedItemRow`), y) => y }
      .getOrElse(fail(s"Expected overlay item row $displayedItemRow for ${surfaceId.value}"))
    Point(x = contentRect.x + 1, y = rowY)

  private def shiftedCommandRunnerItemPoint(state: AppState, displayedItemRow: Int): Point =
    val geometry = shiftedCommandRunnerGeometry(state)
    val rect     = geometry.itemRects.lift(displayedItemRow).getOrElse(fail(s"Expected item $displayedItemRow"))
    Point(
      x = math.floor(rect.x / floatingMetrics(state).charWidth).toInt,
      y = math.floor(rect.y / floatingMetrics(state).lineHeight).toInt,
      pixelX = math.round(rect.x + 1.0).toInt,
      pixelY = math.round(rect.y + rect.height / 2.0).toInt
    )

  private def shiftedCommandRunnerItemGapPoint(state: AppState, displayedItemRow: Int): Point =
    val geometry = shiftedCommandRunnerGeometry(state)
    val current  = geometry.itemRects.lift(displayedItemRow).getOrElse(fail(s"Expected item $displayedItemRow"))
    val next   = geometry.itemRects.lift(displayedItemRow + 1).getOrElse(fail(s"Expected item ${displayedItemRow + 1}"))
    val pixelY = math.round((current.y + current.height + next.y) / 2.0).toInt
    Point(
      x = math.floor(current.x / floatingMetrics(state).charWidth).toInt,
      y = math.floor(pixelY.toDouble / floatingMetrics(state).lineHeight).toInt,
      pixelX = math.round(current.x + 1.0).toInt,
      pixelY = pixelY
    )

  private def shiftedCommandRunnerGeometry(state: AppState): FloatingSurfaceGeometry =
    val surface  = state.commandRunnerSurface.getOrElse(fail("Expected command runner surface"))
    val viewport = state.viewportSize.getOrElse(fail("Expected viewport size"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val contract = EditorLayoutContract.from(state, viewport, layout)
    val contentRect = contract
      .overlayContentRect(surface.id)
      .getOrElse(fail("Expected command runner content rect"))
    val runner  = runnerFrom(state)
    val metrics = floatingMetrics(state)
    FloatingSurfaceGeometry
      .fromCells(
        contentRect,
        metrics,
        borderCells = 0,
        itemCount = runner.visibleItems.length,
        hasHeader = true,
        hasFooter = runner.visibleItems.nonEmpty || runner.statusMessage.nonEmpty,
        itemGapRows = state.config.commandRunnerItemGapRows,
        itemTargetRows = SurfaceFrameLayout.minimumTargetRows(state.config.interfaceDensity)
      )
      .translated(
        0.0,
        FloatingSurfaceGeometry.signedRowOffsetPixels(layout.floatingOverlayOffsetRows(surface.id), metrics)
      )

  private def floatingMetrics(state: AppState): CellMetrics =
    CellMetrics.fromFont(FontLoader.previewCodeFont(state.config.fontConfig))

  private def openLanguageSubmenu(stateManager: com.serenity.state.manager.StateManager): Unit =
    stateManager.applyEvent(ResizeEvent(ViewportSize(100, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "language".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

  private def runnerFrom(state: AppState): CommandRunner =
    state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner)
          case _                                     => None
      }
      .getOrElse(fail("Expected command runner"))
