package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.command.{CommandRunner, CommandSurfaceItem}
import com.serenity.config.InterfaceDensity
import com.serenity.keystroke.events.*
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
    // The persistent key-hint footer (issue #931, Stage 3) defaults on and reserves a row, shifting which absolute
    // item rows fit in the window -- disabled here since this test is about row hit-testing mechanics, not the
    // footer feature, and every row math below assumes the pre-Stage-3 window size.
    disableCommandRunnerKeyHints(stateManager)

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
        state.copy(persisted =
          state.persisted.copy(config =
            state.persisted.config
              .withInterfaceDensity(InterfaceDensity.Compact)
              .withCommandRunnerItemGapRows(1)
          )
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
      .updateState(state =>
        state.copy(persisted =
          state.persisted.copy(config = state.persisted.config.withCommandRunnerCursorGapRows(Some(0.5)))
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "toggle-line".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val before = stateManager.getCurrentState.unsafeRunSync()
    before.persisted.config.surfaceConfig.showLineNumbers shouldBe true
    val point = shiftedCommandRunnerItemPoint(before, 0)

    stateManager
      .applyEvent(MouseClick(point.x, point.y, pixelX = Some(point.pixelX), pixelY = Some(point.pixelY)))
      .unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.persisted.config.surfaceConfig.showLineNumbers shouldBe false
    after.commandRunnerSurface shouldBe None
  }

  it should "ignore a pixel click in a fractional shifted command-item gap" in {
    val stateManager = createStateManager("CommandRunnerMouseSpec")

    stateManager.applyEvent(ResizeEvent(ViewportSize(100, 30))).unsafeRunSync()
    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted.copy(config =
            state.persisted.config
              .withCommandRunnerCursorGapRows(Some(0.5))
              .withCommandRunnerItemGapRows(0.5)
          )
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
    before.persisted.config.surfaceConfig.showLineNumbers shouldBe true
    val point = commandRunnerItemPoint(before, 0)

    stateManager.applyEvent(MouseClick(point.x, point.y)).unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.persisted.config.surfaceConfig.showLineNumbers shouldBe false
    after.commandRunnerSurface shouldBe None
  }

  // issue #931: category tabs are retired -- there is no tab row left to click, so this test (and the mouse
  // hit-testing it exercised, `commandPaletteCategoryAt`) is removed rather than adapted.

  // issue #1059: a drilled-in settings group renders on the one command-runner surface now (no second floating
  // submenu surface), so this hit-tests against that surface directly.
  // issue #1057: previously entered the (now-removed) "settings-language" group via a "language" search; retargeted
  // to "settings-cursor" (still present, 3 plain OptionItem children -- unlike "settings-ui-font", none of them is
  // itself an expandable group, so there's no inline group-preview row competing for space in the capped floating
  // surface height, issue #1045).
  it should "highlight the focused submenu row under the pointer" in {
    val stateManager = createStateManager("CommandRunnerMouseSpec")

    openCursorSubmenu(stateManager)
    // See the equivalent comment on "highlight the command row under the pointer" above.
    disableCommandRunnerKeyHints(stateManager)

    val before = stateManager.getCurrentState.unsafeRunSync()
    val point  = commandRunnerItemPoint(before, 2)

    stateManager.applyEvent(MouseMove(point.x, point.y)).unsafeRunSync()

    runnerFrom(stateManager.getCurrentState.unsafeRunSync()).settingsSurfaceSelectedIndex shouldBe 2
  }

  // issue #1059: hovering a not-yet-entered group's preview rows, and clicking within them to select or enter a
  // child without a real click on the group itself, were both second-floating-surface interactions
  // (RunnerSelectPreviewSubmenuItem, dispatched against a previewOnly CommandPaletteSubmenu surface). The capped
  // group preview that replaces that surface is inline, static, and explicitly never independently
  // selectable/focusable (SurfaceContentResolver's groupPreviewRows), so there is no mouse interaction left to test
  // here -- entering a group is only ever a direct click/submit on the group's own row, covered by "execute the
  // focused submenu row clicked under the pointer" below and the palette's ordinary row-click tests above.

  // issue #1057: previously entered the (now-removed) "settings-language" group and clicked "Plain Text" to observe
  // the buffer's language change. Retargeted to "settings-ui-font"'s nested font-family group -- still a real
  // settings-tree submenu of CommandItems, and a click on one still executes it and closes the runner the same way.
  // The specific family clicked isn't hardcoded (font availability is environment-dependent); the assertion is that
  // clicking row 0 sets the UI font to whichever family renders there.
  it should "execute the focused submenu row clicked under the pointer" in {
    val stateManager = createStateManager("CommandRunnerMouseSpec")

    openUiFontFamilySubmenu(stateManager)

    val before = stateManager.getCurrentState.unsafeRunSync()
    val expectedFamily = runnerFrom(before).focusedSubmenuItems
      .lift(0)
      .collect { case CommandSurfaceItem.CommandItem(command) => command.label }
      .getOrElse(fail("Expected at least one UI font family"))
    val point = commandRunnerItemPoint(before, 0)

    stateManager.applyEvent(MouseClick(point.x, point.y)).unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.persisted.config.editorConfig.fontConfig.uiFontFamily shouldBe expectedFamily
    after.commandRunnerSurface shouldBe None
  }

  final private case class Point(x: Int, y: Int, pixelX: Int = 0, pixelY: Int = 0)

  private def commandRunnerItemPoint(state: AppState, displayedItemRow: Int): Point =
    val surface = state.commandRunnerSurface.getOrElse(fail("Expected command runner surface"))
    overlayItemPoint(state, surface.id, displayedItemRow)

  private def overlayItemPoint(state: AppState, surfaceId: SurfaceId, displayedItemRow: Int): Point =
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
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
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
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
        itemGapRows = state.persisted.config.surfaceConfig.commandRunnerItemGapRows,
        itemTargetRows = SurfaceFrameLayout.minimumTargetRows(state.persisted.config.interfaceDensity)
      )
      .translated(
        0.0,
        FloatingSurfaceGeometry.signedRowOffsetPixels(layout.floatingOverlayOffsetRows(surface.id), metrics)
      )

  private def floatingMetrics(state: AppState): CellMetrics =
    CellMetrics.fromFont(FontLoader.previewCodeFont(state.persisted.config.editorConfig.fontConfig))

  /** Enters "settings-cursor" (search "cursor" exact-matches its label; issue #1057 -- was "settings-language"). */
  private def disableCommandRunnerKeyHints(stateManager: com.serenity.state.manager.StateManager): Unit =
    stateManager
      .updateState(state =>
        state
          .copy(persisted = state.persisted.copy(config = state.persisted.config.withCommandRunnerShowKeyHints(false)))
      )
      .unsafeRunSync()

  private def openCursorSubmenu(stateManager: com.serenity.state.manager.StateManager): Unit =
    stateManager.applyEvent(ResizeEvent(ViewportSize(100, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "cursor".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

  /** Enters "settings-ui-font" (search "ui font" exact-matches its label), then descends one level further into its
    * nested font-family group (selected by default -- it's that group's first child).
    */
  private def openUiFontFamilySubmenu(stateManager: com.serenity.state.manager.StateManager): Unit =
    stateManager.applyEvent(ResizeEvent(ViewportSize(100, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "ui font".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

  private def runnerFrom(state: AppState): CommandRunner =
    state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner)
          case _                                     => None
      }
      .getOrElse(fail("Expected command runner"))
