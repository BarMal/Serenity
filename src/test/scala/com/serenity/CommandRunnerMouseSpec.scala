package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.command.CommandRunner
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.*
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

  private case class Point(x: Int, y: Int)

  private def commandRunnerItemPoint(state: AppState, displayedItemRow: Int): Point =
    val surface     = state.commandRunnerSurface.getOrElse(fail("Expected command runner surface"))
    val rect        = commandRunnerRect(state)
    val contentRect = SurfaceFrameLayout.forContent(rect, surface.content).contentRect
    Point(
      x = contentRect.x + 1,
      y = contentRect.y + overlayHeaderRows(surface.content) + displayedItemRow
    )

  private def commandRunnerSubmenuItemPoint(state: AppState, displayedItemRow: Int): Point =
    val surface     = state.commandRunnerSubmenuSurface.getOrElse(fail("Expected command runner submenu surface"))
    val rect        = commandRunnerSubmenuRect(state)
    val contentRect = SurfaceFrameLayout.forContent(rect, surface.content).contentRect
    Point(
      x = contentRect.x + 1,
      y = contentRect.y + overlayHeaderRows(surface.content) + displayedItemRow
    )

  private def overlayHeaderRows(content: SurfaceContent): Int =
    content match
      case SurfaceContent.CommandPalette(_) => 1
      case SurfaceContent.CommandPaletteSubmenu(runner, groupId, _) =>
        if runner.submenuGroup(groupId).nonEmpty then 1 else 0
      case _ =>
        0

  private def openLanguageSubmenu(stateManager: com.serenity.state.manager.StateManager): Unit =
    stateManager.applyEvent(ResizeEvent(ViewportSize(100, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "language".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

  private def commandRunnerRect(state: AppState): LayoutRect =
    val viewport = state.viewportSize.getOrElse(fail("Expected viewport size"))
    val surface  = state.commandRunnerSurface.getOrElse(fail("Expected command runner surface"))
    LayoutEngine
      .calculateLayoutWithUI(state, viewport)
      .belowCursorOverlayStack
      .collectFirst { case (`surface`.id, rect) => rect }
      .getOrElse(fail("Expected command runner overlay rect"))

  private def commandRunnerSubmenuRect(state: AppState): LayoutRect =
    val viewport = state.viewportSize.getOrElse(fail("Expected viewport size"))
    val surface  = state.commandRunnerSubmenuSurface.getOrElse(fail("Expected command runner submenu surface"))
    LayoutEngine
      .calculateLayoutWithUI(state, viewport)
      .belowCursorOverlayStack
      .collectFirst { case (`surface`.id, rect) => rect }
      .getOrElse(fail("Expected command runner submenu overlay rect"))

  private def runnerFrom(state: AppState): CommandRunner =
    state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner)
          case _                                     => None
      }
      .getOrElse(fail("Expected command runner"))
