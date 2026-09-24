package com.serenity.ui.tui

import com.serenity.state.models.{AppState, BufferId, CloseWorkflowState, Modal, SurfaceContent}
import com.serenity.ui.layout.{LayoutEngine, TabBarSurfaceComposition}

import TuiScenarios.*

/** #1673 in a terminal: the tab strip's close (x) glyph is clicked where it is painted on the cell grid, and runs the
  * same close workflow a GUI click does.
  */
class TuiTabCloseClickSpec extends TuiSpec:

  /** The cell the close glyph of `bufferId`'s tab is painted on, checked against the painted screen. */
  private def closeGlyphCell(bufferId: BufferId): TuiScript[(Int, Int)] =
    for
      current <- state
      size    <- TuiScript(_.viewportSize)
      shown   <- screen
    yield
      val (entries, activeBufferId) = current.tabBarSurface.map(_.content) match
        case Some(SurfaceContent.TabBar(entries, activeBufferId)) => (entries, activeBufferId)
        case other                                                => fail(s"Expected a tab bar, got $other")
      val rect =
        LayoutEngine.calculateLayoutWithUI(current, size).tabBarRect.getOrElse(fail("Expected a tab bar rect"))
      val region = TabBarSurfaceComposition
        .closeAffordances(entries, activeBufferId, rect)
        .find(region => TabBarSurfaceComposition.closeBufferIdOf(region.focusId).contains(bufferId))
        .getOrElse(fail(s"Expected a close affordance for $bufferId"))
      val col = region.rect.x.toInt + region.rect.width.toInt - 1
      val row = region.rect.y.toInt
      shown.textAt(col, row, 1) shouldBe "x"
      (col, row)

  private def clickClose(bufferId: BufferId): TuiScript[Unit] =
    closeGlyphCell(bufferId).flatMap((col, row) => click(col, row))

  private def activeBufferId(current: AppState): Option[BufferId] = current.activeBuffer.map(_.id)

  private def closeWorkflow(current: AppState): Option[CloseWorkflowState] =
    current.topModal.flatMap {
      _.modal match
        case Modal.CloseWorkflow(workflow) => Some(workflow)
        case _                             => None
    }

  /** The file tab and a new tab after it, with the file tab active again; returns (file, new tab). */
  private val fileTabActiveWithNewTabBehind: TuiScript[(BufferId, BufferId)] =
    for
      initial <- state
      first = activeBufferId(initial).getOrElse(fail("Expected an active buffer"))
      _       <- newTab
      created <- state
      second = activeBufferId(created).getOrElse(fail("Expected the new tab to be active"))
      _ <- previousTab
      _ <- verifyState("file tab active again")(current => activeBufferId(current) shouldBe Some(first))
    yield (first, second)

  "clicking a clean background tab's close glyph" should "close it and leave the active tab active" in
    runTui(TuiEnvironment.withFile("keep me")) {
      for
        tabs <- fileTabActiveWithNewTabBehind
        (first, second) = tabs
        _ <- clickClose(second)
        _ <- verifyState("background tab closed") { current =>
          current.topModal shouldBe None
          current.persisted.buffers should not contain key(second)
          activeBufferId(current) shouldBe Some(first)
        }
      yield ()
    }

  "clicking a dirty background tab's close glyph" should "raise the save prompt, and keep the tab when cancelled" in
    runTui(TuiEnvironment.withFile("keep me")) {
      for
        tabs <- fileTabActiveWithNewTabBehind
        (first, second) = tabs
        _ <- nextTab
        _ <- typeText("throwaway")
        _ <- previousTab
        _ <- clickClose(second)
        _ <- verifyState("save prompt")(current => closeWorkflow(current).map(_.currentBufferId) shouldBe Some(second))
        prompt <- screen
        cancelRow = prompt.rowOf("Cancel").getOrElse(fail("expected a Cancel choice"))
        cancelCol = prompt.rowText(cancelRow).indexOf("Cancel")
        _ <- click(cancelCol + 1, cancelRow)
        _ <- verifyState("tab kept") { current =>
          current.topModal shouldBe None
          current.persisted.buffers should contain key second
          current.persisted.buffers(second).document.isDirty shouldBe true
          activeBufferId(current) shouldBe Some(first)
        }
      yield ()
    }
end TuiTabCloseClickSpec
