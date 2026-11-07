package com.serenity.ui.tui

import com.serenity.state.models.{AppState, BufferId, CloseWorkflowState, SurfaceContent}
import com.serenity.ui.layout.{LayoutEngine, TabBarSurfaceComposition}
import com.serenity.ui.renderer.TabBarLayoutSettings

import TuiScenarios.*

/** #1673 in a terminal: the tab strip's close (x) glyph is clicked where it is painted on the cell grid, and runs the
  * same close workflow a GUI click does. Only the active tab shows an x in a terminal, which has no pointer hover to
  * reveal another's.
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
        .closeAffordances(entries, activeBufferId, rect, TabBarLayoutSettings.of(current))
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
    com.serenity.ClosePromptFixtures.closePromptShown(current)

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

  "a terminal's inactive tab" should "paint no close glyph and offer no close region" in
    runTui(TuiEnvironment.withFile("keep me")) {
      for
        tabs    <- fileTabActiveWithNewTabBehind
        current <- state
        size    <- TuiScript(_.viewportSize)
        shown   <- screen
      yield
        val (_, second) = tabs
        val (entries, activeBufferId) = current.tabBarSurface.map(_.content) match
          case Some(SurfaceContent.TabBar(entries, activeBufferId)) => (entries, activeBufferId)
          case other                                                => fail(s"Expected a tab bar, got $other")
        val rect =
          LayoutEngine.calculateLayoutWithUI(current, size).tabBarRect.getOrElse(fail("Expected a tab bar rect"))
        val regions =
          TabBarSurfaceComposition.closeAffordances(entries, activeBufferId, rect, TabBarLayoutSettings.of(current))
        regions.flatMap(region => TabBarSurfaceComposition.closeBufferIdOf(region.focusId)) shouldBe
          activeBufferId.toList
        shown.rowText(rect.y.toInt).count(_ == 'x') shouldBe 1
        second should not be activeBufferId.getOrElse(second)
    }

  "clicking a clean tab's close glyph once it is active" should "close it" in
    runTui(TuiEnvironment.withFile("keep me")) {
      for
        tabs <- fileTabActiveWithNewTabBehind
        (_, second) = tabs
        _ <- nextTab
        _ <- clickClose(second)
        _ <- verifyState("tab closed") { current =>
          current.topModal shouldBe None
          current.persisted.buffers should not contain key(second)
        }
      yield ()
    }

  "clicking a dirty tab's close glyph once it is active" should "raise the save prompt, and keep the tab when cancelled" in
    runTui(TuiEnvironment.withFile("keep me")) {
      for
        tabs <- fileTabActiveWithNewTabBehind
        (_, second) = tabs
        _ <- nextTab
        _ <- typeText("throwaway")
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
        }
      yield ()
    }
end TuiTabCloseClickSpec
