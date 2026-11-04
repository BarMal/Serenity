package com.serenity

import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutEngine, LayoutRect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2570 item 3 in the windowed editor: a surface hung off a caret the view has scrolled away from is laid out on the
  * pane's edge nearest the caret, not dropped from the layout.
  */
class GuiOffscreenCursorSurfaceLayoutSpec extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  private def withSurface(state: AppState, placement: SurfacePlacement): AppState =
    val surface =
      UiSurface(SurfaceId("hover"), SurfaceContent.QuickInfo("hover text"), SurfacePresentation.Floating(None, placement))
    state.copy(runtime = state.runtime.copy(uiSurfaces = List(surface), viewportSize = Some(viewport)))

  private def laidOut(state: AppState): (List[LayoutRect], LayoutRect) =
    val layout = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val pane   = LayoutEngine.laidOutPanes(state, layout).layouts(paneId).contentRect
    ((layout.belowCursorOverlayStack ++ layout.aboveCursorOverlayStack).map(_._2), pane)

  private def inside(rect: LayoutRect, pane: LayoutRect): Boolean =
    rect.y >= pane.y && rect.bottom <= pane.bottom && rect.x >= pane.x && rect.right <= pane.right

  private val text = codeLines.mkString("\n")

  "A below-cursor surface with the caret above the view" should "be laid out at the pane's top, inside it" in {
    val (rects, pane) = laidOut(withSurface(stateOf(text, CursorPosition(0, 0), topLine = 60), SurfacePlacement.BelowCursor))

    rects should have length 1
    rects.filterNot(inside(_, pane)) shouldBe Nil
    rects.head.y should be <= pane.y + 2
  }

  "An above-cursor surface with the caret below the view" should "be laid out at the pane's bottom, inside it" in {
    val (rects, pane) = laidOut(withSurface(stateOf(text, CursorPosition(110, 0)), SurfacePlacement.AboveCursor))

    rects should have length 1
    rects.filterNot(inside(_, pane)) shouldBe Nil
    rects.head.bottom should be >= pane.bottom - 2
  }
