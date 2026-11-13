package com.serenity.ui.layout

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{FallbackHeaderReducer, NoticeReducer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The fallback header is an overlay on the top row: it covers what is there and moves nothing (#2660 item 1). */
class FallbackHeaderLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val message = "Skia could not load: no raster surface. Running in the terminal."

  private def stateFor(viewport: ViewportSize): AppState =
    val base = AppState.initial
    base.copy(runtime = base.runtime.copy(viewportSize = Some(viewport)))

  private def headerId(state: AppState): SurfaceId =
    state.runtime.uiSurfaces
      .collectFirst { case UiSurface(id, SurfaceContent.FallbackHeader(_), _, _) => id }
      .getOrElse(fail("expected a header"))

  "The fallback header" should "span the top row of the viewport, whatever its size" in {
    for viewport <- List(ViewportSize(80, 24), ViewportSize(120, 40), ViewportSize(20, 10), ViewportSize(7, 3)) do
      val state  = FallbackHeaderReducer.shown(stateFor(viewport), message)
      val layout = LayoutEngine.calculateLayout(state, viewport)

      withClue(s"$viewport: ")(
        layout.cornerOverlayStack.find(_._1 == headerId(state)).map(_._2) shouldBe
          Some(LayoutRect(0, 0, viewport.width, 1))
      )
  }

  it should "move nothing else: the chrome and every other overlay lay out as they did" in {
    val viewport = ViewportSize(80, 24)
    val before   = stateFor(viewport)
    val after    = FallbackHeaderReducer.shown(before, message)

    val without = LayoutEngine.calculateLayout(before, viewport)
    val withIt  = LayoutEngine.calculateLayout(after, viewport)

    withIt.copy(cornerOverlayStack = without.cornerOverlayStack) shouldBe without
    ChromeFrame.of(after, viewport) shouldBe ChromeFrame.of(before, viewport)
  }

  it should "leave a bottom-right notice where it was" in {
    val viewport   = ViewportSize(80, 24)
    val withNotice = NoticeReducer.shown(stateFor(viewport), Notice(NoticeLevel.Info, "Saved."), 0L)
    val noticeId   = withNotice.runtime.uiSurfaces.headOption.map(_.id).getOrElse(fail("expected a notice"))
    val without    = LayoutEngine.calculateLayout(withNotice, viewport)
    val withIt     = LayoutEngine.calculateLayout(FallbackHeaderReducer.shown(withNotice, message), viewport)

    withIt.cornerOverlayStack.find(_._1 == noticeId) shouldBe without.cornerOverlayStack.find(_._1 == noticeId)
    withIt.cornerOverlayStack.find(_._1 == noticeId) should not be empty
  }

end FallbackHeaderLayoutSpec
