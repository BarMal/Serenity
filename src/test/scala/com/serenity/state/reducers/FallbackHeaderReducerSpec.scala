package com.serenity.state.reducers

import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The header a terminal launch shows when Skia could not load (#2660 item 1): one floating row on the top edge that
  * never takes focus and goes when it is dismissed.
  */
class FallbackHeaderReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val base    = AppState.initial
  private val message = "Skia could not load: no raster surface. Running in the terminal."

  private def headers(state: AppState): List[UiSurface] =
    state.runtime.uiSurfaces.filter(_.content.isInstanceOf[SurfaceContent.FallbackHeader])

  "FallbackHeaderReducer.shown" should "add one floating top-edge surface carrying the message" in {
    headers(FallbackHeaderReducer.shown(base, message)) match
      case List(surface) =>
        surface.content shouldBe SurfaceContent.FallbackHeader(message)
        surface.presentation shouldBe SurfacePresentation.Floating(None, SurfacePlacement.TopEdge)
      case other => fail(s"expected one header, got $other")
  }

  it should "never take focus, so every key still reaches what had it" in {
    val state = FallbackHeaderReducer.shown(base, message)
    state.persisted.focus shouldBe base.persisted.focus
    state.runtime.focusHistory shouldBe base.runtime.focusHistory
    headers(state).map(_.focusPolicy) shouldBe List(SurfaceFocusPolicy.Peek)
  }

  it should "replace an earlier header rather than stack a second" in {
    val twice = FallbackHeaderReducer.shown(FallbackHeaderReducer.shown(base, "first"), "second")
    headers(twice).map(_.content) shouldBe List(SurfaceContent.FallbackHeader("second"))
  }

  it should "leave the other surfaces as they were" in {
    val withNotice = NoticeReducer.shown(base, Notice(NoticeLevel.Info, "hello"), 0L)
    val state      = FallbackHeaderReducer.shown(withNotice, message)
    state.runtime.uiSurfaces.filterNot(_.content.isInstanceOf[SurfaceContent.FallbackHeader]) shouldBe
      withNotice.runtime.uiSurfaces
  }

  "FallbackHeaderReducer.dismissed" should "remove the header and nothing else" in {
    val withNotice = NoticeReducer.shown(base, Notice(NoticeLevel.Info, "hello"), 0L)
    val state      = FallbackHeaderReducer.shown(withNotice, message)

    FallbackHeaderReducer.dismissed(state).runtime.uiSurfaces shouldBe withNotice.runtime.uiSurfaces
  }

  it should "give back the same state when there is no header" in {
    FallbackHeaderReducer.dismissed(base) should be theSameInstanceAs base
  }

  "FallbackHeaderReducer.isShown" should "say whether a header is up" in {
    FallbackHeaderReducer.isShown(base) shouldBe false
    FallbackHeaderReducer.isShown(FallbackHeaderReducer.shown(base, message)) shouldBe true
    FallbackHeaderReducer.isShown(FallbackHeaderReducer.dismissed(FallbackHeaderReducer.shown(base, message))) shouldBe
      false
  }

end FallbackHeaderReducerSpec
