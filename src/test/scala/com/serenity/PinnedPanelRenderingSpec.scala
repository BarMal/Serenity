package com.serenity

import java.awt.Color

import com.serenity.animation.{AnimatedCell, AnimationState, CharacterKey}
import com.serenity.config.{AppConfig, BackgroundStyle}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.*
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PinnedPanelRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "PinnedPanelRenderer" should "use semantic panel colors rather than hard-coded ANSI backgrounds" in {
    val surface = new MockRenderSurface(40, 12)
    val panel = TextPanelView(
      rect = LayoutRect(2, 2, 20, 6),
      title = "outline",
      rows = List(TextPanelRow("Item 1"), TextPanelRow("Item 2"))
    )

    PinnedPanelRenderer.render(surface, panel, Theme.light, AppConfig.default)

    surface.getBg(panel.rect.x + 1, panel.rect.y + 1) shouldBe Theme.light.panel.background
    surface.strokeRoundRectCalls.map(_.color) should contain(Theme.light.border)
  }

  it should "use the configured UI corner radius for pinned panel borders" in {
    val surface = new MockRenderSurface(40, 12)
    val panel = TextPanelView(
      rect = LayoutRect(2, 2, 20, 6),
      title = "outline",
      rows = List(TextPanelRow("Item 1"))
    )

    PinnedPanelRenderer.render(surface, panel, Theme.light, AppConfig.default.withUiCornerRadiusPx(14))

    surface.strokeRoundRectCalls.headOption.map(_.arcPx) shouldBe Some(14)
  }

  it should "render selected rows using the theme highlight colors" in {
    val surface = new MockRenderSurface(40, 12)
    val panel = TextPanelView(
      rect = LayoutRect(2, 2, 20, 6),
      title = "explorer",
      rows = List(
        TextPanelRow("repo"),
        TextPanelRow("  src", selected = true)
      )
    )

    PinnedPanelRenderer.render(surface, panel, Theme.light, AppConfig.default)

    surface.getBg(panel.rect.x + 1, panel.rect.y + 2) shouldBe Theme.light.highlighted.background
    surface.getFg(panel.rect.x + 1, panel.rect.y + 2) shouldBe Theme.light.highlighted.foreground
    surface.getBg(panel.rect.x + 1, panel.rect.y + 1) shouldBe Theme.light.panel.background
  }

  it should "apply active animation foreground colors to panel text" in {
    val surface            = new MockRenderSurface(40, 12)
    val animatedForeground = new Color(10, 20, 30, 96)
    val panel = TextPanelView(
      rect = LayoutRect(2, 2, 20, 6),
      title = "outline",
      rows = List(TextPanelRow("Item 1"))
    )
    val animationState = AnimationState(
      Map(
        CharacterKey(-1, -1) -> AnimatedCell(None, List(animatedForeground), Nil),
        CharacterKey(0, 1)   -> AnimatedCell(Some('I'), List(animatedForeground), Nil)
      )
    )

    PinnedPanelRenderer.render(surface, panel, Theme.light, AppConfig.default, animationState)

    surface.strokeRoundRectCalls.map(_.color) should contain(animatedForeground)
    surface.getFg(panel.rect.x + 1, panel.rect.y + 1) shouldBe animatedForeground
  }

  it should "request backdrop blur for pinned panels using the configured blur radius" in {
    val state = AppState.initial.copy(
      theme = Theme.light,
      config = AppConfig.default.withBlurRadius(0.4f),
      viewportSize = Some(ViewportSize(100, 30)),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("diagnostics"),
          SurfaceContent.Diagnostics(
            List(Diagnostic("unused import", DiagnosticSeverity.Warning, Location(1, 0)))
          ),
          SurfacePresentation.Pinned(PanelPosition.Right, 24)
        )
      )
    )
    val surface   = new MockRenderSurface(100, 30)
    val viewport  = ViewportSize(100, 30)
    val layout    = com.serenity.ui.layout.LayoutEngine.calculateLayout(state, viewport)
    val panelRect = layout.pinnedPanelRects.getOrElse(PanelPosition.Right, fail("Expected right pinned panel rect"))

    Renderer.render(state, cursorVisible = true, surface, viewport)

    surface.blurRegionCalls should contain(
      surface.BlurRegionCall(panelRect.x, panelRect.y, panelRect.width, panelRect.height, 0.4f)
    )
  }

  it should "skip backdrop blur for pinned panels when the background style is solid" in {
    val state = AppState.initial.copy(
      theme = Theme.light,
      config = AppConfig.default
        .withBlurRadius(0.4f)
        .withBackgroundStyle(BackgroundStyle.Solid),
      viewportSize = Some(ViewportSize(100, 30)),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("diagnostics"),
          SurfaceContent.Diagnostics(
            List(Diagnostic("unused import", DiagnosticSeverity.Warning, Location(1, 0)))
          ),
          SurfacePresentation.Pinned(PanelPosition.Right, 24)
        )
      )
    )
    val surface = new MockRenderSurface(100, 30)

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.blurRegionCalls shouldBe empty
  }
end PinnedPanelRenderingSpec
