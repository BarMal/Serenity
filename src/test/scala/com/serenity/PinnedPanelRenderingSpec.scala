package com.serenity

import com.serenity.ui.layout.LayoutRect
import com.serenity.ui.renderer.{PinnedPanelRenderer, TextPanelView}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PinnedPanelRenderingSpec extends AnyFlatSpec with Matchers:

  "PinnedPanelRenderer" should "use semantic panel colors rather than hard-coded ANSI backgrounds" in {
    val surface = new MockRenderSurface(40, 12)
    val panel = TextPanelView(
      rect = LayoutRect(2, 2, 20, 6),
      title = "outline",
      lines = List("Item 1", "Item 2")
    )

    PinnedPanelRenderer.render(surface, panel, Theme.light)

    surface.getBg(panel.rect.x + 1, panel.rect.y + 1) shouldBe Theme.light.panel.background
    surface.getFg(panel.rect.x + 1, panel.rect.y) shouldBe Theme.light.border
  }
end PinnedPanelRenderingSpec
