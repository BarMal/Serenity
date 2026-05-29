package com.serenity

import com.googlecode.lanterna.screen.TerminalScreen
import com.googlecode.lanterna.terminal.virtual.DefaultVirtualTerminal
import com.googlecode.lanterna.{TerminalSize as LanternaSize}
import com.serenity.ui.layout.LayoutRect
import com.serenity.ui.renderer.{LanternaRenderSurface, PinnedPanelRenderer, TextPanelView}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PinnedPanelRenderingSpec extends AnyFlatSpec with Matchers:

  private def screen(width: Int, height: Int): TerminalScreen =
    val terminal = new DefaultVirtualTerminal(new LanternaSize(width, height))
    val screen   = new TerminalScreen(terminal)
    screen.startScreen()
    screen

  "PinnedPanelRenderer" should "use semantic panel colors rather than hard-coded ANSI backgrounds" in {
    val testScreen = screen(40, 12)
    val surface    = LanternaRenderSurface(testScreen, testScreen.newTextGraphics())
    val panel = TextPanelView(
      rect = LayoutRect(2, 2, 20, 6),
      title = "outline",
      lines = List("Item 1", "Item 2")
    )

    PinnedPanelRenderer.render(surface, panel, Theme.light)

    testScreen.getBackCharacter(panel.rect.x + 1, panel.rect.y + 1).getBackgroundColor shouldBe Theme.light.panel.background
    testScreen.getBackCharacter(panel.rect.x + 1, panel.rect.y).getForegroundColor shouldBe Theme.light.border

    testScreen.stopScreen()
  }
end PinnedPanelRenderingSpec
