package com.serenity.ui.terminal

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SwingWindowMenuBarPlacementSpec extends AnyFlatSpec with Matchers:

  "The menu bar" should "go in the frame under native window decorations" in {
    SwingWindow.menuBarPlacement("Windows 11", usesCustomChrome = false) shouldBe MenuBarPlacement.FrameMenuBar
    SwingWindow.menuBarPlacement("Linux", usesCustomChrome = false) shouldBe MenuBarPlacement.FrameMenuBar
  }

  it should "sit under the custom title bar, not above it, with custom chrome" in {
    SwingWindow.menuBarPlacement("Linux", usesCustomChrome = true) shouldBe MenuBarPlacement.UnderCustomTitleBar
  }

  it should "be the screen menu bar on macOS, custom chrome or not" in {
    SwingWindow.menuBarPlacement("Mac OS X", usesCustomChrome = false) shouldBe MenuBarPlacement.ScreenMenuBar
    SwingWindow.menuBarPlacement("Mac OS X", usesCustomChrome = true) shouldBe MenuBarPlacement.ScreenMenuBar
  }

  "Menus" should "be installed on Windows and Linux only until the macOS integration lands" in {
    SwingMenuBar.enabledFor("Linux") shouldBe true
    SwingMenuBar.enabledFor("Windows 11") shouldBe true
    SwingMenuBar.enabledFor("Mac OS X") shouldBe false
  }
