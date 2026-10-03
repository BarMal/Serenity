package com.serenity.frontend

import com.serenity.keystroke.KeyboardFidelityTier
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #1669: `FrontendCapabilities` replaces `Runtime.isTuiMode`'s single boolean with the independent flags pure
  * code actually branches on. Each flag is asserted on its own -- a GUI/TUI split alone would not catch a capabilities
  * value that mixes flags in a way neither `FrontendCapabilities.gui` nor `.tui` produces (e.g. a future frontend with
  * `pixelMotion = false` but `typography = true`, issue #1532's narrower-than-full-GUI mintty case).
  */
class FrontendCapabilitiesSpec extends AnyFlatSpec with Matchers:

  "FrontendCapabilities.gui" should "measure on a pixel grid" in {
    FrontendCapabilities.gui.grid shouldBe MetricGrid.Pixels
    FrontendCapabilities.gui.isCellGrid shouldBe false
  }

  it should "enable caret glide and smooth scroll" in {
    FrontendCapabilities.gui.pixelMotion shouldBe true
  }

  it should "make font family/size/ligature settings meaningful" in {
    FrontendCapabilities.gui.typography shouldBe true
  }

  it should "default to full keyboard fidelity, since a focused Swing window decodes AWT key events directly" in {
    FrontendCapabilities.gui.keyboardFidelityTier shouldBe KeyboardFidelityTier.Full
  }

  "FrontendCapabilities.tui" should "measure on the terminal's fixed cell grid" in {
    FrontendCapabilities.tui().grid shouldBe MetricGrid.Cells
    FrontendCapabilities.tui().isCellGrid shouldBe true
  }

  it should "disable caret glide and smooth scroll -- a fixed cell grid cannot represent sub-cell motion" in {
    FrontendCapabilities.tui().pixelMotion shouldBe false
  }

  it should "make font family/size/ligature settings inert -- nothing paints differently on a fixed-cell surface" in {
    FrontendCapabilities.tui().typography shouldBe false
  }

  it should "default to full keyboard fidelity, but accept a narrower negotiated tier" in {
    FrontendCapabilities.tui().keyboardFidelityTier shouldBe KeyboardFidelityTier.Full
    FrontendCapabilities.tui(KeyboardFidelityTier.ModifyOtherKeys).keyboardFidelityTier shouldBe
      KeyboardFidelityTier.ModifyOtherKeys
  }

  "a capabilities value with a mixed flag combination" should
    "still answer isCellGrid from the grid field alone, independent of the other flags (issue #1532 mintty case)" in {
      val narrowedTui = FrontendCapabilities.tui().copy(pixelMotion = true)

      narrowedTui.isCellGrid shouldBe true
      narrowedTui.pixelMotion shouldBe true
      narrowedTui.typography shouldBe false
    }
