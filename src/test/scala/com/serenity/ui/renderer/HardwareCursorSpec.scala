package com.serenity.ui.renderer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers #1170's `HardwareCursorStyle.decscusrParam`: the pure mapping from a shape/blink pair to the DECSCUSR (`CSI
  * Ps SP q`) parameter xterm's ctlseqs documents -- 1/2 block blinking/steady, 3/4 underline blinking/steady, 5/6 bar
  * blinking/steady. Kept as its own unit test, independent of any terminal I/O, so the mapping itself is pinned
  * regardless of how [[TerminalRenderSurfaceSpec]] wires it into real ANSI output.
  */
class HardwareCursorSpec extends AnyFlatSpec with Matchers:

  "decscusrParam" should "map every shape/blink combination to its documented DECSCUSR parameter" in {
    HardwareCursorStyle(HardwareCursorShape.Block, blinking = true).decscusrParam shouldBe 1
    HardwareCursorStyle(HardwareCursorShape.Block, blinking = false).decscusrParam shouldBe 2
    HardwareCursorStyle(HardwareCursorShape.Underline, blinking = true).decscusrParam shouldBe 3
    HardwareCursorStyle(HardwareCursorShape.Underline, blinking = false).decscusrParam shouldBe 4
    HardwareCursorStyle(HardwareCursorShape.Bar, blinking = true).decscusrParam shouldBe 5
    HardwareCursorStyle(HardwareCursorShape.Bar, blinking = false).decscusrParam shouldBe 6
  }
