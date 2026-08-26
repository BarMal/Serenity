package com.serenity.input

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ClipboardStrategySpec extends AnyFlatSpec with Matchers:

  "ClipboardStrategy.select" should "prefer AWT whenever a display is reachable" in {
    ClipboardStrategy.select(
      hasDisplay = true,
      hasTerminalWriter = true,
      externalTool = Some(ExternalClipboardTool.Xclip)
    ) shouldBe
      ClipboardStrategy.Awt
    ClipboardStrategy.select(
      hasDisplay = true,
      hasTerminalWriter = false,
      externalTool = None
    ) shouldBe ClipboardStrategy.Awt
  }

  it should "choose OSC 52 when there is no display but the terminal exposes a writer" in {
    ClipboardStrategy.select(
      hasDisplay = false,
      hasTerminalWriter = true,
      externalTool = None
    ) shouldBe ClipboardStrategy.Osc52
    ClipboardStrategy.select(
      hasDisplay = false,
      hasTerminalWriter = true,
      externalTool = Some(ExternalClipboardTool.Xsel)
    ) shouldBe ClipboardStrategy.Osc52
  }

  it should "fall back to a detected external tool with no display and no terminal writer" in {
    ClipboardStrategy.select(
      hasDisplay = false,
      hasTerminalWriter = false,
      externalTool = Some(ExternalClipboardTool.WlClipboard)
    ) shouldBe ClipboardStrategy.ExternalTool(ExternalClipboardTool.WlClipboard)
  }

  it should "fall back to an in-process clipboard when nothing else is available" in {
    ClipboardStrategy.select(hasDisplay = false, hasTerminalWriter = false, externalTool = None) shouldBe
      ClipboardStrategy.InProcess
  }
