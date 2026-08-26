package com.serenity.input

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExternalClipboardToolSpec extends AnyFlatSpec with Matchers:

  "ExternalClipboardTool.detect" should "prefer the Wayland tool when both wl-copy and wl-paste are on PATH" in {
    ExternalClipboardTool.detect(command => Set("wl-copy", "wl-paste", "xclip").contains(command)) shouldBe
      Some(ExternalClipboardTool.WlClipboard)
  }

  it should "require both the write and read executables of a tool, not just one" in {
    ExternalClipboardTool.detect(command => command == "wl-copy") shouldBe None
  }

  it should "fall back to xclip when wl-copy/wl-paste are absent" in {
    ExternalClipboardTool.detect(command => Set("xclip").contains(command)) shouldBe Some(ExternalClipboardTool.Xclip)
  }

  it should "fall back to xsel when neither the Wayland nor xclip tools are present" in {
    ExternalClipboardTool.detect(command => Set("xsel").contains(command)) shouldBe Some(ExternalClipboardTool.Xsel)
  }

  it should "report no tool when nothing is on PATH" in {
    ExternalClipboardTool.detect(_ => false) shouldBe None
  }
