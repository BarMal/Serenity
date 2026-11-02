package com.serenity.ui.display

import com.serenity.ui.display.DisplayScale.DeviceScale
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DisplaySnapshotSpec extends AnyFlatSpec with Matchers:

  private val wayland = DisplaySnapshot(
    toolkitClass = "sun.awt.wl.WLToolkit",
    scale = DeviceScale(2.0, 2.0),
    frameWidthPx = 2048,
    frameHeightPx = 1536,
    canvasWidthPx = 2040,
    canvasHeightPx = 1500,
    preferredWidthPx = Some(1024),
    preferredHeightPx = Some(768)
  )

  "DisplaySnapshot.logLine" should "name the toolkit class, scale, frame, canvas and configured size" in {
    wayland.logLine shouldBe
      "[DISPLAY] toolkit=sun.awt.wl.WLToolkit scale=2.0x2.0 frame=2048x1536 canvas=2040x1500 preferred=1024x768"
  }

  it should "show a fractional and non-square scale as it was reported" in {
    wayland.copy(scale = DeviceScale(1.5, 1.25)).logLine should include("scale=1.5x1.25")
  }

  it should "say none when no window size is configured" in {
    wayland.copy(preferredWidthPx = None, preferredHeightPx = None).logLine should endWith("preferred=none")
  }

  it should "say none when only one dimension is configured" in {
    wayland.copy(preferredHeightPx = None).logLine should endWith("preferred=none")
  }
