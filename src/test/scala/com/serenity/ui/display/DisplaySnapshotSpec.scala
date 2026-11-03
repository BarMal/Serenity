package com.serenity.ui.display

import javax.swing.JPanel

import com.serenity.config.PreferredWindowSize
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

  "DisplaySnapshot.settledLogLine" should "carry the same fields as logLine, after saying whether the size had settled" in {
    wayland.settledLogLine(settled = true) shouldBe
      "[DISPLAY] settled=true toolkit=sun.awt.wl.WLToolkit scale=2.0x2.0 frame=2048x1536 canvas=2040x1500 " +
      "preferred=1024x768"
  }

  it should "say so when the size was still changing" in {
    wayland.settledLogLine(settled = false) should startWith("[DISPLAY] settled=false ")
  }

  "DisplaySnapshot.of" should "read the canvas size and the configured size, and say 0x0 for a frame it has no window for" in {
    val canvas = new JPanel
    canvas.setSize(1024, 768)

    val snapshot = DisplaySnapshot.of(canvas, Some(PreferredWindowSize(1024, 768)))

    snapshot.canvasWidthPx shouldBe 1024
    snapshot.canvasHeightPx shouldBe 768
    snapshot.frameWidthPx shouldBe 0
    snapshot.frameHeightPx shouldBe 0
    snapshot.preferredWidthPx shouldBe Some(1024)
    snapshot.preferredHeightPx shouldBe Some(768)
    snapshot.scale shouldBe DisplayScale.One
  }

  it should "say none for a window size that is not configured" in {
    DisplaySnapshot.of(new JPanel, None).logLine should endWith("preferred=none")
  }
