package com.serenity.ui.sdl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a GL window knows about itself, changed only by the events SDL reports. */
class GlWindowStateSpec extends AnyFlatSpec with Matchers:

  private val start = GlWindowState(pixelWidth = 640, pixelHeight = 480, scale = 1.0)

  private def pixelSizeChanged(width: Int, height: Int) =
    SdlEvent.Window(SdlEvent.WindowKind.PixelSizeChanged, width, height)

  "A GL window state" should "start with nothing to do" in {
    start.takeResize shouldBe (start, None)
    start.closeRequested shouldBe false
  }

  it should "report a new pixel size once, until it changes again" in {
    val resized         = start.after(pixelSizeChanged(800, 600), scaleNow = 1.0)
    val (taken, report) = resized.takeResize
    report shouldBe Some((800, 600))
    taken.takeResize._2 shouldBe None
    taken.after(pixelSizeChanged(801, 600), 1.0).takeResize._2 shouldBe Some((801, 600))
  }

  it should "not report a resize when the pixel size is the one it had" in {
    start.after(pixelSizeChanged(640, 480), 1.0).takeResize._2 shouldBe None
  }

  it should "take the new scale from the display and report a resize, since the cells change size with it" in {
    val rescaled = start.after(SdlEvent.Window(SdlEvent.WindowKind.DisplayScaleChanged, 0, 0), scaleNow = 1.5)
    rescaled.scale shouldBe 1.5
    rescaled.takeResize._2 shouldBe Some((640, 480))
  }

  it should "note a close request from the window manager or from a quit event" in {
    start.after(SdlEvent.Window(SdlEvent.WindowKind.CloseRequested, 0, 0), 1.0).closeRequested shouldBe true
    start.after(SdlEvent.Quit, 1.0).closeRequested shouldBe true
  }

  it should "ignore the events it does not follow" in {
    val ignored = List(
      SdlEvent.Window(SdlEvent.WindowKind.FocusGained, 0, 0),
      SdlEvent.Key(down = true, repeat = false, scancode = 4, key = 97, modifiers = 0),
      SdlEvent.MouseMotion(1f, 2f, 0f, 0f),
      SdlEvent.Unknown(9999)
    )
    ignored.foreach(event => start.after(event, 2.0) shouldBe start)
  }
