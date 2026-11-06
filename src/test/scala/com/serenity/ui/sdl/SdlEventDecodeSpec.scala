package com.serenity.ui.sdl

import java.lang.foreign.{Arena, MemorySegment, ValueLayout}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Events are read from raw bytes laid out the way the SDL headers say, so the decoder is tested without the library.
  */
class SdlEventDecodeSpec extends AnyFlatSpec with Matchers:

  private def withEvent[A](eventType: String)(fill: (MemorySegment, Arena) => Unit)(check: SdlEvent => A): A =
    val arena = Arena.ofConfined()
    try
      val event = arena.allocate(SdlGolden("sizeof(SDL_Event)"))
      event.set(ValueLayout.JAVA_INT, SdlGolden("SDL_CommonEvent.type"), SdlGolden(eventType).toInt)
      fill(event, arena)
      check(SdlEvent.decode(event))
    finally arena.close()

  private def int(event: MemorySegment, field: String, value: Int): Unit =
    event.set(ValueLayout.JAVA_INT_UNALIGNED, SdlGolden(field), value)

  private def float(event: MemorySegment, field: String, value: Float): Unit =
    event.set(ValueLayout.JAVA_FLOAT_UNALIGNED, SdlGolden(field), value)

  private def bool(event: MemorySegment, field: String, value: Boolean): Unit =
    event.set(ValueLayout.JAVA_BYTE, SdlGolden(field), if value then 1.toByte else 0.toByte)

  private def text(event: MemorySegment, arena: Arena, field: String, value: String): Unit =
    event.set(ValueLayout.ADDRESS_UNALIGNED, SdlGolden(field), arena.allocateFrom(value))

  "SdlEvent.decode" should "read a key press with its repeat flag, codes and modifiers" in
    withEvent("SDL_EVENT_KEY_DOWN") { (event, _) =>
      int(event, "SDL_KeyboardEvent.scancode", 4)
      int(event, "SDL_KeyboardEvent.key", 0x61)
      event.set(ValueLayout.JAVA_SHORT_UNALIGNED, SdlGolden("SDL_KeyboardEvent.mod"), 0x0003.toShort)
      bool(event, "SDL_KeyboardEvent.down", true)
      bool(event, "SDL_KeyboardEvent.repeat", true)
    }(_ shouldBe SdlEvent.Key(down = true, repeat = true, scancode = 4, key = 0x61, modifiers = 3))

  it should "read a key release as not down" in
    withEvent("SDL_EVENT_KEY_UP") { (event, _) =>
      int(event, "SDL_KeyboardEvent.key", 0x1000000d)
      bool(event, "SDL_KeyboardEvent.down", false)
    }(_ shouldBe SdlEvent.Key(down = false, repeat = false, scancode = 0, key = 0x1000000d, modifiers = 0))

  it should "read committed text as UTF-8 code points, not as bytes" in
    withEvent("SDL_EVENT_TEXT_INPUT")((event, arena) => text(event, arena, "SDL_TextInputEvent.text", "aé中😀"))(
      _ shouldBe SdlEvent.TextInput("aé中😀")
    )

  it should "read pre-edit text with its selection" in
    withEvent("SDL_EVENT_TEXT_EDITING") { (event, arena) =>
      text(event, arena, "SDL_TextEditingEvent.text", "ni hao")
      int(event, "SDL_TextEditingEvent.start", 2)
      int(event, "SDL_TextEditingEvent.length", 3)
    }(_ shouldBe SdlEvent.TextEditing("ni hao", start = 2, length = 3))

  it should "read mouse motion in window coordinates" in
    withEvent("SDL_EVENT_MOUSE_MOTION") { (event, _) =>
      float(event, "SDL_MouseMotionEvent.x", 12.5f)
      float(event, "SDL_MouseMotionEvent.y", 40.25f)
      float(event, "SDL_MouseMotionEvent.xrel", 1.5f)
      float(event, "SDL_MouseMotionEvent.yrel", -2f)
    }(_ shouldBe SdlEvent.MouseMotion(x = 12.5f, y = 40.25f, deltaX = 1.5f, deltaY = -2f))

  it should "read a mouse button press with its click count" in
    withEvent("SDL_EVENT_MOUSE_BUTTON_DOWN") { (event, _) =>
      event.set(ValueLayout.JAVA_BYTE, SdlGolden("SDL_MouseButtonEvent.button"), SdlGolden("SDL_BUTTON_RIGHT").toByte)
      bool(event, "SDL_MouseButtonEvent.down", true)
      event.set(ValueLayout.JAVA_BYTE, SdlGolden("SDL_MouseButtonEvent.clicks"), 2.toByte)
      float(event, "SDL_MouseButtonEvent.x", 7f)
      float(event, "SDL_MouseButtonEvent.y", 9f)
    }(_ shouldBe SdlEvent.MouseButton(down = true, button = 3, clicks = 2, x = 7f, y = 9f))

  it should "read a wheel scroll with its whole ticks and direction" in
    withEvent("SDL_EVENT_MOUSE_WHEEL") { (event, _) =>
      float(event, "SDL_MouseWheelEvent.x", 0f)
      float(event, "SDL_MouseWheelEvent.y", 1.5f)
      int(event, "SDL_MouseWheelEvent.direction", SdlGolden("SDL_MOUSEWHEEL_FLIPPED").toInt)
      float(event, "SDL_MouseWheelEvent.mouse_x", 3f)
      float(event, "SDL_MouseWheelEvent.mouse_y", 4f)
      int(event, "SDL_MouseWheelEvent.integer_x", 0)
      int(event, "SDL_MouseWheelEvent.integer_y", 1)
    }(
      _ shouldBe SdlEvent.MouseWheel(
        x = 0f,
        y = 1.5f,
        ticksX = 0,
        ticksY = 1,
        mouseX = 3f,
        mouseY = 4f,
        flipped = true
      )
    )

  it should "read window events with their data" in {
    withEvent("SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED") { (event, _) =>
      int(event, "SDL_WindowEvent.data1", 3840)
      int(event, "SDL_WindowEvent.data2", 2160)
    }(_ shouldBe SdlEvent.Window(SdlEvent.WindowKind.PixelSizeChanged, 3840, 2160))
    withEvent("SDL_EVENT_WINDOW_FOCUS_LOST")((_, _) => ())(
      _ shouldBe SdlEvent.Window(SdlEvent.WindowKind.FocusLost, 0, 0)
    )
    withEvent("SDL_EVENT_WINDOW_CLOSE_REQUESTED")((_, _) => ())(
      _ shouldBe SdlEvent.Window(SdlEvent.WindowKind.CloseRequested, 0, 0)
    )
  }

  it should "read the events that carry nothing" in {
    withEvent("SDL_EVENT_QUIT")((_, _) => ())(_ shouldBe SdlEvent.Quit)
    withEvent("SDL_EVENT_CLIPBOARD_UPDATE")((_, _) => ())(_ shouldBe SdlEvent.ClipboardUpdate)
  }

  it should "pass an event it does not model through with its type" in
    withEvent("SDL_EVENT_USER")((_, _) => ())(_ shouldBe SdlEvent.Unknown(SdlGolden("SDL_EVENT_USER").toInt))
