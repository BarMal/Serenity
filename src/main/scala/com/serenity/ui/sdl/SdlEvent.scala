package com.serenity.ui.sdl

import java.lang.foreign.{MemorySegment, ValueLayout}

/** What SDL reports, read out of its 128-byte `SDL_Event`. Coordinates are in the window's logical pixels. */
enum SdlEvent:
  case Quit
  case Window(kind: SdlEvent.WindowKind, data1: Int, data2: Int)
  case Key(down: Boolean, repeat: Boolean, scancode: Int, key: Int, modifiers: Int)

  /** Text the input method committed. */
  case TextInput(text: String)

  /** Text the input method is composing, with the selected part (`start`, `length`; -1 when not set). */
  case TextEditing(text: String, start: Int, length: Int)
  case MouseMotion(x: Float, y: Float, deltaX: Float, deltaY: Float)
  case MouseButton(down: Boolean, button: Int, clicks: Int, x: Float, y: Float)
  case MouseWheel(x: Float, y: Float, ticksX: Int, ticksY: Int, mouseX: Float, mouseY: Float, flipped: Boolean)
  case ClipboardUpdate

  /** An event type the binding does not model. */
  case Unknown(eventType: Int)

object SdlEvent:

  enum WindowKind:
    case Shown, Resized, PixelSizeChanged, MouseEnter, MouseLeave, FocusGained, FocusLost, CloseRequested,
      DisplayScaleChanged

  private val windowKinds: Map[Int, WindowKind] = Map(
    SdlLayouts.eventType("SDL_EVENT_WINDOW_SHOWN")                 -> WindowKind.Shown,
    SdlLayouts.eventType("SDL_EVENT_WINDOW_RESIZED")               -> WindowKind.Resized,
    SdlLayouts.eventType("SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED")    -> WindowKind.PixelSizeChanged,
    SdlLayouts.eventType("SDL_EVENT_WINDOW_MOUSE_ENTER")           -> WindowKind.MouseEnter,
    SdlLayouts.eventType("SDL_EVENT_WINDOW_MOUSE_LEAVE")           -> WindowKind.MouseLeave,
    SdlLayouts.eventType("SDL_EVENT_WINDOW_FOCUS_GAINED")          -> WindowKind.FocusGained,
    SdlLayouts.eventType("SDL_EVENT_WINDOW_FOCUS_LOST")            -> WindowKind.FocusLost,
    SdlLayouts.eventType("SDL_EVENT_WINDOW_CLOSE_REQUESTED")       -> WindowKind.CloseRequested,
    SdlLayouts.eventType("SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED") -> WindowKind.DisplayScaleChanged
  )

  private def offset(field: String): Long = SdlLayouts.offset(field)

  /** Reads `event`, which must be at least [[SdlLayouts.EventSize]] bytes. Every read is unaligned: the buffer's own
    * alignment is the caller's business, not a precondition of decoding.
    */
  def decode(event: MemorySegment): SdlEvent =
    val eventType                    = event.get(ValueLayout.JAVA_INT_UNALIGNED, offset("SDL_CommonEvent.type"))
    def int(field: String): Int      = event.get(ValueLayout.JAVA_INT_UNALIGNED, offset(field))
    def float(field: String): Float  = event.get(ValueLayout.JAVA_FLOAT_UNALIGNED, offset(field))
    def byte(field: String): Int     = event.get(ValueLayout.JAVA_BYTE, offset(field)) & 0xff
    def flag(field: String): Boolean = event.get(ValueLayout.JAVA_BYTE, offset(field)) != 0
    def text(field: String): String  = cString(event.get(ValueLayout.ADDRESS_UNALIGNED, offset(field)))
    def is(name: String): Boolean    = eventType == SdlLayouts.eventType(name)

    if is("SDL_EVENT_QUIT") then Quit
    else if is("SDL_EVENT_KEY_DOWN") || is("SDL_EVENT_KEY_UP") then
      Key(
        flag("SDL_KeyboardEvent.down"),
        flag("SDL_KeyboardEvent.repeat"),
        int("SDL_KeyboardEvent.scancode"),
        int("SDL_KeyboardEvent.key"),
        event.get(ValueLayout.JAVA_SHORT_UNALIGNED, offset("SDL_KeyboardEvent.mod")) & 0xffff
      )
    else if is("SDL_EVENT_TEXT_INPUT") then TextInput(text("SDL_TextInputEvent.text"))
    else if is("SDL_EVENT_TEXT_EDITING") then
      TextEditing(
        text("SDL_TextEditingEvent.text"),
        int("SDL_TextEditingEvent.start"),
        int("SDL_TextEditingEvent.length")
      )
    else if is("SDL_EVENT_MOUSE_MOTION") then
      MouseMotion(
        float("SDL_MouseMotionEvent.x"),
        float("SDL_MouseMotionEvent.y"),
        float("SDL_MouseMotionEvent.xrel"),
        float("SDL_MouseMotionEvent.yrel")
      )
    else if is("SDL_EVENT_MOUSE_BUTTON_DOWN") || is("SDL_EVENT_MOUSE_BUTTON_UP") then
      MouseButton(
        flag("SDL_MouseButtonEvent.down"),
        byte("SDL_MouseButtonEvent.button"),
        byte("SDL_MouseButtonEvent.clicks"),
        float("SDL_MouseButtonEvent.x"),
        float("SDL_MouseButtonEvent.y")
      )
    else if is("SDL_EVENT_MOUSE_WHEEL") then
      MouseWheel(
        float("SDL_MouseWheelEvent.x"),
        float("SDL_MouseWheelEvent.y"),
        int("SDL_MouseWheelEvent.integer_x"),
        int("SDL_MouseWheelEvent.integer_y"),
        float("SDL_MouseWheelEvent.mouse_x"),
        float("SDL_MouseWheelEvent.mouse_y"),
        int("SDL_MouseWheelEvent.direction") == SdlLayouts.MouseWheelFlipped
      )
    else if is("SDL_EVENT_CLIPBOARD_UPDATE") then ClipboardUpdate
    else
      windowKinds
        .get(eventType)
        .fold(Unknown(eventType))(kind => Window(kind, int("SDL_WindowEvent.data1"), int("SDL_WindowEvent.data2")))

  /** The UTF-8 string a `const char *` field points at; empty for a null pointer. */
  private[sdl] def cString(pointer: MemorySegment): String =
    if pointer.address() == 0L then "" else pointer.reinterpret(Long.MaxValue).getString(0)
