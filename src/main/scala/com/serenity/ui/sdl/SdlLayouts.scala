package com.serenity.ui.sdl

/** The byte layout of the SDL3 structs the binding reads and writes, and the values of the constants it passes, for the
  * pinned SDL release (`ci/sdl3/pins.env`). `SdlLayoutsSpec` holds every entry against what the SDL headers print
  * (`ci/sdl3/layout.c`), so this table is checked, not trusted.
  */
object SdlLayouts:

  val entries: Map[String, Long] = Map(
    "sizeof(SDL_Event)"                      -> 128L,
    "sizeof(SDL_Rect)"                       -> 16L,
    "sizeof(SDL_Surface)"                    -> 48L,
    "sizeof(SDL_DisplayMode)"                -> 40L,
    "SDL_CommonEvent.type"                   -> 0L,
    "SDL_CommonEvent.timestamp"              -> 8L,
    "SDL_WindowEvent.windowID"               -> 16L,
    "SDL_WindowEvent.data1"                  -> 20L,
    "SDL_WindowEvent.data2"                  -> 24L,
    "SDL_KeyboardEvent.windowID"             -> 16L,
    "SDL_KeyboardEvent.which"                -> 20L,
    "SDL_KeyboardEvent.scancode"             -> 24L,
    "SDL_KeyboardEvent.key"                  -> 28L,
    "SDL_KeyboardEvent.mod"                  -> 32L,
    "SDL_KeyboardEvent.raw"                  -> 34L,
    "SDL_KeyboardEvent.down"                 -> 36L,
    "SDL_KeyboardEvent.repeat"               -> 37L,
    "SDL_TextEditingEvent.windowID"          -> 16L,
    "SDL_TextEditingEvent.text"              -> 24L,
    "SDL_TextEditingEvent.start"             -> 32L,
    "SDL_TextEditingEvent.length"            -> 36L,
    "SDL_TextInputEvent.windowID"            -> 16L,
    "SDL_TextInputEvent.text"                -> 24L,
    "SDL_MouseMotionEvent.windowID"          -> 16L,
    "SDL_MouseMotionEvent.which"             -> 20L,
    "SDL_MouseMotionEvent.state"             -> 24L,
    "SDL_MouseMotionEvent.x"                 -> 28L,
    "SDL_MouseMotionEvent.y"                 -> 32L,
    "SDL_MouseMotionEvent.xrel"              -> 36L,
    "SDL_MouseMotionEvent.yrel"              -> 40L,
    "SDL_MouseButtonEvent.windowID"          -> 16L,
    "SDL_MouseButtonEvent.which"             -> 20L,
    "SDL_MouseButtonEvent.button"            -> 24L,
    "SDL_MouseButtonEvent.down"              -> 25L,
    "SDL_MouseButtonEvent.clicks"            -> 26L,
    "SDL_MouseButtonEvent.x"                 -> 28L,
    "SDL_MouseButtonEvent.y"                 -> 32L,
    "SDL_MouseWheelEvent.windowID"           -> 16L,
    "SDL_MouseWheelEvent.which"              -> 20L,
    "SDL_MouseWheelEvent.x"                  -> 24L,
    "SDL_MouseWheelEvent.y"                  -> 28L,
    "SDL_MouseWheelEvent.direction"          -> 32L,
    "SDL_MouseWheelEvent.mouse_x"            -> 36L,
    "SDL_MouseWheelEvent.mouse_y"            -> 40L,
    "SDL_MouseWheelEvent.integer_x"          -> 44L,
    "SDL_MouseWheelEvent.integer_y"          -> 48L,
    "SDL_Rect.x"                             -> 0L,
    "SDL_Rect.y"                             -> 4L,
    "SDL_Rect.w"                             -> 8L,
    "SDL_Rect.h"                             -> 12L,
    "SDL_Surface.flags"                      -> 0L,
    "SDL_Surface.format"                     -> 4L,
    "SDL_Surface.w"                          -> 8L,
    "SDL_Surface.h"                          -> 12L,
    "SDL_Surface.pitch"                      -> 16L,
    "SDL_Surface.pixels"                     -> 24L,
    "SDL_DisplayMode.displayID"              -> 0L,
    "SDL_DisplayMode.format"                 -> 4L,
    "SDL_DisplayMode.w"                      -> 8L,
    "SDL_DisplayMode.h"                      -> 12L,
    "SDL_DisplayMode.pixel_density"          -> 16L,
    "SDL_DisplayMode.refresh_rate"           -> 20L,
    "SDL_INIT_VIDEO"                         -> 32L,
    "SDL_WINDOW_RESIZABLE"                   -> 32L,
    "SDL_WINDOW_HIDDEN"                      -> 8L,
    "SDL_WINDOW_HIGH_PIXEL_DENSITY"          -> 8192L,
    "SDL_WINDOW_OPENGL"                      -> 2L,
    "SDL_WINDOW_SURFACE_VSYNC_DISABLED"      -> 0L,
    "SDL_WINDOW_SURFACE_VSYNC_ADAPTIVE"      -> -1L,
    "SDL_PIXELFORMAT_XRGB8888"               -> 370546692L,
    "SDL_PIXELFORMAT_ARGB8888"               -> 372645892L,
    "SDL_GL_RED_SIZE"                        -> 0L,
    "SDL_GL_GREEN_SIZE"                      -> 1L,
    "SDL_GL_BLUE_SIZE"                       -> 2L,
    "SDL_GL_ALPHA_SIZE"                      -> 3L,
    "SDL_GL_DOUBLEBUFFER"                    -> 5L,
    "SDL_GL_STENCIL_SIZE"                    -> 7L,
    "SDL_GL_CONTEXT_MAJOR_VERSION"           -> 17L,
    "SDL_GL_CONTEXT_MINOR_VERSION"           -> 18L,
    "SDL_GL_CONTEXT_PROFILE_MASK"            -> 20L,
    "SDL_GL_CONTEXT_PROFILE_CORE"            -> 1L,
    "SDL_GL_CONTEXT_PROFILE_COMPATIBILITY"   -> 2L,
    "SDL_GL_CONTEXT_PROFILE_ES"              -> 4L,
    "SDL_EVENT_QUIT"                         -> 256L,
    "SDL_EVENT_WINDOW_SHOWN"                 -> 514L,
    "SDL_EVENT_WINDOW_RESIZED"               -> 518L,
    "SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED"    -> 519L,
    "SDL_EVENT_WINDOW_MOUSE_ENTER"           -> 524L,
    "SDL_EVENT_WINDOW_MOUSE_LEAVE"           -> 525L,
    "SDL_EVENT_WINDOW_FOCUS_GAINED"          -> 526L,
    "SDL_EVENT_WINDOW_FOCUS_LOST"            -> 527L,
    "SDL_EVENT_WINDOW_CLOSE_REQUESTED"       -> 528L,
    "SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED" -> 532L,
    "SDL_EVENT_KEY_DOWN"                     -> 768L,
    "SDL_EVENT_KEY_UP"                       -> 769L,
    "SDL_EVENT_TEXT_EDITING"                 -> 770L,
    "SDL_EVENT_TEXT_INPUT"                   -> 771L,
    "SDL_EVENT_TEXT_EDITING_CANDIDATES"      -> 775L,
    "SDL_EVENT_MOUSE_MOTION"                 -> 1024L,
    "SDL_EVENT_MOUSE_BUTTON_DOWN"            -> 1025L,
    "SDL_EVENT_MOUSE_BUTTON_UP"              -> 1026L,
    "SDL_EVENT_MOUSE_WHEEL"                  -> 1027L,
    "SDL_EVENT_CLIPBOARD_UPDATE"             -> 2304L,
    "SDL_EVENT_USER"                         -> 32768L,
    "SDL_SYSTEM_CURSOR_DEFAULT"              -> 0L,
    "SDL_SYSTEM_CURSOR_TEXT"                 -> 1L,
    "SDL_SYSTEM_CURSOR_POINTER"              -> 11L,
    "SDL_SYSTEM_CURSOR_EW_RESIZE"            -> 7L,
    "SDL_SYSTEM_CURSOR_NS_RESIZE"            -> 8L,
    "SDL_SYSTEM_CURSOR_NWSE_RESIZE"          -> 5L,
    "SDL_SYSTEM_CURSOR_NESW_RESIZE"          -> 6L,
    "SDL_SYSTEM_CURSOR_MOVE"                 -> 9L,
    "SDL_SYSTEM_CURSOR_NOT_ALLOWED"          -> 10L,
    "SDL_SYSTEM_CURSOR_WAIT"                 -> 2L,
    "SDL_SYSTEM_CURSOR_CROSSHAIR"            -> 3L,
    "SDL_BUTTON_LEFT"                        -> 1L,
    "SDL_BUTTON_MIDDLE"                      -> 2L,
    "SDL_BUTTON_RIGHT"                       -> 3L,
    "SDL_MOUSEWHEEL_NORMAL"                  -> 0L,
    "SDL_MOUSEWHEEL_FLIPPED"                 -> 1L
  )

  private def at(name: String): Long = entries(name)

  val EventSize: Long   = at("sizeof(SDL_Event)")
  val RectSize: Long    = at("sizeof(SDL_Rect)")
  val SurfaceSize: Long = at("sizeof(SDL_Surface)")

  val InitVideo: Int               = at("SDL_INIT_VIDEO").toInt
  val WindowResizable: Long        = at("SDL_WINDOW_RESIZABLE")
  val WindowHidden: Long           = at("SDL_WINDOW_HIDDEN")
  val WindowHighPixelDensity: Long = at("SDL_WINDOW_HIGH_PIXEL_DENSITY")
  val WindowOpenGl: Long           = at("SDL_WINDOW_OPENGL")
  val GlProfileEs: Int             = at("SDL_GL_CONTEXT_PROFILE_ES").toInt
  val SurfaceVsyncDisabled: Int    = at("SDL_WINDOW_SURFACE_VSYNC_DISABLED").toInt
  val SurfaceVsyncAdaptive: Int    = at("SDL_WINDOW_SURFACE_VSYNC_ADAPTIVE").toInt
  val PixelFormatXrgb8888: Int     = at("SDL_PIXELFORMAT_XRGB8888").toInt
  val PixelFormatArgb8888: Int     = at("SDL_PIXELFORMAT_ARGB8888").toInt
  val ButtonLeft: Int              = at("SDL_BUTTON_LEFT").toInt
  val MouseWheelFlipped: Int       = at("SDL_MOUSEWHEEL_FLIPPED").toInt

  /** An `SDL_EventType`, by the name the header gives it. */
  def eventType(name: String): Int = at(name).toInt

  /** Byte offset of `Struct.field` within its struct. */
  def offset(name: String): Long = at(name)

  /** An `SDL_GLAttr`, to be set before the window that needs it is created. */
  enum GlAttribute(val id: Int):
    case RedSize             extends GlAttribute(at("SDL_GL_RED_SIZE").toInt)
    case GreenSize           extends GlAttribute(at("SDL_GL_GREEN_SIZE").toInt)
    case BlueSize            extends GlAttribute(at("SDL_GL_BLUE_SIZE").toInt)
    case AlphaSize           extends GlAttribute(at("SDL_GL_ALPHA_SIZE").toInt)
    case DoubleBuffer        extends GlAttribute(at("SDL_GL_DOUBLEBUFFER").toInt)
    case StencilSize         extends GlAttribute(at("SDL_GL_STENCIL_SIZE").toInt)
    case ContextMajorVersion extends GlAttribute(at("SDL_GL_CONTEXT_MAJOR_VERSION").toInt)
    case ContextMinorVersion extends GlAttribute(at("SDL_GL_CONTEXT_MINOR_VERSION").toInt)
    case ContextProfileMask  extends GlAttribute(at("SDL_GL_CONTEXT_PROFILE_MASK").toInt)

  enum SystemCursor(val id: Int):
    case Default    extends SystemCursor(at("SDL_SYSTEM_CURSOR_DEFAULT").toInt)
    case Text       extends SystemCursor(at("SDL_SYSTEM_CURSOR_TEXT").toInt)
    case Pointer    extends SystemCursor(at("SDL_SYSTEM_CURSOR_POINTER").toInt)
    case EwResize   extends SystemCursor(at("SDL_SYSTEM_CURSOR_EW_RESIZE").toInt)
    case NsResize   extends SystemCursor(at("SDL_SYSTEM_CURSOR_NS_RESIZE").toInt)
    case NwseResize extends SystemCursor(at("SDL_SYSTEM_CURSOR_NWSE_RESIZE").toInt)
    case NeswResize extends SystemCursor(at("SDL_SYSTEM_CURSOR_NESW_RESIZE").toInt)
    case Move       extends SystemCursor(at("SDL_SYSTEM_CURSOR_MOVE").toInt)
    case NotAllowed extends SystemCursor(at("SDL_SYSTEM_CURSOR_NOT_ALLOWED").toInt)
    case Wait       extends SystemCursor(at("SDL_SYSTEM_CURSOR_WAIT").toInt)
    case Crosshair  extends SystemCursor(at("SDL_SYSTEM_CURSOR_CROSSHAIR").toInt)
