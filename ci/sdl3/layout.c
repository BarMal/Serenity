/* Prints the layout of the SDL3 structs and the values of the constants the Scala binding (com.serenity.ui.sdl.SdlLayouts)
 * relies on, one "name value" line each, from the headers of the pinned SDL tag. CI regenerates the list and diffs it
 * against src/test/resources/sdl3/layout.txt, so a wrong offset or constant is a failing check, not memory corruption.
 * Build: cc -I <SDL>/include -o layout ci/sdl3/layout.c && ./layout > src/test/resources/sdl3/layout.txt
 */
#include <SDL3/SDL.h>
#include <stddef.h>
#include <stdio.h>

#define SIZE(type) printf("sizeof(" #type ") %zu\n", sizeof(type))
#define OFFSET(type, field) printf(#type "." #field " %zu\n", offsetof(type, field))
#define VALUE(name) printf(#name " %lld\n", (long long) (name))

int main(void) {
  SIZE(SDL_Event);
  SIZE(SDL_Rect);
  SIZE(SDL_Surface);
  SIZE(SDL_DisplayMode);

  OFFSET(SDL_CommonEvent, type);
  OFFSET(SDL_CommonEvent, timestamp);

  OFFSET(SDL_WindowEvent, windowID);
  OFFSET(SDL_WindowEvent, data1);
  OFFSET(SDL_WindowEvent, data2);

  OFFSET(SDL_KeyboardEvent, windowID);
  OFFSET(SDL_KeyboardEvent, which);
  OFFSET(SDL_KeyboardEvent, scancode);
  OFFSET(SDL_KeyboardEvent, key);
  OFFSET(SDL_KeyboardEvent, mod);
  OFFSET(SDL_KeyboardEvent, raw);
  OFFSET(SDL_KeyboardEvent, down);
  OFFSET(SDL_KeyboardEvent, repeat);

  OFFSET(SDL_TextEditingEvent, windowID);
  OFFSET(SDL_TextEditingEvent, text);
  OFFSET(SDL_TextEditingEvent, start);
  OFFSET(SDL_TextEditingEvent, length);

  OFFSET(SDL_TextInputEvent, windowID);
  OFFSET(SDL_TextInputEvent, text);

  OFFSET(SDL_MouseMotionEvent, windowID);
  OFFSET(SDL_MouseMotionEvent, which);
  OFFSET(SDL_MouseMotionEvent, state);
  OFFSET(SDL_MouseMotionEvent, x);
  OFFSET(SDL_MouseMotionEvent, y);
  OFFSET(SDL_MouseMotionEvent, xrel);
  OFFSET(SDL_MouseMotionEvent, yrel);

  OFFSET(SDL_MouseButtonEvent, windowID);
  OFFSET(SDL_MouseButtonEvent, which);
  OFFSET(SDL_MouseButtonEvent, button);
  OFFSET(SDL_MouseButtonEvent, down);
  OFFSET(SDL_MouseButtonEvent, clicks);
  OFFSET(SDL_MouseButtonEvent, x);
  OFFSET(SDL_MouseButtonEvent, y);

  OFFSET(SDL_MouseWheelEvent, windowID);
  OFFSET(SDL_MouseWheelEvent, which);
  OFFSET(SDL_MouseWheelEvent, x);
  OFFSET(SDL_MouseWheelEvent, y);
  OFFSET(SDL_MouseWheelEvent, direction);
  OFFSET(SDL_MouseWheelEvent, mouse_x);
  OFFSET(SDL_MouseWheelEvent, mouse_y);
  OFFSET(SDL_MouseWheelEvent, integer_x);
  OFFSET(SDL_MouseWheelEvent, integer_y);

  OFFSET(SDL_Rect, x);
  OFFSET(SDL_Rect, y);
  OFFSET(SDL_Rect, w);
  OFFSET(SDL_Rect, h);

  OFFSET(SDL_Surface, flags);
  OFFSET(SDL_Surface, format);
  OFFSET(SDL_Surface, w);
  OFFSET(SDL_Surface, h);
  OFFSET(SDL_Surface, pitch);
  OFFSET(SDL_Surface, pixels);

  OFFSET(SDL_DisplayMode, displayID);
  OFFSET(SDL_DisplayMode, format);
  OFFSET(SDL_DisplayMode, w);
  OFFSET(SDL_DisplayMode, h);
  OFFSET(SDL_DisplayMode, pixel_density);
  OFFSET(SDL_DisplayMode, refresh_rate);

  VALUE(SDL_INIT_VIDEO);
  VALUE(SDL_WINDOW_RESIZABLE);
  VALUE(SDL_WINDOW_HIDDEN);
  VALUE(SDL_WINDOW_HIGH_PIXEL_DENSITY);
  VALUE(SDL_WINDOW_OPENGL);
  VALUE(SDL_WINDOW_SURFACE_VSYNC_DISABLED);
  VALUE(SDL_WINDOW_SURFACE_VSYNC_ADAPTIVE);
  VALUE(SDL_PIXELFORMAT_XRGB8888);
  VALUE(SDL_PIXELFORMAT_ARGB8888);

  VALUE(SDL_GL_RED_SIZE);
  VALUE(SDL_GL_GREEN_SIZE);
  VALUE(SDL_GL_BLUE_SIZE);
  VALUE(SDL_GL_ALPHA_SIZE);
  VALUE(SDL_GL_DOUBLEBUFFER);
  VALUE(SDL_GL_STENCIL_SIZE);
  VALUE(SDL_GL_CONTEXT_MAJOR_VERSION);
  VALUE(SDL_GL_CONTEXT_MINOR_VERSION);
  VALUE(SDL_GL_CONTEXT_PROFILE_MASK);
  VALUE(SDL_GL_CONTEXT_PROFILE_CORE);
  VALUE(SDL_GL_CONTEXT_PROFILE_COMPATIBILITY);
  VALUE(SDL_GL_CONTEXT_PROFILE_ES);

  VALUE(SDL_EVENT_QUIT);
  VALUE(SDL_EVENT_WINDOW_SHOWN);
  VALUE(SDL_EVENT_WINDOW_RESIZED);
  VALUE(SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED);
  VALUE(SDL_EVENT_WINDOW_MOUSE_ENTER);
  VALUE(SDL_EVENT_WINDOW_MOUSE_LEAVE);
  VALUE(SDL_EVENT_WINDOW_FOCUS_GAINED);
  VALUE(SDL_EVENT_WINDOW_FOCUS_LOST);
  VALUE(SDL_EVENT_WINDOW_CLOSE_REQUESTED);
  VALUE(SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED);
  VALUE(SDL_EVENT_KEY_DOWN);
  VALUE(SDL_EVENT_KEY_UP);
  VALUE(SDL_EVENT_TEXT_EDITING);
  VALUE(SDL_EVENT_TEXT_INPUT);
  VALUE(SDL_EVENT_TEXT_EDITING_CANDIDATES);
  VALUE(SDL_EVENT_MOUSE_MOTION);
  VALUE(SDL_EVENT_MOUSE_BUTTON_DOWN);
  VALUE(SDL_EVENT_MOUSE_BUTTON_UP);
  VALUE(SDL_EVENT_MOUSE_WHEEL);
  VALUE(SDL_EVENT_CLIPBOARD_UPDATE);
  VALUE(SDL_EVENT_USER);

  VALUE(SDL_SYSTEM_CURSOR_DEFAULT);
  VALUE(SDL_SYSTEM_CURSOR_TEXT);
  VALUE(SDL_SYSTEM_CURSOR_POINTER);
  VALUE(SDL_SYSTEM_CURSOR_EW_RESIZE);
  VALUE(SDL_SYSTEM_CURSOR_NS_RESIZE);
  VALUE(SDL_SYSTEM_CURSOR_NWSE_RESIZE);
  VALUE(SDL_SYSTEM_CURSOR_NESW_RESIZE);
  VALUE(SDL_SYSTEM_CURSOR_MOVE);
  VALUE(SDL_SYSTEM_CURSOR_NOT_ALLOWED);
  VALUE(SDL_SYSTEM_CURSOR_WAIT);
  VALUE(SDL_SYSTEM_CURSOR_CROSSHAIR);

  VALUE(SDL_BUTTON_LEFT);
  VALUE(SDL_BUTTON_MIDDLE);
  VALUE(SDL_BUTTON_RIGHT);
  VALUE(SDL_MOUSEWHEEL_NORMAL);
  VALUE(SDL_MOUSEWHEEL_FLIPPED);
  return 0;
}
