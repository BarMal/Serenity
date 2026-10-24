package com.serenity.ui.sdl

import java.lang.foreign.MemorySegment

/** An `SDL_Window *`. Valid until [[Sdl.destroyWindow]]. */
final case class SdlWindow private[sdl] (handle: MemorySegment)

/** An `SDL_Cursor *`. */
final case class SdlCursor private[sdl] (handle: MemorySegment)

final case class SdlRect(x: Int, y: Int, width: Int, height: Int)

/** The pixels a window shows, as SDL lays them out: `pitch` bytes a row, `format` an `SDL_PixelFormat`. SDL replaces
  * the surface when the window's size changes, so ask for it again each frame.
  */
final case class SdlSurface(width: Int, height: Int, pitch: Int, format: Int, pixels: MemorySegment)
