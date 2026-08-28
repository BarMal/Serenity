package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.ui.layout.PixelRect
import com.serenity.ui.theme.TextStyle

/** Identity of the pixels a [[RenderSurface]] accumulates into. A newtype over the backing object's own reference
  * identity (typically the image it draws into) rather than a bare `AnyRef`, so the render path's cache keys carry an
  * asserted domain meaning instead of an untyped object reference. Erases to the wrapped value at runtime, so equality,
  * hashing and weak-reference behaviour are exactly the backing object's own -- what a `WeakHashMap[AnyRef, _]` needs
  * to track it correctly.
  */
opaque type SurfaceContentIdentity = AnyRef

object SurfaceContentIdentity:
  def apply(value: AnyRef): SurfaceContentIdentity = value

trait RenderSurface:

  /** Identity of the pixels this surface accumulates into, when it preserves what earlier frames drew.
    *
    * `Some(key)` promises two things: the surface still holds the pixels of the last frame flushed through a surface
    * reporting the same key, and [[clearViewportExcept]] genuinely preserves the rectangles it is handed. Renderers may
    * only skip redrawing unchanged content when a key is present. `None` — the default — means every frame starts from
    * unknown pixels and everything must be drawn.
    */
  def persistentContentKey: Option[SurfaceContentIdentity] = None
  def setForegroundColor(color: Color): Unit
  def setBackgroundColor(color: Color): Unit
  def getBackgroundColor: Color

  def clearViewport(color: Color): Unit =
    setBackgroundColor(color)
    fillRect(0, 0, viewportWidth, viewportHeight, ' ')

  /** Clear the viewport to `color` while leaving the given logical-pixel rectangles untouched.
    *
    * The default clears everything, which is why callers must check [[persistentContentKey]] first: a surface without a
    * persistent key preserves nothing, so its caller has to redraw the content it would otherwise have skipped.
    */
  def clearViewportExcept(color: Color, preserved: List[PixelRect]): Unit =
    clearViewport(color)

  def putString(x: Int, y: Int, s: String): Unit
  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit
  def enableStyle(style: TextStyle): Unit
  def disableStyle(style: TextStyle): Unit
  def devicePixelScaleX: Double = 1.0
  def devicePixelScaleY: Double = 1.0

  /** Character- and pixel-run text drawing. Required: every real surface must draw text. */
  def text: TextDrawing

  /** Pixel-addressed rect fills, image blits, and pixel-space translation. Required: every real surface must draw
    * carets and images.
    */
  def pixels: PixelDrawing

  /** Alpha compositing, region blur, and post-processing, when this surface supports them. `None` means callers must
    * skip the effect rather than assume it happened.
    */
  def effects: Option[Effects] = None

  /** Rounded-rectangle borders, shadows, and clipping, when this surface supports them. `None` means callers must fall
    * back to drawing without that chrome rather than assume it happened.
    */
  def roundedRects: Option[RoundedRectDrawing] = None

  def hideCursor(): Unit
  def viewportWidth: Int
  def viewportHeight: Int
  def flush(): Unit
