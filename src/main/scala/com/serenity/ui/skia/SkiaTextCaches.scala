package com.serenity.ui.skia

import com.serenity.ui.renderer.{BackendCacheKey, BackendCaches}

/** What a Skia surface paints text and images through. Their owner shares one set across every frame and layer it
  * draws, as it does for a Java2D surface ([[com.serenity.ui.renderer.SurfaceTextCaches]]).
  */
final case class SkiaTextCaches(fonts: SkiaFontCache, shapedText: SkiaShapedTextCache, images: SkiaImageCache)

object SkiaTextCaches:

  private val Key = BackendCacheKey(classOf[SkiaTextCaches])

  def fresh(): SkiaTextCaches =
    SkiaTextCaches(new SkiaFontCache(), new SkiaShapedTextCache(), new SkiaImageCache())

  /** The set `owner` draws with, made the first time it is asked for. */
  def of(owner: BackendCaches): SkiaTextCaches = owner.getOrCreate(Key)(fresh())

  def heldBy(owner: BackendCaches): Boolean = owner.holds(Key)
