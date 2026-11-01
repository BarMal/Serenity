package com.serenity.ui.skia

import com.serenity.ui.renderer.{BackendCacheKey, BackendCaches}

/** What a Skia surface paints text and images through. Their owner shares one set across every frame and layer it
  * draws, as it does for a Java2D surface ([[com.serenity.ui.renderer.SurfaceTextCaches]]).
  */
final case class SkiaTextCaches(
    fonts: SkiaFontCache,
    shapedText: SkiaShapedTextCache,
    images: SkiaImageCache,
    inks: SkiaInkCache = new SkiaInkCache(),
    warmup: WarmupStats = new WarmupStats()
)

object SkiaTextCaches:

  private val Key = BackendCacheKey(classOf[SkiaTextCaches])

  def fresh(): SkiaTextCaches =
    val warmup = new WarmupStats()
    SkiaTextCaches(
      new SkiaFontCache(warmup = warmup),
      new SkiaShapedTextCache(warmup = warmup),
      new SkiaImageCache(),
      warmup = warmup
    )

  /** The set `owner` draws with, made the first time it is asked for. */
  def of(owner: BackendCaches): SkiaTextCaches = owner.getOrCreate(Key)(fresh())

  def heldBy(owner: BackendCaches): Boolean = owner.holds(Key)

  /** PROBE (#2465, throwaway): lets `into` draw with the set `from` holds. */
  def adopt(from: BackendCaches, into: BackendCaches): Unit =
    if heldBy(from) then { val _ = into.getOrCreate(Key)(of(from)) }

  /** What first use has cost `owner`'s Skia text caches so far, if it has any. */
  def warmupOf(owner: BackendCaches): Option[WarmupStats.Snapshot] =
    Option.when(heldBy(owner))(of(owner).warmup.snapshot)
