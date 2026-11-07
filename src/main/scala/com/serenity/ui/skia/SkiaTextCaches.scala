package com.serenity.ui.skia

import com.serenity.ui.renderer.{BackendCacheKey, BackendCaches}
import org.jetbrains.skia.Typeface

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

  def fresh(resolveTypeface: SkiaTypefaces.TypefaceKey => Typeface = SkiaTypefaces.typefaceFor): SkiaTextCaches =
    val warmup = new WarmupStats()
    SkiaTextCaches(
      new SkiaFontCache(warmup = warmup, resolveTypeface = resolveTypeface),
      new SkiaShapedTextCache(warmup = warmup),
      new SkiaImageCache(),
      warmup = warmup
    )

  /** The set `owner` draws with, made the first time it is asked for; `resolveTypeface` only matters to that first ask. */
  def of(
      owner: BackendCaches,
      resolveTypeface: SkiaTypefaces.TypefaceKey => Typeface = SkiaTypefaces.typefaceFor
  ): SkiaTextCaches = owner.getOrCreate(Key)(fresh(resolveTypeface))

  def heldBy(owner: BackendCaches): Boolean = owner.holds(Key)

  /** Gives `into` what a new document leaves of `from`'s caches: the faces stay, since they depend on the fonts and not
    * on the text, and the shaped text, images and inks start empty. Nothing happens if `from` never drew with Skia.
    */
  def keepingFonts(from: BackendCaches, into: BackendCaches): Unit =
    if heldBy(from) then
      val warmup = new WarmupStats()
      val _ = into.getOrCreate(Key)(
        SkiaTextCaches(of(from).fonts, new SkiaShapedTextCache(warmup = warmup), new SkiaImageCache(), warmup = warmup)
      )

  /** What first use has cost `owner`'s Skia text caches so far, if it has any. */
  def warmupOf(owner: BackendCaches): Option[WarmupStats.Snapshot] =
    Option.when(heldBy(owner))(of(owner).warmup.snapshot)
