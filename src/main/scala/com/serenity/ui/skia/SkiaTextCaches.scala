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
):

  /** Shapes text for this owner's frames in the background; its workers are made by the first request. */
  lazy val ahead: ShapeAhead = new ShapeAhead(shapedText)

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
