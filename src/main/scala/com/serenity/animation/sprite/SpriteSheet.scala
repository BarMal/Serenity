package com.serenity.animation.sprite

import java.awt.image.BufferedImage

/** A rectangular region of a sprite sheet, in source pixels. */
final case class SpriteFrameRect(x: Int, y: Int, width: Int, height: Int)

/** One named animation clip: an ordered sequence of frame rects played in order. */
final case class SpriteClip(name: String, frames: Vector[SpriteFrameRect])

/** Declares where each clip's frames live on a sprite sheet. Pure data -- no image, no IO -- so a clip layout can be
  * described and tested independently of any actual sheet asset.
  */
final case class SpriteSheetLayout(clips: Map[String, SpriteClip]):
  def clip(name: String): Option[SpriteClip] = clips.get(name)

object SpriteSheetLayout:

  /** A single horizontal strip of `frameCount` equally sized, contiguous frames -- the shape `CompanionSpriteAssets`'s
    * generated placeholder sheet uses, and the simplest shape a hand-drawn replacement sheet can use too.
    */
  def horizontalStrip(clipName: String, frameWidth: Int, frameHeight: Int, frameCount: Int): SpriteSheetLayout =
    val frames = Vector.tabulate(frameCount)(i => SpriteFrameRect(i * frameWidth, 0, frameWidth, frameHeight))
    SpriteSheetLayout(Map(clipName -> SpriteClip(clipName, frames)))

/** Slices an already-decoded sprite sheet into per-frame images for a layout. Pure given a `BufferedImage` -- loading
  * the sheet from a classpath resource is `CompanionSpriteAssets`'s job, not this one's.
  */
object SpriteSheetSlicer:

  def frames(sheet: BufferedImage, clip: SpriteClip): Vector[BufferedImage] =
    clip.frames.map(rect => sheet.getSubimage(rect.x, rect.y, rect.width, rect.height))

  def slice(sheet: BufferedImage, layout: SpriteSheetLayout): Map[String, Vector[BufferedImage]] =
    layout.clips.view.mapValues(clip => frames(sheet, clip)).toMap
